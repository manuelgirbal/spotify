spotify_get <- function(url, token, params = list(), max_retries = 5) {
  for (attempt in seq_len(max_retries)) {
    res    <- httr::GET(url, httr::config(token = token), query = params)
    status <- httr::status_code(res)
    # Token2.0 is R6 — refresh mutates in-place, no need to reassign
    if (status == 401 && attempt == 1) { token$refresh(); next }
    if (status == 429) {
      Sys.sleep(max(as.numeric(httr::headers(res)[["retry-after"]]), 1))
      next
    }
    if (status %in% c(403, 404) || attempt == max_retries) httr::stop_for_status(res)
    if (status == 200) break
  }
  jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"), flatten = TRUE)
}

fetch_user_playlists <- function(access_token) {
  i            <- 0
  my_playlists <- dplyr::tibble()
  repeat {
    batch        <- spotifyr::get_my_playlists(authorization = access_token, limit = 50, offset = i)
    my_playlists <- dplyr::bind_rows(my_playlists, batch)
    if (nrow(batch) < 50) break
    i <- i + 50
  }
  user_id <- spotifyr::get_my_profile(authorization = access_token)$id
  list(
    playlists = my_playlists |> dplyr::filter(owner.id == user_id) |> dplyr::select(id, name),
    user_id   = user_id
  )
}

fetch_playlist_audio_features <- function(playlist_ids, token) {
  all_tracks <- purrr::map_df(playlist_ids, function(pid) {
    meta  <- spotify_get(paste0("https://api.spotify.com/v1/playlists/", pid), token)
    total <- meta$tracks$total
    if (total == 0) return(dplyr::tibble())

    purrr::map_df(seq(0, total - 1, by = 100), function(offset) {
      res <- spotify_get(
        paste0("https://api.spotify.com/v1/playlists/", pid, "/tracks"),
        token,
        params = list(limit = 100, offset = offset)
      )
      res$items |>
        dplyr::filter(!is.na(track.id)) |>
        dplyr::mutate(playlist_name = meta$name)
    })
  })

  if (nrow(all_tracks) == 0) return(all_tracks)

  artist_ids <- purrr::map_chr(all_tracks$track.artists, function(x) {
    if (is.null(x) || nrow(x) == 0) NA_character_ else x$id[1]
  })

  unique_artist_ids <- unique(stats::na.omit(artist_ids))

  # /v1/artists takes at most 50 ids per request
  artist_data <- purrr::map_df(
    split(unique_artist_ids, ceiling(seq_along(unique_artist_ids) / 50)),
    function(ids) {
      res <- spotify_get(
        "https://api.spotify.com/v1/artists",
        token,
        params = list(ids = paste(ids, collapse = ","))
      )
      dplyr::tibble(
        artist_id         = res$artists$id,
        artist_popularity = res$artists$popularity,
        genres            = res$artists$genres
      )
    }
  )

  all_genres <- purrr::flatten_chr(artist_data$genres)
  top_genres  <- names(sort(table(all_genres), decreasing = TRUE))[seq_len(min(10, length(unique(all_genres))))]

  all_tracks <- all_tracks |>
    dplyr::mutate(primary_artist_id = artist_ids) |>
    dplyr::left_join(artist_data, by = c("primary_artist_id" = "artist_id"))

  for (g in top_genres) {
    col_name <- paste0("genre_", gsub("[^a-zA-Z0-9]", "_", g))
    all_tracks[[col_name]] <- purrr::map_int(all_tracks$genres, function(gs) {
      if (is.null(gs)) 0L else as.integer(g %in% gs)
    })
  }

  all_tracks
}

fetch_backup_tracks <- function(playlist_id, token) {
  meta  <- spotify_get(paste0("https://api.spotify.com/v1/playlists/", playlist_id), token)
  total <- meta$tracks$total
  if (total == 0) return(dplyr::tibble())

  purrr::map_df(seq(0, total - 1, by = 100), function(offset) {
    res <- spotify_get(
      paste0("https://api.spotify.com/v1/playlists/", playlist_id, "/tracks"),
      token,
      params = list(limit = 100, offset = offset)
    )
    res$items |>
      dplyr::filter(!is.na(track.id)) |>
      dplyr::transmute(
        track_name   = track.name,
        artist_name  = purrr::map_chr(track.artists, function(x) x$name[1]),
        album_name   = track.album.name,
        duration_min = round(track.duration_ms / 60000, 2),
        explicit     = track.explicit,
        release_year = as.integer(substr(track.album.release_date, 1, 4))
      )
  })
}

create_spotify_playlist <- function(user_id, playlist_name, track_ids, token) {
  res <- httr::POST(
    paste0("https://api.spotify.com/v1/users/", user_id, "/playlists"),
    httr::config(token = token),
    httr::content_type_json(),
    body = jsonlite::toJSON(list(name = playlist_name, public = FALSE), auto_unbox = TRUE)
  )
  httr::stop_for_status(res)
  playlist_id <- jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"))$id

  batches <- split(track_ids, ceiling(seq_along(track_ids) / 100))
  for (batch in batches) {
    uris <- paste0("spotify:track:", batch)
    res2 <- httr::POST(
      paste0("https://api.spotify.com/v1/playlists/", playlist_id, "/tracks"),
      httr::config(token = token),
      httr::content_type_json(),
      body = jsonlite::toJSON(list(uris = uris), auto_unbox = TRUE)
    )
    httr::stop_for_status(res2)
  }

  playlist_id
}
