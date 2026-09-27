spotify_req <- function(path, method = "GET", body = NULL, ...) {
  url <- if (startsWith(path, "https://")) path else paste0("https://api.spotify.com/v1/", path)
  req <- httr2::request(url) |>
    httr2::req_method(method) |>
    httr2::req_url_query(...) |>
    req_spotify_auth() |>
    httr2::req_retry(max_tries = 5)
  if (!is.null(body)) req <- httr2::req_body_json(req, body)
  resp <- httr2::req_perform(req)
  if (!httr2::resp_has_body(resp)) return(invisible(NULL))
  httr2::resp_body_json(resp)
}

spotify_paginate <- function(path, ...) {
  page  <- spotify_req(path, ...)
  items <- page$items
  while (!is.null(page[["next"]])) {
    page  <- spotify_req(page[["next"]])
    items <- c(items, page$items)
  }
  items
}

chunk <- function(x, size) split(x, ceiling(seq_along(x) / size))

fetch_me <- function() spotify_req("me")

fetch_user_playlists <- function(user_id) {
  playlists <- spotify_paginate("me/playlists", limit = 50) |>
    purrr::keep(\(p) !is.null(p) && identical(p$owner$id, user_id))
  dplyr::tibble(
    id          = purrr::map_chr(playlists, "id"),
    name        = purrr::map_chr(playlists, "name"),
    total       = purrr::map_int(playlists, \(p) p$tracks$total %||% NA_integer_),
    snapshot_id = purrr::map_chr(playlists, "snapshot_id")
  )
}

fetch_playlist_snapshot <- function(playlist_id) {
  spotify_req(paste0("playlists/", playlist_id), fields = "snapshot_id")$snapshot_id
}

fetch_tracks <- function(playlist_id) {
  items <- spotify_paginate(paste0("playlists/", playlist_id, "/tracks"), limit = 100)
  # Position is taken before dropping local files/episodes so it matches Spotify's index
  position <- seq_along(items) - 1L
  keep     <- purrr::map_lgl(items, \(it) is.character(it$track$id) && identical(it$track$type, "track"))
  items    <- items[keep]
  t        <- purrr::map(items, "track")
  dplyr::tibble(
    position     = position[keep],
    track_id     = purrr::map_chr(t, "id"),
    track_name   = purrr::map_chr(t, "name"),
    artist_id    = purrr::map_chr(t, \(x) x$artists[[1]]$id %||% NA_character_),
    artist_name  = purrr::map_chr(t, \(x) x$artists[[1]]$name %||% NA_character_),
    album_name   = purrr::map_chr(t, \(x) x$album$name %||% NA_character_),
    release_year = as.integer(substr(purrr::map_chr(t, \(x) x$album$release_date %||% NA_character_), 1, 4)),
    duration_min = round(purrr::map_dbl(t, "duration_ms") / 60000, 2),
    explicit     = purrr::map_lgl(t, \(x) isTRUE(x$explicit)),
    popularity   = purrr::map_int(t, \(x) x$popularity %||% NA_integer_),
    added_at     = as.Date(substr(purrr::map_chr(items, \(it) it$added_at %||% NA_character_), 1, 10))
  )
}

fetch_artists <- function(artist_ids) {
  ids <- unique(stats::na.omit(artist_ids))
  # /v1/artists takes at most 50 ids per request
  artists <- purrr::map(chunk(ids, 50), \(batch) spotify_req("artists", ids = paste(batch, collapse = ","))$artists) |>
    purrr::list_flatten() |>
    purrr::compact()
  dplyr::tibble(
    artist_id         = purrr::map_chr(artists, "id"),
    artist_popularity = purrr::map_int(artists, \(a) a$popularity %||% NA_integer_),
    genres            = purrr::map(artists, \(a) as.character(unlist(a$genres)))
  )
}

fetch_tracks_with_artists <- function(playlists) {
  tracks <- purrr::map2(playlists$id, playlists$name, \(id, name) {
    fetch_tracks(id) |> dplyr::mutate(playlist_id = id, playlist_name = name, .before = 1)
  }) |>
    purrr::list_rbind()
  dplyr::left_join(tracks, fetch_artists(tracks$artist_id), by = "artist_id")
}

track_uris <- function(track_ids) purrr::map(paste0("spotify:track:", track_ids), \(u) list(uri = u))

add_tracks <- function(playlist_id, track_ids, position = NULL) {
  snapshot <- NULL
  for (batch in chunk(track_ids, 100)) {
    body <- list(uris = as.list(paste0("spotify:track:", batch)))
    if (!is.null(position)) {
      body$position <- position
      position <- position + length(batch)
    }
    snapshot <- spotify_req(paste0("playlists/", playlist_id, "/tracks"), "POST", body)$snapshot_id
  }
  snapshot
}

# Spotify removes every occurrence of each track in the playlist
remove_tracks <- function(playlist_id, track_ids) {
  snapshot <- NULL
  for (batch in chunk(unique(track_ids), 100)) {
    body <- list(tracks = track_uris(batch))
    snapshot <- spotify_req(paste0("playlists/", playlist_id, "/tracks"), "DELETE", body)$snapshot_id
  }
  snapshot
}

create_playlist <- function(user_id, name, track_ids) {
  playlist <- spotify_req(paste0("users/", user_id, "/playlists"), "POST", list(name = name, public = FALSE))
  add_tracks(playlist$id, track_ids)
  playlist$id
}

# Spotify has no playlist delete: the owner unfollowing it removes it from the library
unfollow_playlist <- function(playlist_id) spotify_req(paste0("playlists/", playlist_id, "/followers"), "DELETE")
follow_playlist   <- function(playlist_id) spotify_req(paste0("playlists/", playlist_id, "/followers"), "PUT")
