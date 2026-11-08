# One row per track. added_at is the first time it was added to any of the playlists
dedupe_tracks <- function(tracks) {
  tracks |>
    dplyr::arrange(added_at) |>
    dplyr::mutate(playlists = paste(unique(playlist_name), collapse = ", "), .by = track_id) |>
    dplyr::distinct(track_id, .keep_all = TRUE) |>
    dplyr::select(-playlist_id, -playlist_name, -position)
}

# Missing values pass range filters, so tracks without that data aren't silently dropped
in_range <- function(x, range) is.na(x) | (x >= range[1] & x <= range[2])

filter_tracks <- function(tracks, added = NULL, years = NULL, popularity = NULL,
                          genres = NULL, artists = NULL, exclude_explicit = FALSE) {
  if (!is.null(added))      tracks <- dplyr::filter(tracks, in_range(added_at, added))
  if (!is.null(years))      tracks <- dplyr::filter(tracks, in_range(release_year, years))
  if (!is.null(popularity)) tracks <- dplyr::filter(tracks, in_range(popularity, .env$popularity))
  if (length(genres) > 0)   tracks <- dplyr::filter(tracks, purrr::map_lgl(genres, \(g) any(g %in% .env$genres)))
  if (length(artists) > 0)  tracks <- dplyr::filter(tracks, artist_name %in% artists)
  if (exclude_explicit)     tracks <- dplyr::filter(tracks, !explicit)
  dplyr::arrange(tracks, dplyr::desc(added_at))
}

genre_choices <- function(tracks) names(sort(table(unlist(tracks$genres)), decreasing = TRUE))
