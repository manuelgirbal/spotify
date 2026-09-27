add_genre_columns <- function(tracks, n = 10) {
  all_genres <- unlist(tracks$genres)
  top_genres <- names(sort(table(all_genres), decreasing = TRUE))[seq_len(min(n, length(unique(all_genres))))]
  for (g in top_genres) {
    tracks[[paste0("genre_", gsub("[^a-zA-Z0-9]", "_", g))]] <- purrr::map_int(tracks$genres, \(gs) as.integer(g %in% gs))
  }
  tracks
}
