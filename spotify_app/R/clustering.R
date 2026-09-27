feature_cols <- c("popularity", "duration_min", "explicit", "release_year", "artist_popularity")
feature_labels <- c("Popularity", "Duration (min)", "Explicit", "Release year", "Artist popularity")

add_genre_columns <- function(tracks, n = 10) {
  all_genres <- unlist(tracks$genres)
  top_genres <- names(sort(table(all_genres), decreasing = TRUE))[seq_len(min(n, length(unique(all_genres))))]
  for (g in top_genres) {
    tracks[[paste0("genre_", gsub("[^a-zA-Z0-9]", "_", g))]] <- purrr::map_int(tracks$genres, \(gs) as.integer(g %in% gs))
  }
  tracks
}

# genre_weight scales the one-hot genre block so it doesn't dominate the distance
build_features <- function(tracks, genre_weight = 1) {
  tracks <- tracks |>
    dplyr::mutate(explicit = as.integer(explicit)) |>
    dplyr::distinct(track_id, .keep_all = TRUE) |>
    dplyr::filter(dplyr::if_all(dplyr::all_of(feature_cols), \(x) !is.na(x)))
  features <- tracks |>
    dplyr::select(dplyr::all_of(feature_cols), dplyr::starts_with("genre_")) |>
    dplyr::select(dplyr::where(\(x) stats::sd(x) > 0)) |>
    scale()
  is_genre <- startsWith(colnames(features), "genre_")
  features[, is_genre] <- features[, is_genre] * genre_weight
  list(tracks = tracks, features = features)
}

run_clustering <- function(features, algo = c("k-means", "HDBSCAN"), k = 3, min_pts = 5) {
  algo <- match.arg(algo)
  if (algo == "HDBSCAN") return(dbscan::hdbscan(features, minPts = min_pts)$cluster)
  spec <- tidyclust::k_means(num_clusters = k) |> parsnip::set_engine("stats", nstart = 25)
  set.seed(123)
  fit <- parsnip::fit(spec, ~ ., data = as.data.frame(features))
  as.integer(tidyclust::extract_cluster_assignment(fit)$.cluster)
}

cluster_label <- function(cluster) ifelse(cluster == 0, "Unclassified", paste("Cluster", cluster))
