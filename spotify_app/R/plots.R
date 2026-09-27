cluster_palette <- c("#1DB954", "#FF6B6B", "#4ECDC4", "#FFE66D", "#A8E6CF", "#FF8B94", "#888888")

rescale_100 <- function(x) {
  r <- range(x, na.rm = TRUE)
  if (diff(r) == 0) rep(50, length(x)) else (x - r[1]) / diff(r) * 100
}

# Axes are cluster means rescaled to the dataset range (0% = min, 100% = max)
cluster_means <- function(tracks, values_to) {
  tracks |>
    dplyr::group_by(label = cluster_label(cluster)) |>
    dplyr::summarise(dplyr::across(dplyr::all_of(feature_cols), mean)) |>
    tidyr::pivot_longer(-label, names_to = "feature", values_to = values_to)
}

radar_plot <- function(tracks) {
  radar <- dplyr::left_join(
    cluster_means(tracks, "actual"),
    cluster_means(dplyr::mutate(tracks, dplyr::across(dplyr::all_of(feature_cols), rescale_100)), "r"),
    by = c("label", "feature")
  )
  # Repeat the first axis at the end so each polygon closes
  radar <- radar |>
    dplyr::arrange(label, factor(feature, levels = feature_cols)) |>
    dplyr::group_by(label) |>
    dplyr::group_modify(\(d, key) dplyr::bind_rows(d, d[1, ])) |>
    dplyr::ungroup() |>
    dplyr::mutate(feature = feature_labels[match(feature, feature_cols)])

  plotly::plot_ly(
    radar,
    type = "scatterpolar", mode = "lines+markers", fill = "toself",
    r = ~r, theta = ~feature, color = ~label,
    colors = cluster_palette[seq_along(unique(radar$label))],
    text = ~sprintf("%s: %s", feature, signif(actual, 3)), hoverinfo = "name+text"
  ) |>
    plotly::layout(
      polar = list(
        bgcolor     = "rgba(0,0,0,0)",
        radialaxis  = list(range = c(0, 100), ticksuffix = "%", gridcolor = "#444", linecolor = "#444"),
        angularaxis = list(gridcolor = "#444")
      ),
      paper_bgcolor = "rgba(0,0,0,0)",
      font          = list(color = "#ffffff")
    )
}
