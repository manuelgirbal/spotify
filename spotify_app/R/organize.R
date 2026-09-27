apply_ops <- function(original, ops) {
  current <- original
  for (op in ops) {
    moved <- current[[op$from]] |>
      dplyr::filter(track_id %in% op$track_ids) |>
      dplyr::distinct(track_id, .keep_all = TRUE) |>
      dplyr::filter(!track_id %in% current[[op$to]]$track_id)
    current[[op$to]] <- dplyr::bind_rows(current[[op$to]], moved)
    if (op$mode == "Move") current[[op$from]] <- dplyr::filter(current[[op$from]], !track_id %in% op$track_ids)
  }
  current
}

pending_diff <- function(original, current) {
  diff <- purrr::imap(current, \(tracks, pid) list(
    add    = setdiff(tracks$track_id, original[[pid]]$track_id),
    remove = setdiff(original[[pid]]$track_id, tracks$track_id)
  ))
  purrr::keep(diff, \(d) length(d$add) + length(d$remove) > 0)
}

# Adds go first so a failure halfway leaves a duplicate, never a lost track
commit_changes <- function(diff, original) {
  for (pid in names(diff)) if (length(diff[[pid]]$add)) add_tracks(pid, diff[[pid]]$add)
  for (pid in names(diff)) if (length(diff[[pid]]$remove)) remove_tracks(pid, diff[[pid]]$remove)
  purrr::imap(diff, \(d, pid) list(
    added   = d$add,
    removed = original[[pid]] |> dplyr::filter(track_id %in% d$remove) |> dplyr::select(track_id, position)
  ))
}

# Re-inserting in ascending original position, one run of consecutive positions per request,
# restores the original order
revert_changes <- function(record) {
  for (pid in record$deleted) follow_playlist(pid)
  for (pid in names(record$changes)) {
    change <- record$changes[[pid]]
    if (length(change$added)) remove_tracks(pid, change$added)
    removed <- dplyr::arrange(change$removed, position)
    if (nrow(removed) == 0) next
    runs    <- split(removed, cumsum(c(1, diff(removed$position) != 1)))
    for (run in runs) add_tracks(pid, run$track_id, position = run$position[1])
  }
}
