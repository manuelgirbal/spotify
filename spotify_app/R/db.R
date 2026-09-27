db_path <- function() Sys.getenv("SPOTIAPP_DB", "spotiapp.duckdb")

# Append-only: tables and columns are only ever added, never dropped or updated
schema <- c(
  "CREATE TABLE IF NOT EXISTS playlist_snapshots (
     ts TIMESTAMP NOT NULL, playlist_id VARCHAR NOT NULL, playlist_name VARCHAR, snapshot_id VARCHAR,
     position INTEGER NOT NULL, track_id VARCHAR NOT NULL, track_name VARCHAR, artist_name VARCHAR,
     album_name VARCHAR, added_at DATE,
     PRIMARY KEY (ts, playlist_id, position))",
  "CREATE TABLE IF NOT EXISTS commits (
     commit_id VARCHAR NOT NULL, ts TIMESTAMP NOT NULL, playlist_id VARCHAR NOT NULL,
     playlist_name VARCHAR, snapshot_after VARCHAR,
     PRIMARY KEY (commit_id, playlist_id))",
  "CREATE TABLE IF NOT EXISTS changes (
     commit_id VARCHAR NOT NULL, playlist_id VARCHAR NOT NULL,
     action VARCHAR NOT NULL, track_id VARCHAR, position INTEGER)",
  "CREATE TABLE IF NOT EXISTS reverts (commit_id VARCHAR PRIMARY KEY, ts TIMESTAMP NOT NULL)"
)

with_db <- function(f) {
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), db_path())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  for (stmt in schema) DBI::dbExecute(con, stmt)
  f(con)
}

db_append <- function(con, table, rows) if (nrow(rows) > 0) DBI::dbAppendTable(con, table, rows)

# Millisecond precision so timestamps survive the R <-> DuckDB round trip and work as keys
now_ms <- function() as.POSIXct(round(as.numeric(Sys.time()), 3), origin = "1970-01-01", tz = "UTC")

save_snapshot <- function(tracks, playlist_id, playlist_name, snapshot_id = NA_character_) {
  rows <- tracks |>
    dplyr::transmute(
      ts = now_ms(), playlist_id = playlist_id, playlist_name = playlist_name, snapshot_id = snapshot_id,
      position, track_id, track_name, artist_name, album_name, added_at
    )
  with_db(\(con) db_append(con, "playlist_snapshots", rows))
}

log_commit <- function(changes, names, snapshots) {
  commit_id <- format(Sys.time(), "%Y%m%d-%H%M%OS3")
  commits <- dplyr::tibble(
    commit_id, ts = now_ms(), playlist_id = names(changes),
    playlist_name = unname(names[names(changes)]), snapshot_after = unname(unlist(snapshots[names(changes)]))
  )
  rows <- purrr::imap(changes, \(ch, pid) dplyr::bind_rows(
    dplyr::tibble(action = "add", track_id = ch$added, position = NA_integer_),
    dplyr::tibble(action = "remove", track_id = ch$removed$track_id, position = as.integer(ch$removed$position))
  ) |> dplyr::mutate(commit_id = commit_id, playlist_id = pid, .before = 1)) |>
    purrr::list_rbind()
  with_db(\(con) {
    db_append(con, "commits", commits)
    db_append(con, "changes", rows)
  })
  commit_id
}

log_deleted <- function(commit_id, playlist_ids) {
  rows <- dplyr::tibble(commit_id, playlist_id = playlist_ids, action = "delete", track_id = NA_character_, position = NA_integer_)
  with_db(\(con) db_append(con, "changes", rows))
}

log_revert <- function(commit_id) {
  with_db(\(con) db_append(con, "reverts", dplyr::tibble(commit_id, ts = now_ms())))
}

load_commits <- function() {
  with_db(\(con) DBI::dbGetQuery(con, "
    SELECT c.commit_id, min(c.ts) AS ts,
           string_agg(c.playlist_name || ' (+' || coalesce(n.added, 0) || ' / -' || coalesce(n.removed, 0) || ')', ', ') AS summary,
           bool_or(r.commit_id IS NOT NULL) AS reverted
    FROM commits c
    LEFT JOIN (
      SELECT commit_id, playlist_id,
             count(*) FILTER (WHERE action = 'add') AS added,
             count(*) FILTER (WHERE action = 'remove') AS removed
      FROM changes GROUP BY ALL
    ) n USING (commit_id, playlist_id)
    LEFT JOIN reverts r USING (commit_id)
    GROUP BY c.commit_id
    ORDER BY ts DESC")) |>
    dplyr::as_tibble()
}

load_commit <- function(commit_id) {
  with_db(\(con) {
    commits <- DBI::dbGetQuery(con, "SELECT * FROM commits WHERE commit_id = ?", list(commit_id))
    changes <- DBI::dbGetQuery(con, "SELECT * FROM changes WHERE commit_id = ?", list(commit_id))
    list(
      changes = purrr::map(stats::setNames(commits$playlist_id, commits$playlist_id), \(pid) list(
        added   = changes$track_id[changes$playlist_id == pid & changes$action == "add"],
        removed = dplyr::as_tibble(changes[changes$playlist_id == pid & changes$action == "remove", c("track_id", "position")])
      )),
      deleted   = changes$playlist_id[changes$action == "delete"],
      snapshots = as.list(stats::setNames(commits$snapshot_after, commits$playlist_id)),
      names     = stats::setNames(commits$playlist_name, commits$playlist_id)
    )
  })
}

# Snapshots are identified by epoch milliseconds, computed on the DuckDB side both ways
load_snapshot_times <- function(playlist_id) {
  with_db(\(con) DBI::dbGetQuery(con, "
    SELECT epoch_ms(ts) AS ts_ms, ts, count(*) AS tracks FROM playlist_snapshots
    WHERE playlist_id = ? GROUP BY ts ORDER BY ts DESC", list(playlist_id))) |>
    dplyr::as_tibble()
}

compare_snapshots <- function(playlist_id, from_ms, to_ms) {
  with_db(\(con) DBI::dbGetQuery(con, "
    WITH a AS (SELECT DISTINCT track_id, track_name, artist_name FROM playlist_snapshots
               WHERE playlist_id = ? AND epoch_ms(ts) = ?),
         b AS (SELECT DISTINCT track_id, track_name, artist_name FROM playlist_snapshots
               WHERE playlist_id = ? AND epoch_ms(ts) = ?)
    SELECT 'Removed' AS change, track_name, artist_name FROM a WHERE track_id NOT IN (SELECT track_id FROM b)
    UNION ALL
    SELECT 'Added', track_name, artist_name FROM b WHERE track_id NOT IN (SELECT track_id FROM a)
    ORDER BY change DESC, artist_name, track_name",
    list(playlist_id, as.numeric(from_ms), playlist_id, as.numeric(to_ms)))) |>
    dplyr::as_tibble()
}
