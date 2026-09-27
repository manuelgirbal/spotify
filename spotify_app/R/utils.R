with_notify <- function(expr, msg) {
  tryCatch(expr, error = function(e) {
    showNotification(paste0(msg, ": ", conditionMessage(e)), type = "error", duration = NULL)
    NULL
  })
}

playlist_choices <- function(playlists) {
  if (is.null(playlists)) return(character())
  stats::setNames(playlists$id, playlists$name)
}

safe_filename <- function(x) make.unique(gsub("[^a-zA-Z0-9]", "_", x), sep = "_")

login_notice <- function() {
  p(class = "text-muted", "Log in with Spotify (top right) to load your playlists.")
}
