spotify_scopes <- paste(
  "playlist-read-private",
  "playlist-read-collaborative",
  "playlist-modify-private",
  "playlist-modify-public"
)

spotify_client <- function() {
  secret <- Sys.getenv("SPOTIFY_CLIENT_SECRET")
  httr2::oauth_client(
    id        = Sys.getenv("SPOTIFY_CLIENT_ID"),
    token_url = "https://accounts.spotify.com/api/token",
    secret    = if (nzchar(secret)) secret else NULL,
    name      = "spotiapp"
  )
}

# Token is cached encrypted on disk; httr2 refreshes it and re-runs the
# browser flow on its own if the refresh token was revoked.
req_spotify_auth <- function(req) {
  httr2::req_oauth_auth_code(
    req,
    client       = spotify_client(),
    auth_url     = "https://accounts.spotify.com/authorize",
    scope        = spotify_scopes,
    redirect_uri = Sys.getenv("SPOTIFY_CLIENT_REDIRECT_URI", "http://127.0.0.1:1410/"),
    cache_disk   = TRUE
  )
}
