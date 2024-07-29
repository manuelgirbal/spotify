library(spotifyr)
library(tidyverse)
library(lubridate)

#### Configuration ####

source("credentials.R")

# Set Spotify Client ID and Secret
Sys.setenv(SPOTIFY_CLIENT_ID = client_id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = client_secret)

# Specify the required scope for the operations
scope <- "user-read-recently-played playlist-read-private playlist-read-collaborative"

# Obtain the authorization code with the specified scope
access_token <- get_spotify_authorization_code(scope = scope)


#### Testing analysis ####

# Get your most recently played tracks
get_my_recently_played(limit = 5, authorization = access_token) |> 
  mutate(artist.name = map_chr(track.artists, function(x) x$name[1]),
         played_at = as_datetime(played_at)) |> 
  select(track.name, artist.name, track.album.name, played_at)

# Playlists analysis
get_my_playlists(limit = 5,authorization = access_token)
