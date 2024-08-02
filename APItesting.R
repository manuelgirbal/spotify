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

## Playlists analysis
i <- 0
rows <- 0
my_playlists <- tibble()

while (i <= rows) {
  
  playlists <- get_my_playlists(authorization = access_token,
                                   limit = 50,
                                   offset = i)
  
  i <<- i+50
  my_playlists <<- bind_rows(my_playlists, playlists)
  rows <<- nrow(my_playlists)
  }


# Temporary fix to get only owned playlists
most_freq_user <- my_playlists |> 
  count(owner.display_name) |> 
  arrange(desc(n)) |> 
  slice(1) |> 
  transmute(user = owner.display_name)

my_playlists <- my_playlists |> 
  filter(owner.display_name == most_freq_user[[1]]) |> 
  select(id, name)


# Getting tracks from two playlists to compare

ruta <- my_playlists |> 
  filter(name == 'Ruta Provincial') |> 
  select(1)

ruta_tracks <- get_playlist_tracks(ruta) 

pampeano <- my_playlists |> 
  filter(name == 'Pampeano - Stoner') |> 
  select(1)

pampeano_tracks <- get_playlist_tracks(pampeano) 


# Getting features both from playlists and songs:

### SEGUIR CON: 
# https://www.rdocumentation.org/packages/spotifyr/versions/2.1.1/topics/get_playlist_audio_features
# https://www.rdocumentation.org/packages/spotifyr/versions/2.1.1/topics/get_track_audio_features
