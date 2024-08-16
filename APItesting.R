library(spotifyr)
library(tidyverse, warn.conflicts = F)
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
# Iterating through the get_my_playlists offset argument because of max limit = 50
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


## Getting tracks from two or more playlists to compare

# Create a function that iterates through the get_playlist_tracks offset argument because of max limit = 100
# and through a vector of playlists names

get_all_playlist_tracks <- function(playlists) {
  tracks_list <- list()
  
  for (playlist_name in playlists) {
    j <- 0
    rows <- 0
    all_tracks <- tibble()
    
    while (j <= rows) {
      tracks <- get_playlist_tracks(
        playlist_id = my_playlists[my_playlists$name == playlist_name, 1],
        limit = 100,
        offset = j
      )
      
      j <- j + 100
      all_tracks <- bind_rows(all_tracks, tracks)
      rows <- nrow(all_tracks)
    }
    
    tracks_list[[playlist_name]] <- all_tracks
  }
  
  return(tracks_list)
}

playlists_to_fetch <- c('Ruta Provincial', 'Slow Burn')
all_tracks <- get_all_playlist_tracks(playlists_to_fetch)

# Access the tracks for 'Ruta Provincial'
ruta_tracks <- all_tracks[['Ruta Provincial']]

# Access the tracks for 'Slow Burn'
slow_tracks <- all_tracks[['Slow Burn']]



# Getting features both from playlists and songs:

### SEGUIR CON: 
# https://www.rdocumentation.org/packages/spotifyr/versions/2.1.1/topics/get_playlist_audio_features
# https://www.rdocumentation.org/packages/spotifyr/versions/2.1.1/topics/get_track_audio_features
