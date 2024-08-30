library(spotifyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

options(scipen = 999)  # Large value to avoid scientific notation


#### Configuration ####

source("credentials.R")

# Set Spotify Client ID and Secret
Sys.setenv(SPOTIFY_CLIENT_ID = client_id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = client_secret)

# Specify the required scope for the operations
scope <- "user-read-recently-played playlist-read-private playlist-read-collaborative"

# Obtain the authorization code with the specified scope
access_token <- get_spotify_authorization_code(scope = scope)


#### Data gathering ####

## Iterating through the get_my_playlists offset argument because of max limit = 50

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



# Getting music features:

playlistaudiofeatures <-  get_playlist_audio_features(manuelgg, # More complete than get_track_audio_features
                                                      c("4OH3OMSL2I9fm6iSwr1KMl","1IpBInrdGIu43h73vb9tCf"))

playlistaudiofeatures <- playlistaudiofeatures |> unnest_wider(track.artists)

#### Exploratory analysis ####

playlistaudiofeatures_a <- playlistaudiofeatures |> 
  transmute(playlist_name,
         track.id,
         track.name,
         artist = name,
         danceability,
         energy,
         loudness,
         speechiness,
         acousticness,
         instrumentalness,
         liveness,
         valence,
         mode,
         key,
         track.popularity,
         duration = track.duration_ms/60000) |> 
  distinct(.keep_all = T) 

# playlistaudiofeatures_b <-  playlistaudiofeatures_a |> 
#   pivot_longer(!c(playlist_name, track.id),
#                names_to = "feature_name", 
#                values_to = "feature_value")
# 
# 
# ggplot(playlistaudiofeatures_b, aes(x = feature_value)) +
#   geom_histogram(binwidth = 0.2, fill = "blue", color = "black") +  
#   facet_grid(playlist_name ~ feature_name,
#              scales="free") +  
#   theme_minimal() +
#   # xlim(-10, 10) +
#   # ylim(0, 50) +
#   labs(title = "Distribution of Musical Features by Playlist",
#        x = "Feature Value", y = "Count")
# 
# 
# ggplot(playlistaudiofeatures_b, aes(x = feature_value)) +
#   geom_histogram(binwidth = 0.2, fill = "blue", color = "black") +  
#   facet_wrap(~playlist_name + feature_name,
#              scales = "free") +
#   theme_minimal() +
#   labs(title = "Distribution of Musical Features by Playlist",
#        x = "Feature Value", y = "Count")


#### k-means cluster analysis ####

# standardizing data
features <- scale(playlistaudiofeatures_a |> select(!c(playlist_name, track.id, track.name, artist)))

# clustering
set.seed(123)  # for reproducibility
k <- 4  # how many playlists we'd like to get out of these two. Alternative here: https://www.r-bloggers.com/2017/02/finding-optimal-number-of-clusters/
clusters <- kmeans(features, centers = k, nstart = 25)
playlistaudiofeatures_a$cluster <- clusters$cluster

# # Comparing clusters to playlists
# table(playlistaudiofeatures_a$playlist_name, playlistaudiofeatures_a$cluster)

# Splitting both playlists into k new playlists

for (i in 1:k) {
  playlist <- playlistaudiofeatures_a |> 
    filter(cluster == i) |> 
    select(track.id, track.name, artist) |> 
    distinct()
  
  assign(paste0("new_playlist_", i), playlist, envir = .GlobalEnv)
  }


#### Pending ####
# Shiny app:
  # https://mastering-shiny.org/reactive-motivation.html
# Add main characteristic description for each kluster-playlist:
  # https://r-graph-gallery.com/spider-or-radar-chart.html
  # https://r-graph-gallery.com/lollipop-plot.html 
# Trying DBSCAN instead of k-means (https://cran.r-project.org/web/packages/dbscan/vignettes/hdbscan.html)
# Offer how to modify/create playlists based on results

