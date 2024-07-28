library(spotifyr)
library(tidyverse)
library(lubridate)

source("credentials.R")

Sys.setenv(SPOTIFY_CLIENT_ID = client_id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = client_secret)
scope <- "user-read-recently-played"

get_spotify_authorization_code(client_id,
                               client_secret,
                               scope)


# Get your most recently played tracks

get_my_recently_played(limit = 20)

library(kableExtra)
get_my_recently_played(limit = 5) %>% 
  mutate(artist.name = map_chr(track.artists, function(x) x$name[1]),
         played_at = as_datetime(played_at)) %>% 
  select(track.name, artist.name, track.album.name, played_at) %>% 
  kable()

