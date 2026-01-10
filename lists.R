## Playlists: 
# Once you receive your data from Spotify (see README.md), unzip and save "MyData/" folder in this project's root directory:

library(jsonlite)
library(tidyverse)

# Read your playlists .json file:
lists <- fromJSON("MyData/Playlist1.json", flatten = TRUE)

# Create a vector with the names from each playlist:
names <- lists$playlists$name

# Assign each name to each playlist object:
for (i in 1:length(names)){
  assign(x = names[i], value = lists$playlists$items[[i]])
}

# Save each list in a folder (in this case, "lists/") as .csv files:
for(i in 1:length(names)) {                              
  write_csv(lists$playlists$items[[i]],            
             paste0("lists/", names[i], ".csv"))
}
