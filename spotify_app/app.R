#### Setup ####

library(shiny)
library(spotifyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(shinythemes)

options(scipen = 999)  # Large value to avoid scientific notation

#### UI ####
ui <- fluidPage(
  theme = shinytheme("cyborg"),
  titlePanel("SpotiApp"),
  
  sidebarLayout(
    sidebarPanel(
      # h3("Authorization"),
      actionButton("auth_button", "Authorize Spotify"),
      br(), br(),
      textInput("user_name",
                "User Name",
                "Please insert your Spotify User Name"),
      selectInput(
        "selected_playlists",
        "Select Playlists:",
        choices = unique(my_playlists$name),
        multiple = TRUE,
        selected = NULL
      ),
      selectInput(
        "selected_clusters",
        "Select Amount of New Playlists:",
        choices = c(1:10),
        selected = NULL
      )
    ),
    
    mainPanel(
      textOutput("auth_status")
      # Placeholder for future results
    )
  )
)



#### Server ####
server <- function(input, output, session) {
  
  # Reactive value to store authentication status
  auth_status <- reactiveVal(FALSE)
  
  # Authentication process
  observeEvent(input$auth_button, {
    tryCatch({
      # Source credentials file
      source("credentials.R")
      
      # Set Spotify Client ID and Secret
      Sys.setenv(SPOTIFY_CLIENT_ID = client_id)
      Sys.setenv(SPOTIFY_CLIENT_SECRET = client_secret)
      
      # Specify the required scope for the operations
      scope <- "user-read-recently-played playlist-read-private playlist-read-collaborative"
      
      # Obtain the authorization code with the specified scope
      access_token <- get_spotify_authorization_code(scope = scope)
      
      # If we reach this point without error, authentication was successful
      auth_status(TRUE)
    }, error = function(e) {
      # If an error occurs, authentication failed
      auth_status(FALSE)
      showNotification("Authentication failed. Please try again.", type = "error")
    })
  })
  
  # Display authentication status
  output$auth_status <- renderText({
    if (auth_status()) {
      "Authentication successful! You can now use the app."
    } else {
      "Please click the 'Authorize Spotify' button to begin."
    }
  })
  
  
  ##### User Playlists #####
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
  
  
  
  # Getting music features:
  
  playlistaudiofeatures <-  get_playlist_audio_features(user_name,
                                                        my_playlists$id[my_playlists$name %in%  selected_playlists]
                                                        )
  
  playlistaudiofeatures <- playlistaudiofeatures |> unnest_wider(track.artists)
  
  
  ##### Cluster analysis #####
  
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

  
  # standardizing data
  features <- scale(playlistaudiofeatures_a |> select(!c(playlist_name, track.id, track.name, artist)))
  
  # k-means clustering
  set.seed(123)  # for reproducibility
  k <- selected_clusters
  clusters <- kmeans(features, centers = k, nstart = 25)
  playlistaudiofeatures_a$cluster <- clusters$cluster
  
  # Splitting both playlists into k new playlists
  
  for (i in 1:k) {
    playlist <- playlistaudiofeatures_a |> 
      filter(cluster == i) |> 
      select(track.id, track.name, artist) |> 
      distinct()
    
    assign(paste0("new_playlist_", i), playlist, envir = .GlobalEnv)
  }
  

}

# Run the app
shinyApp(ui = ui, server = server)