library(shiny)
library(spotifyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(DT)
library(purrr)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("cyborg"),
  titlePanel("SpotiApp"),
  
  uiOutput("dynamic_sidebar"),
  
  mainPanel(
    textOutput("auth_status"),
    uiOutput("dynamic_main_panel")
  )
)

server <- function(input, output, session) {
  
  # Reactive values
  auth_status <- reactiveVal(FALSE)
  user_playlists <- reactiveVal(NULL)
  cluster_results <- reactiveVal(NULL)
  cluster_playlist <- reactiveVal(NULL)

  # Authentication process
  observeEvent(input$auth_button, {
    tryCatch({
      source("credentials.R")
      Sys.setenv(SPOTIFY_CLIENT_ID = client_id)
      Sys.setenv(SPOTIFY_CLIENT_SECRET = client_secret)
      scope <- "user-read-recently-played playlist-read-private playlist-read-collaborative"
      access_token <- get_spotify_authorization_code(scope = scope)
      auth_status(TRUE)
      
      # Fetch playlists after successful authentication
      playlists <- fetch_user_playlists(access_token)
      user_playlists(playlists)
      
    }, error = function(e) {
      auth_status(FALSE)
      showNotification("Authentication failed. Please try again.", type = "error")
    })
  })
  
  # Dynamic sidebar UI
  output$dynamic_sidebar <- renderUI({
    if (auth_status()) {
      sidebarPanel(
        # textInput("user_name", "User Name", "Please insert your Spotify User Name"),
        selectizeInput(
          "selected_playlists",
          "Select up to 2 playlists:",
          choices = if (!is.null(user_playlists())) unique(user_playlists()$name) else NULL,
          multiple = TRUE,
          selected = NULL,
          options = list(
            maxItems = 2
          )
        ),
        selectInput(
          "selected_clusters",
          "Select up to 4 new playlists:",
          choices = c(1:4),
          selected = NULL
        ),
        actionButton("perform_cluster", "Perform Cluster Analysis")  
        )
    } else {
      sidebarPanel(
        actionButton("auth_button", "Authorize Spotify")
      )
    }
  })
  
  # Dynamic main panel UI
  output$dynamic_main_panel <- renderUI({
    if (auth_status()) {
      tagList(
        # verbatimTextOutput("cluster_summary"),
        DTOutput("cluster_summary"),
        DTOutput("selected_playlists_output")
      )
    }
  })
  
  # Display authentication status
  output$auth_status <- renderText({
    if (auth_status()) {
      "Authentication successful! You can now use the app."
    } else {
      "Please click the 'Authorize Spotify' button to begin."
    }
  })
  
  
  # Display the selected playlists
  output$selected_playlists_output <- renderDT({
      datatable(
        playlistaudiofeatures_react()
      )
  })
  

  
  # Playlist Fetching
  fetch_user_playlists <- function(access_token) {
    i <- 0
    rows <- 0
    my_playlists <- tibble()
    
    while (i <= rows) {
      playlists <- get_my_playlists(authorization = access_token,
                                    limit = 50,
                                    offset = i)
      i <- i + 50
      my_playlists <- bind_rows(my_playlists, playlists)
      rows <- nrow(my_playlists)
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

  }

  

    # Getting music features:
    playlistaudiofeatures_react <- reactive({
    
    req(user_playlists())
    req(input$selected_playlists) ## VER si funciona o debe ser reactive
    # req(input$user_name) ## VER si funciona o debe ser reactive
      
    playlists_filtered <- user_playlists() |> filter(name %in% input$selected_playlists)
    
    # playlistaudiofeatures <-  get_playlist_audio_features(input$user_name,
    #                                                       playlists_filtered$id)

    playlistaudiofeatures <-  get_playlist_audio_features(playlist_uris = playlists_filtered$id)
    
    # playlistaudiofeatures <- playlistaudiofeatures |> unnest_wider(track.artists)


    playlistaudiofeatures <- playlistaudiofeatures |>
      transmute(playlist_name,
                track.id,
                track.name,
                # artist = name,
                artist.name = map_chr(track.artists, function(x) x$name[1]),
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
                duration = round(track.duration_ms/60000,2)) |>
      distinct(.keep_all = T)
  })

  
  clusters <- NA  
    
  observeEvent(input$perform_cluster, {
    req(playlistaudiofeatures_react(), input$selected_clusters)
    
    playlist_data <- playlistaudiofeatures_react()
    
    features <- playlist_data %>%
      select(danceability,
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
             duration) %>%
      scale()
    
    set.seed(123)
    clusters <<- kmeans(features, centers = as.numeric(input$selected_clusters), nstart = 25)
    
    # cluster_results(clusters)
    
    playlist_data$cluster <- clusters$cluster
    
    cluster_playlist(playlist_data)
    
    
  })


  # output$cluster_summary <- renderPrint({
  #   req(cluster_results())
  #   summary(cluster_results())
  # })

    
  
  
  output$cluster_summary <- renderDT({
    req(cluster_playlist())
    datatable(cluster_playlist())
    
  })
  
}
    
    
# Run the app
shinyApp(ui = ui, server = server)
