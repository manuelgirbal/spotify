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
    uiOutput("auth_status"),
    uiOutput("dynamic_main_panel")
  )
)

server <- function(input, output, session) {
  
  # Reactive values
  auth_status <- reactiveVal(FALSE)
  user_playlists <- reactiveVal(NULL)
  cluster_results <- reactiveVal(NULL)
  cluster_playlist <- reactiveVal(NULL)
  clustering_performed <- reactiveVal(FALSE)

    # Credentials
  source("credentials.R")
  Sys.setenv(SPOTIFY_CLIENT_ID = client_id)
  Sys.setenv(SPOTIFY_CLIENT_SECRET = client_secret)

  # Authentication process
  observeEvent(input$auth_button, {
    tryCatch({
      scope <- "user-read-recently-played playlist-read-private playlist-read-collaborative"
      
      # Get the current URL from the session
      host_url <- session$clientData$url_hostname
      
      # Set the redirect URI based on whether it's running locally or on shinyapps.io
      redirect_uri <- if(grepl("shinyapps.io", host_url)) {
        "https://manuelgg.shinyapps.io/spotiApp/" 
      } else {
        "http://localhost:1410/"
      }
      
      # Set the redirect URI environment variable
      Sys.setenv(SPOTIFY_CLIENT_REDIRECT_URI = redirect_uri)
      
      access_token <- get_spotify_authorization_code(scope = scope)
      
      auth_status(TRUE)
      
      # Fetch playlists after successful authentication
      playlists <- fetch_user_playlists(access_token)
      user_playlists(playlists)
      
    }, error = function(e) {
      auth_status(FALSE)
      msg <- paste("Authentication failed:", e$message)
      message(msg)
      showNotification(msg, type = "error", duration = NULL)
    })
  })
  
  # Dynamic sidebar UI
  output$dynamic_sidebar <- renderUI({
    if (auth_status()) {
      sidebarPanel(
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
          "Select amount of new playlists:",
          choices = c(2:4),
          selected = NULL
        ),
        actionButton("perform_cluster", "Perform Cluster Analysis")  
        )
    # } 
    } else {
      sidebarPanel(
        actionButton("auth_button", "Authorize Spotify")
      )
    }
  })
  
  # Dynamic main panel UI
  output$dynamic_main_panel <- renderUI({
    if (auth_status()) {
      if (clustering_performed()) {  
        tagList(
          selectInput(
            "cluster_filter", 
            "Select new playlist to display:",
            choices = unique(cluster_playlist()$cluster),
            selected = 1,
            multiple = FALSE
          ),
          downloadButton("download_csv", "Download Playlist as CSV"),
          DTOutput("cluster_summary")
        )      } else {
        DTOutput("selected_playlists_output") 
      }
    }
  })
  
  # Display authentication status
  output$auth_status <- renderUI({
    if (auth_status()) {
      "Authentication successful! You can now use the app."
    } else {
      HTML("With this app, you can select two of your Spotify playlists that you feel are a bit mixed up with each other and use them to create between 2 and 4 new playlists. 
                <hr style='border-color: black; border-width: 1px;'>
      The app will perform a cluster analysis (k-means) using the musical characteristics of both playlists as input (danceability, energy, loudness, speechiness, acousticness, instrumentalness, liveness, valence, mode, key, track popularity, and duration).
                <hr style='border-color: black; border-width: 1px;'>
      Please click the 'Authorize Spotify' button to begin.")
    }
  })
  
  
  # Display the selected playlists
  
  output$selected_playlists_output <- renderDT({

    req(playlistaudiofeatures_react())

    selected_columns <- playlistaudiofeatures_react() |> 
      select(playlist_name,
             track.name,
             artist.name)
    
    datatable(selected_columns,
              rownames = F)

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
    req(input$selected_playlists) 

    playlists_filtered <- user_playlists() |> filter(name %in% input$selected_playlists)
    


    playlistaudiofeatures <-  get_playlist_audio_features(playlist_uris = playlists_filtered$id)
    

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
    
    clustering_performed(TRUE) 
    
    
  })


  output$cluster_summary <- renderDT({
    
    req(cluster_playlist())
    
    selected_columns2 <- cluster_playlist() |>
        filter(cluster == input$cluster_filter) |> 
        transmute(new_playlist = cluster,
                  track.name,
                  artist.name) |> 
        distinct()

    datatable(selected_columns2,
              rownames = F)
    
  })
  
  
  # Download handler for CSV
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("new_playlist_cluster_", input$cluster_filter, ".csv", sep = "")  # Define the filename
    },
    content = function(file) {
      req(cluster_playlist())  # Ensure the cluster data is available
      
      # Filter the data according to the selected cluster
      filtered_data <- cluster_playlist() |>
        filter(cluster == input$cluster_filter) |>
        transmute(new_playlist = cluster,
                  track.name,
                  artist.name) |>
        distinct()
      
      write.csv(filtered_data, file, row.names = FALSE)  # Write data to CSV file without row names
    }
  ) 
}
    
    
# Run the app
shinyApp(ui = ui, server = server)
