library(shiny)
library(spotifyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
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
        textInput("user_name", "User Name", "Please insert your Spotify User Name"),
        selectInput(
          "selected_playlists",
          "Select Playlists:",
          choices = if (!is.null(user_playlists())) unique(user_playlists()$name) else NULL,
          multiple = TRUE,
          selected = NULL
        ),
        selectInput(
          "selected_clusters",
          "Select Amount of New Playlists:",
          choices = c(1:10),
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
        # plotOutput("playlist_analysis_plot"), ## CAMBIAR
        verbatimTextOutput("cluster_summary")  ## CAMBIAR
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
  
  # 2. Playlist Fetching (refine if necessary)
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

    my_playlists %>%
      select(id, name)
  }

  
  
  # # 3. Track Fetching
  # get_all_playlist_tracks <- function(playlist_ids) {
  #   all_tracks <- tibble()
  #   
  #   for (pid in playlist_ids) {
  #     j <- 0
  #     rows <- 0
  #     playlist_tracks <- tibble()
  #     
  #     while (j <= rows) {
  #       tracks <- get_playlist_tracks(
  #         playlist_id = pid,
  #         authorization = access_token,
  #         limit = 100,
  #         offset = j
  #       )
  #       
  #       j <- j + 100
  #       playlist_tracks <- bind_rows(playlist_tracks, tracks)
  #       rows <- nrow(playlist_tracks)
  #     }
  #     
  #     all_tracks <- bind_rows(all_tracks, playlist_tracks)
  #   }
  #   
  #   return(all_tracks)
  # }
  
  
  # 4. Audio Features}
  
  playlist_audio_features <- reactive({

    req(input$user_name, input$selected_playlists)

    playlist_ids <- user_playlists() |>
                      filter(name %in% input$selected_playlists) |>
                      pull(id)


    audio_features <- get_playlist_audio_features(
      input$user_name,
      playlist_ids#,
      # authorization = access_token
    )

    audio_features %>%
      select(id, danceability, energy, loudness, speechiness, acousticness,
             instrumentalness, liveness, valence, tempo)
  })
  
  
  # 5. Clustering Analysis
  observeEvent(input$perform_cluster, {
    req(playlist_audio_features(), input$selected_clusters)
    
    features <- playlist_audio_features() %>%
      select(-id) %>%
      scale()
    
    set.seed(123)
    clusters <- kmeans(features, centers = as.numeric(input$selected_clusters), nstart = 25)
    
    cluster_results(clusters)
  })
  
  # # 6. Visualization
  # output$playlist_analysis_plot <- renderPlot({
  #   req(cluster_results(), playlist_audio_features())
  #   
  #   clusters <- cluster_results()
  #   data <- playlist_audio_features() %>%
  #     mutate(cluster = as.factor(clusters$cluster))
  #   
  #   ggplot(data, aes(x = energy, y = valence, color = cluster)) +
  #     geom_point(alpha = 0.7) +
  #     theme_minimal() +
  #     labs(title = "Clustering of Tracks by Energy and Valence",
  #          x = "Energy", y = "Valence")
  # })
  
  # New output for cluster summary
  output$cluster_summary <- renderPrint({
    req(cluster_results())
    summary(cluster_results())
  })
}

# Run the app
shinyApp(ui = ui, server = server)
