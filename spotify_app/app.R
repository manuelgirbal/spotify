library(shiny)
library(spotifyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(DT)
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
        selectInput(
          "selected_playlists",
          "Select Playlists:",
          choices = if (!is.null(user_playlists())) unique(user_playlists()$name) else NULL,
          multiple = TRUE,
          selected = NULL
        )
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

      datatable(user_playlists() |> 
                  filter(name %in% input$selected_playlists)
                )  

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
}  
 


# Run the app
shinyApp(ui = ui, server = server)
