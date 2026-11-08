library(shiny)
library(bslib)
library(DT)
library(plotly)

ui <- page_navbar(
  title = "SpotiApp",
  fillable = FALSE,
  theme = bs_theme(version = 5, preset = "cyborg"),
  nav_panel("Clustering", clusteringUI("clustering")),
  nav_panel("Build", buildUI("build")),
  nav_panel("Organize", organizeUI("organize")),
  nav_panel("Backup", backupUI("backup")),
  nav_spacer(),
  nav_item(uiOutput("login_ui"))
)

server <- function(input, output, session) {
  user_id        <- reactiveVal(NULL)
  user_playlists <- reactiveVal(NULL)
  db_version     <- reactiveVal(0)

  output$login_ui <- renderUI({
    if (is.null(user_id())) actionButton("login", "Log in with Spotify", class = "btn-success btn-sm")
    else span(class = "navbar-text", "Connected to Spotify")
  })

  observeEvent(input$login, {
    withProgress(message = "Connecting to Spotify", with_notify({
      me <- fetch_me()
      user_playlists(fetch_user_playlists(me$id))
      user_id(me$id)
    }, "Login failed"))
  })

  clusteringServer("clustering", user_id, user_playlists)
  buildServer("build", user_id, user_playlists)
  organizeServer("organize", user_id, user_playlists, db_version)
  backupServer("backup", user_id, user_playlists, db_version)
}

options(shiny.port = 3838)
shinyApp(ui = ui, server = server)
