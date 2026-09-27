library(shiny)
library(dplyr)
library(DT)
library(purrr)
library(shinythemes)
library(plotly)
library(dbscan)
library(zip)
library(tidyclust)

ui <- navbarPage(
  "SpotiApp",
  theme = shinytheme("cyborg"),
  tabPanel("Clustering", clusteringUI("clustering")),
  tabPanel("Backup",     backupUI("backup"))
)

server <- function(input, output, session) {
  user_playlists  <- reactiveVal(NULL)
  user_id_rv      <- reactiveVal(NULL)

  clusteringServer("clustering", user_id_rv, user_playlists)
  backupServer("backup", user_id_rv, user_playlists)
}

options(shiny.port = 3838)
shinyApp(ui = ui, server = server)
