library(shiny)
library(httr)
library(spotifyr)
library(dplyr)
library(DT)
library(purrr)
library(shinythemes)
library(jsonlite)
library(ggplot2)
library(plotly)
library(tidyr)
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
  auth_status     <- reactiveVal(FALSE)
  access_token_rv <- reactiveVal(NULL)
  user_playlists  <- reactiveVal(NULL)
  user_id_rv      <- reactiveVal(NULL)

  clusteringServer("clustering", auth_status, access_token_rv, user_playlists, user_id_rv)
  backupServer("backup", auth_status, access_token_rv, user_playlists)
}

options(shiny.port = 3838)
shinyApp(ui = ui, server = server)
