backupUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("backup_panel"))
}

backupServer <- function(id, auth_status, access_token_rv, user_playlists) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$backup_panel <- renderUI({
      if (!auth_status()) {
        return(p("Please authorize the app in the Clustering tab first."))
      }
      fluidRow(
        column(5,
          h4("Export selected playlists"),
          selectizeInput(
            ns("backup_playlists"),
            "Select one or more playlists:",
            choices  = if (!is.null(user_playlists()))
              setNames(user_playlists()$id, user_playlists()$name)
            else NULL,
            multiple = TRUE,
            selected = NULL
          ),
          downloadButton(ns("backup_download"), "Export as CSV / ZIP"),
          hr(),
          h4("Full backup"),
          p("Download all your playlists as a ZIP, one CSV per playlist."),
          downloadButton(ns("backup_all"), "Download all as ZIP")
        )
      )
    })

    output$backup_download <- downloadHandler(
      filename = function() {
        req(input$backup_playlists)
        if (length(input$backup_playlists) == 1) {
          pname <- user_playlists() |> dplyr::filter(id == input$backup_playlists) |> dplyr::pull(name)
          paste0(gsub("[^a-zA-Z0-9]", "_", pname), ".csv")
        } else {
          paste0("playlists_backup_", format(Sys.Date(), "%Y%m%d"), ".zip")
        }
      },
      content = function(file) {
        req(input$backup_playlists)
        if (length(input$backup_playlists) == 1) {
          tracks <- fetch_backup_tracks(input$backup_playlists, access_token_rv())
          write.csv(tracks, file, row.names = FALSE)
        } else {
          tmp_dir <- file.path(tempdir(), paste0("backup_", as.integer(Sys.time())))
          dir.create(tmp_dir, showWarnings = FALSE)
          csv_files <- character(0)
          for (pid in input$backup_playlists) {
            pname    <- user_playlists() |> dplyr::filter(id == pid) |> dplyr::pull(name)
            tracks   <- fetch_backup_tracks(pid, access_token_rv())
            csv_path <- file.path(tmp_dir, paste0(gsub("[^a-zA-Z0-9]", "_", pname), ".csv"))
            write.csv(tracks, csv_path, row.names = FALSE)
            csv_files <- c(csv_files, csv_path)
          }
          zip::zip(file, files = csv_files, mode = "cherry-pick")
        }
      }
    )

    output$backup_all <- downloadHandler(
      filename = function() paste0("full_backup_", format(Sys.Date(), "%Y%m%d"), ".zip"),
      content = function(file) {
        req(user_playlists())
        all_ids <- user_playlists()$id
        tmp_dir <- file.path(tempdir(), paste0("backup_all_", as.integer(Sys.time())))
        dir.create(tmp_dir, showWarnings = FALSE)
        csv_files <- character(0)
        for (pid in all_ids) {
          pname  <- user_playlists() |> dplyr::filter(id == pid) |> dplyr::pull(name)
          tracks <- tryCatch(
            fetch_backup_tracks(pid, access_token_rv()),
            error = function(e) NULL
          )
          if (is.null(tracks) || nrow(tracks) == 0) next
          csv_path <- file.path(tmp_dir, paste0(gsub("[^a-zA-Z0-9]", "_", pname), ".csv"))
          write.csv(tracks, csv_path, row.names = FALSE)
          csv_files <- c(csv_files, csv_path)
        }
        zip::zip(file, files = csv_files, mode = "cherry-pick")
      }
    )
  })
}
