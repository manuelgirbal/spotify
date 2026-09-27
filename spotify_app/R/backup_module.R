backup_tracks <- function(playlist_id) {
  fetch_tracks(playlist_id) |>
    dplyr::select(track_name, artist_name, album_name, duration_min, explicit, release_year, added_at)
}

write_playlists_zip <- function(playlists, file) {
  dir   <- tempfile("backup_")
  dir.create(dir)
  paths <- file.path(dir, paste0(safe_filename(playlists$name), ".csv"))
  withProgress(message = "Exporting playlists", value = 0, {
    for (i in seq_along(paths)) {
      incProgress(1 / length(paths), detail = playlists$name[i])
      write.csv(backup_tracks(playlists$id[i]), paths[i], row.names = FALSE)
    }
  })
  zip::zip(file, files = paths, mode = "cherry-pick")
}

backupUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("notice")),
    layout_columns(
      card(
        card_header("Export selected playlists"),
        selectizeInput(ns("playlists"), "Playlists", choices = NULL, multiple = TRUE, width = "100%"),
        p(class = "text-muted", "One playlist downloads as CSV, several as a ZIP."),
        downloadButton(ns("download"), "Export")
      ),
      card(
        card_header("Full backup"),
        p("Download all your playlists as a ZIP, one CSV per playlist."),
        downloadButton(ns("download_all"), "Download all as ZIP")
      )
    )
  )
}

backupServer <- function(id, user_id, user_playlists) {
  moduleServer(id, function(input, output, session) {
    observe(updateSelectizeInput(session, "playlists", choices = playlist_choices(user_playlists())))

    output$notice <- renderUI(if (is.null(user_id())) login_notice())

    selected <- reactive({
      req(input$playlists)
      dplyr::filter(user_playlists(), id %in% input$playlists)
    })

    output$download <- downloadHandler(
      filename = function() {
        if (nrow(selected()) == 1) paste0(safe_filename(selected()$name), ".csv")
        else paste0("playlists_backup_", format(Sys.Date(), "%Y%m%d"), ".zip")
      },
      content = function(file) {
        if (nrow(selected()) == 1) write.csv(backup_tracks(selected()$id), file, row.names = FALSE)
        else write_playlists_zip(selected(), file)
      }
    )

    output$download_all <- downloadHandler(
      filename = function() paste0("full_backup_", format(Sys.Date(), "%Y%m%d"), ".zip"),
      content  = function(file) {
        req(user_playlists())
        write_playlists_zip(user_playlists(), file)
      }
    )
  })
}
