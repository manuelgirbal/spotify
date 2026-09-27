# Every export also stores a local snapshot, so playlist history builds up over time
backup_tracks <- function(playlist) {
  tracks <- fetch_tracks(playlist$id)
  save_snapshot(tracks, playlist$id, playlist$name, playlist[["snapshot_id"]] %||% NA_character_)
  dplyr::select(tracks, track_name, artist_name, album_name, duration_min, explicit, release_year, added_at)
}

write_playlists_zip <- function(playlists, file) {
  dir   <- tempfile("backup_")
  dir.create(dir)
  paths <- file.path(dir, paste0(safe_filename(playlists$name), ".csv"))
  withProgress(message = "Exporting playlists", value = 0, {
    for (i in seq_along(paths)) {
      incProgress(1 / length(paths), detail = playlists$name[i])
      write.csv(backup_tracks(playlists[i, ]), paths[i], row.names = FALSE)
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
    ),
    card(
      fill = FALSE,
      card_header("Playlist history"),
      p(class = "text-muted", "Each export and each confirmed change in Organize saves a snapshot. Compare two to see what was added or removed."),
      layout_columns(
        selectInput(ns("history_playlist"), "Playlist", choices = NULL, width = "100%"),
        selectInput(ns("from"), "From snapshot", choices = NULL, width = "100%"),
        selectInput(ns("to"), "To snapshot", choices = NULL, width = "100%")
      ),
      uiOutput(ns("history_status")),
      DTOutput(ns("history"), fill = FALSE)
    )
  )
}

backupServer <- function(id, user_id, user_playlists, db_version) {
  moduleServer(id, function(input, output, session) {
    observe({
      choices <- playlist_choices(user_playlists())
      updateSelectizeInput(session, "playlists", choices = choices)
      updateSelectInput(session, "history_playlist", choices = choices, selected = keep_selected(isolate(input$history_playlist), choices))
    })

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
        if (nrow(selected()) == 1) write.csv(backup_tracks(selected()), file, row.names = FALSE)
        else write_playlists_zip(selected(), file)
        db_version(db_version() + 1)
      }
    )

    output$download_all <- downloadHandler(
      filename = function() paste0("full_backup_", format(Sys.Date(), "%Y%m%d"), ".zip"),
      content  = function(file) {
        req(user_playlists())
        write_playlists_zip(user_playlists(), file)
        db_version(db_version() + 1)
      }
    )

    snapshot_times <- reactive({
      req(input$history_playlist)
      db_version()
      with_notify(load_snapshot_times(input$history_playlist), "Error reading history")
    })

    observe({
      times   <- snapshot_times()
      req(times)
      choices <- stats::setNames(
        sprintf("%.0f", times$ts_ms),
        sprintf("%s (%d tracks)", format(times$ts, "%Y-%m-%d %H:%M", tz = Sys.timezone()), times$tracks)
      )
      updateSelectInput(session, "from", choices = choices, selected = choices[min(2, length(choices))])
      updateSelectInput(session, "to", choices = choices, selected = choices[1])
    })

    output$history_status <- renderUI({
      n <- nrow(snapshot_times())
      if (n == 0) p(class = "text-muted", "No snapshots yet for this playlist. Export it to save the first one.")
      else if (n == 1) p(class = "text-muted", "Only one snapshot so far. Export again later to compare.")
    })

    output$history <- renderDT({
      req(nrow(snapshot_times()) > 1, input$from, input$to)
      compare_snapshots(input$history_playlist, input$from, input$to) |>
        datatable(rownames = FALSE, colnames = c("Change", "Track", "Artist"), class = "compact hover",
                  options = list(pageLength = 10, dom = "ftip"))
    })
  })
}
