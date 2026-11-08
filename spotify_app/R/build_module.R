buildUI <- function(id) {
  ns <- NS(id)
  this_year <- as.integer(format(Sys.Date(), "%Y"))
  layout_sidebar(
    sidebar = sidebar(
      width = 320,
      selectizeInput(ns("playlists"), "Source playlists", choices = NULL, multiple = TRUE),
      input_task_button(ns("load"), "Load tracks"),
      hr(),
      dateRangeInput(ns("added"), "Added to my playlists", start = Sys.Date() - 365, end = Sys.Date()),
      sliderInput(ns("years"), "Release year", min = 1950, max = this_year, value = c(1950, this_year), sep = ""),
      sliderInput(ns("popularity"), "Track popularity", min = 0, max = 100, value = c(0, 100)),
      selectizeInput(ns("genres"), "Genres (any of)", choices = NULL, multiple = TRUE),
      selectizeInput(ns("artists"), "Artists", choices = NULL, multiple = TRUE),
      checkboxInput(ns("exclude_explicit"), "Exclude explicit tracks")
    ),
    uiOutput(ns("notice")),
    card(
      fill = FALSE,
      card_header(textOutput(ns("count"))),
      DTOutput(ns("tracks"), fill = FALSE),
      div(
        class = "d-flex gap-2",
        actionButton(ns("exclude"), "Exclude selected"),
        actionButton(ns("reset"), "Bring back excluded")
      )
    ),
    card(
      fill = FALSE,
      card_header("Save as a new playlist"),
      p(class = "text-muted", "Tracks are saved newest added first. Tracks with no data for a filter are kept."),
      layout_columns(
        textInput(ns("new_name"), "New playlist name", placeholder = "E.g.: Added this year"),
        div(
          tags$label(class = "form-label", HTML("&nbsp;")),
          div(class = "d-flex gap-2",
            actionButton(ns("create"), "Create in Spotify", class = "btn-success"),
            downloadButton(ns("download_csv"), "CSV")))
      )
    )
  )
}

buildServer <- function(id, user_id, user_playlists) {
  moduleServer(id, function(input, output, session) {
    tracks   <- reactiveVal(NULL)
    excluded <- reactiveVal(character())

    observe(updateSelectizeInput(session, "playlists", choices = playlist_choices(user_playlists()),
                                 selected = isolate(input$playlists)))

    output$notice <- renderUI({
      if (is.null(user_id())) login_notice()
      else if (is.null(tracks())) p(class = "text-muted", "Pick one or more playlists and load their tracks.")
    })

    observeEvent(input$load, {
      req(input$playlists)
      selected <- dplyr::filter(user_playlists(), id %in% input$playlists)
      loaded <- withProgress(message = "Loading tracks and artists from Spotify",
        with_notify(dedupe_tracks(fetch_tracks_with_artists(selected)), "Error fetching tracks"))
      req(loaded)
      tracks(loaded)
      excluded(character())

      years <- range(loaded$release_year, na.rm = TRUE)
      if (all(is.finite(years))) updateSliderInput(session, "years", min = years[1], max = years[2], value = years)
      dates <- range(loaded$added_at, na.rm = TRUE)
      if (all(is.finite(dates))) {
        updateDateRangeInput(session, "added", min = dates[1], max = Sys.Date(),
                             start = max(dates[1], Sys.Date() - 365), end = Sys.Date())
      }
      updateSelectizeInput(session, "genres", choices = genre_choices(loaded), server = TRUE)
      updateSelectizeInput(session, "artists", choices = sort(unique(stats::na.omit(loaded$artist_name))), server = TRUE)
    })

    filtered <- reactive({
      req(tracks())
      tracks() |>
        filter_tracks(
          added = input$added, years = input$years, popularity = input$popularity,
          genres = input$genres, artists = input$artists, exclude_explicit = input$exclude_explicit
        ) |>
        dplyr::filter(!track_id %in% excluded())
    })

    output$count <- renderText({
      if (is.null(tracks())) "Tracks"
      else paste0(nrow(filtered()), " of ", nrow(tracks()), " tracks match")
    })

    output$tracks <- renderDT({
      filtered() |>
        dplyr::transmute(
          Track = track_name, Artist = artist_name, Year = release_year, Popularity = popularity,
          Genres = purrr::map_chr(genres, \(g) paste(utils::head(g, 3), collapse = ", ")),
          Added = added_at, Playlists = playlists
        ) |>
        datatable(rownames = FALSE, class = "compact nowrap hover",
                  options = list(pageLength = 15, dom = "ftip", scrollX = TRUE))
    })

    observeEvent(input$exclude, {
      rows <- input$tracks_rows_selected
      if (length(rows) == 0) {
        showNotification("Select tracks in the table first.", type = "warning")
        return()
      }
      excluded(c(excluded(), filtered()$track_id[rows]))
    })

    observeEvent(input$reset, excluded(character()))

    output$download_csv <- downloadHandler(
      filename = function() {
        name <- trimws(input$new_name)
        paste0(safe_filename(if (nzchar(name)) name else "playlist"), ".csv")
      },
      content  = function(file) {
        filtered() |>
          dplyr::select(track_name, artist_name, album_name, release_year, added_at) |>
          write.csv(file, row.names = FALSE)
      }
    )

    observeEvent(input$create, {
      req(tracks())
      name <- trimws(input$new_name)
      if (!nzchar(name)) {
        showNotification("Please enter a name for the playlist.", type = "warning")
        return()
      }
      if (nrow(filtered()) == 0) {
        showNotification("No tracks match the filters.", type = "warning")
        return()
      }
      created <- with_notify(create_playlist(user_id(), name, filtered()$track_id), "Error creating playlist")
      req(created)
      user_playlists(fetch_user_playlists(user_id()))
      showNotification(paste0("Playlist '", name, "' created in Spotify with ", nrow(filtered()), " tracks."), type = "message")
    })
  })
}
