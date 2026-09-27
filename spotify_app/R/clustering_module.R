clusteringUI <- function(id) {
  ns <- NS(id)
  algo_help <- tagList(
    strong("k-means"), ": you choose how many playlists to create. Every track is assigned to one.",
    br(), br(),
    strong("HDBSCAN"), ": finds the number of groups by itself. Tracks that don't fit are ",
    em("Unclassified"), ". Higher minimum group size gives fewer, bigger groups."
  )
  layout_sidebar(
    sidebar = sidebar(
      width = 300,
      selectizeInput(ns("playlists"), "Playlists (up to 2)", choices = NULL, multiple = TRUE,
                     options = list(maxItems = 2)),
      input_task_button(ns("load"), "Load tracks"),
      hr(),
      radioButtons(ns("algo"), span("Algorithm ", popover(icon("circle-info"), algo_help)),
                   choices = c("k-means", "HDBSCAN"), inline = TRUE),
      conditionalPanel("input.algo == 'k-means'", ns = ns,
        sliderInput(ns("k"), "Number of playlists", min = 2, max = 4, value = 3, step = 1)),
      conditionalPanel("input.algo == 'HDBSCAN'", ns = ns,
        sliderInput(ns("min_pts"), "Minimum group size", min = 5, max = 30, value = 15)),
      sliderInput(ns("genre_weight"), "Genre weight", min = 0, max = 1, value = 1, step = 0.25),
      input_task_button(ns("run"), "Run clustering")
    ),
    uiOutput(ns("notice")),
    navset_card_tab(
      nav_panel("Tracks", DTOutput(ns("tracks"))),
      nav_panel("Characteristics",
        p(class = "text-muted", "Axes are cluster means: 0% = dataset min, 100% = max. Hover for actual values."),
        plotlyOutput(ns("radar")))
    ),
    card(
      card_header("Save a cluster"),
      layout_columns(
        selectInput(ns("cluster"), "Cluster", choices = NULL),
        textInput(ns("new_name"), "New playlist name", placeholder = "E.g.: My rock 2000s playlist"),
        div(
          tags$label(class = "form-label", HTML("&nbsp;")),
          div(class = "d-flex gap-2",
            actionButton(ns("create"), "Create in Spotify", class = "btn-success"),
            downloadButton(ns("download_csv"), "CSV")))
      )
    )
  )
}

clusteringServer <- function(id, user_id, user_playlists) {
  moduleServer(id, function(input, output, session) {
    tracks <- reactiveVal(NULL)
    result <- reactiveVal(NULL)

    observe(updateSelectizeInput(session, "playlists", choices = playlist_choices(user_playlists())))

    output$notice <- renderUI({
      if (is.null(user_id())) login_notice()
      else if (is.null(tracks())) p(class = "text-muted", "Pick up to 2 playlists and load their tracks.")
    })

    observeEvent(input$load, {
      req(input$playlists)
      selected <- dplyr::filter(user_playlists(), id %in% input$playlists)
      withProgress(message = "Loading tracks and artists from Spotify", {
        tracks(with_notify(fetch_tracks_with_artists(selected) |> add_genre_columns(), "Error fetching tracks"))
      })
      result(NULL)
    })

    observeEvent(input$run, {
      req(tracks())
      built <- build_features(tracks(), input$genre_weight)
      if (nrow(built$tracks) < 10) {
        showNotification("Not enough tracks for clustering. Select playlists with more tracks.", type = "warning")
        return()
      }
      clusters <- with_notify(
        run_clustering(built$features, input$algo, k = input$k, min_pts = input$min_pts),
        "Clustering failed"
      )
      req(clusters)
      n_real <- length(unique(clusters[clusters > 0]))
      if (n_real > 6) {
        showNotification(paste0("HDBSCAN found ", n_real, " groups. Try a higher minimum group size."), type = "warning")
        return()
      }
      result(dplyr::mutate(built$tracks, cluster = clusters))
      ids <- sort(unique(clusters))
      updateSelectInput(session, "cluster", choices = stats::setNames(ids, cluster_label(ids)))
    })

    cluster_tracks <- reactive({
      req(result(), input$cluster)
      dplyr::filter(result(), cluster == as.integer(input$cluster))
    })

    output$tracks <- renderDT({
      shown <- if (is.null(result())) tracks() else cluster_tracks()
      req(shown)
      shown |>
        dplyr::select(playlist_name, track_name, artist_name, release_year, popularity) |>
        datatable(rownames = FALSE, colnames = c("Playlist", "Track", "Artist", "Year", "Popularity"))
    })

    output$radar <- renderPlotly({
      req(result())
      radar_plot(result())
    })

    output$download_csv <- downloadHandler(
      filename = function() paste0("cluster_", input$cluster, ".csv"),
      content  = function(file) {
        cluster_tracks() |>
          dplyr::select(track_name, artist_name, album_name, release_year) |>
          write.csv(file, row.names = FALSE)
      }
    )

    observeEvent(input$create, {
      req(cluster_tracks())
      name <- trimws(input$new_name)
      if (!nzchar(name)) {
        showNotification("Please enter a name for the playlist.", type = "warning")
        return()
      }
      created <- with_notify(create_playlist(user_id(), name, cluster_tracks()$track_id), "Error creating playlist")
      req(created)
      user_playlists(fetch_user_playlists(user_id()))
      showNotification(paste0("Playlist '", name, "' created in Spotify."), type = "message")
    })
  })
}
