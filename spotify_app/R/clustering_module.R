clusteringUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    uiOutput(ns("dynamic_sidebar")),
    mainPanel(
      uiOutput(ns("auth_status")),
      uiOutput(ns("dynamic_main_panel"))
    )
  )
}

clusteringServer <- function(id, auth_status, access_token_rv, user_playlists, user_id_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    cluster_results      <- reactiveVal(NULL)
    cluster_playlist     <- reactiveVal(NULL)
    clustering_performed <- reactiveVal(FALSE)

    observeEvent(input$auth_button, {
      tryCatch({
        scope <- paste(
          "user-read-recently-played",
          "playlist-read-private",
          "playlist-read-collaborative",
          "playlist-modify-private"
        )
        endpoint <- httr::oauth_endpoint(
          authorize = "https://accounts.spotify.com/authorize",
          access    = "https://accounts.spotify.com/api/token"
        )
        app <- httr::oauth_app(
          appname      = "spotifyr",
          key          = Sys.getenv("SPOTIFY_CLIENT_ID"),
          secret       = Sys.getenv("SPOTIFY_CLIENT_SECRET"),
          redirect_uri = Sys.getenv("SPOTIFY_CLIENT_REDIRECT_URI")
        )
        access_token <- httr::oauth2.0_token(endpoint, app, scope = scope)
        access_token_rv(access_token)
        auth_status(TRUE)

        result <- fetch_user_playlists(access_token)
        user_playlists(result$playlists)
        user_id_rv(result$user_id)
      }, error = function(e) {
        auth_status(FALSE)
        showNotification(paste("Authentication failed:", e$message), type = "error", duration = NULL)
      })
    })

    output$dynamic_sidebar <- renderUI({
      if (auth_status()) {
        sidebarPanel(
          selectizeInput(
            ns("selected_playlists"),
            "Select up to 2 playlists:",
            choices  = if (!is.null(user_playlists())) unique(user_playlists()$name) else NULL,
            multiple = TRUE,
            selected = NULL,
            options  = list(maxItems = 2)
          ),
          radioButtons(ns("algo"), "Algorithm:", choices = c("k-means", "DBSCAN"), inline = TRUE),
          actionLink(ns("algo_info"), "Which one? \u2139\ufe0f"),
          br(), br(),
          conditionalPanel(
            condition = "input.algo == 'k-means'",
            ns = ns,
            selectInput(ns("selected_clusters"), "Number of playlists:", choices = 2:4)
          ),
          conditionalPanel(
            condition = "input.algo == 'DBSCAN'",
            ns = ns,
            sliderInput(ns("eps"),    "eps",    min = 0.1, max = 2.0, value = 0.5, step = 0.1),
            sliderInput(ns("minPts"), "minPts", min = 2,   max = 10,  value = 3)
          ),
          actionButton(ns("perform_cluster"), "Perform Cluster Analysis")
        )
      } else {
        sidebarPanel(
          actionButton(ns("auth_button"), "Authorize Spotify")
        )
      }
    })

    observeEvent(input$algo_info, {
      showModal(modalDialog(
        title = "K-means or DBSCAN?",
        HTML("
          <table style='width:100%; border-collapse:collapse;'>
            <tr>
              <th style='padding:8px; text-align:left; border-bottom:2px solid #555;'>K-means</th>
              <th style='padding:8px; text-align:left; border-bottom:2px solid #555;'>DBSCAN</th>
            </tr>
            <tr>
              <td style='padding:8px; vertical-align:top;'>
                \u2705 Fast and reproducible<br>
                \u2705 You choose how many playlists to create<br>
                \u274c Assumes clusters of similar size<br>
                \u274c Every track is assigned to a group<br><br>
                <em>\U0001f3b5 Best when you already know how many playlists you want</em>
              </td>
              <td style='padding:8px; vertical-align:top;'>
                \u2705 No need to define the number of clusters upfront<br>
                \u2705 Tracks that don't fit are labeled <strong>Unclassified</strong><br>
                \u274c Can give 0 or 20 clusters depending on eps/minPts<br>
                \u274c Capped at 6 clusters — adjust parameters if it exceeds that<br><br>
                <em>\U0001f3b5 Best for exploring natural groupings when you're not sure how many exist</em>
              </td>
            </tr>
          </table>
        "),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })

    output$auth_status <- renderUI({
      if (auth_status()) {
        "Authentication successful! You can now use the app."
      } else {
        HTML("With this app, you can select two of your Spotify playlists that you feel are a bit mixed up with each other and use them to create between 2 and 4 new playlists.
          <hr style='border-color: black; border-width: 1px;'>
        The app will perform a cluster analysis (k-means or DBSCAN) using track and artist characteristics: popularity, duration, explicitness, release year, artist popularity, and genre tags.
          <hr style='border-color: black; border-width: 1px;'>
        Please click the 'Authorize Spotify' button to begin.")
      }
    })

    output$dynamic_main_panel <- renderUI({
      if (auth_status()) {
        if (clustering_performed()) {
          clusters_available <- sort(unique(cluster_playlist()$cluster))
          tabs <- lapply(clusters_available, function(cl) {
            label <- if (cl == 0) "Unclassified" else paste("Playlist", cl)
            tabPanel(label, value = as.character(cl))
          })
          tagList(
            do.call(tabsetPanel, c(list(id = ns("cluster_filter")), tabs)),
            br(),
            downloadButton(ns("download_csv"), "Download as CSV"),
            br(), br(),
            textInput(ns("new_playlist_name"), "Save to Spotify as:",
                      placeholder = "E.g.: My rock 2000s playlist"),
            actionButton(ns("create_playlist_btn"), "Create in Spotify \u25b6"),
            br(), br(),
            tabsetPanel(
              tabPanel("Tracks",
                br(),
                DTOutput(ns("cluster_summary"))
              ),
              tabPanel("Characteristics",
                br(),
                helpText(
                  "The radar shows all clusters at once.",
                  "Each axis is a normalized feature (0% = dataset min, 100% = max).",
                  "Hover over the points to see the actual values."
                ),
                plotlyOutput(ns("cluster_features_plot"))
              )
            )
          )
        } else {
          req(playlistaudiofeatures_react())
          pnames <- unique(playlistaudiofeatures_react()$playlist_name)
          tabs <- lapply(seq_along(pnames), function(i) {
            tabPanel(pnames[i], br(), DTOutput(ns(paste0("playlist_dt_", i))))
          })
          do.call(tabsetPanel, tabs)
        }
      }
    })

    playlistaudiofeatures_react <- reactive({
      req(user_playlists())
      req(input$selected_playlists)
      playlists_filtered <- user_playlists() |> dplyr::filter(name %in% input$selected_playlists)
      playlistaudiofeatures <- tryCatch(
        fetch_playlist_audio_features(playlists_filtered$id, access_token_rv()),
        error = function(e) {
          showNotification(paste("Error fetching tracks:", e$message), type = "error", duration = NULL)
          NULL
        }
      )
      req(playlistaudiofeatures)
      playlistaudiofeatures |>
        dplyr::transmute(
          playlist_name,
          track.id,
          track.name,
          artist.name       = purrr::map_chr(track.artists, function(x) x$name[1]),
          popularity        = track.popularity,
          duration          = round(track.duration_ms / 60000, 2),
          explicit          = as.integer(track.explicit),
          release_year      = as.integer(substr(track.album.release_date, 1, 4)),
          artist_popularity,
          dplyr::across(dplyr::starts_with("genre_"))
        ) |>
        dplyr::distinct(.keep_all = TRUE)
    })

    observe({
      data <- playlistaudiofeatures_react()
      req(data)
      pnames <- unique(data$playlist_name)
      for (i in seq_along(pnames)) {
        # local() captures i by value — without this all outputs point to the last i
        local({
          li <- i
          ln <- pnames[li]
          output[[paste0("playlist_dt_", li)]] <- renderDT({
            data |>
              dplyr::filter(playlist_name == ln) |>
              dplyr::select(track.name, artist.name) |>
              datatable(rownames = FALSE)
          })
        })
      }
    })

    observeEvent(input$perform_cluster, {
      req(playlistaudiofeatures_react())
      playlist_data <- playlistaudiofeatures_react()

      features <- playlist_data |>
        dplyr::select(popularity, duration, explicit, release_year, artist_popularity, dplyr::starts_with("genre_")) |>
        scale()

      if (input$algo == "k-means") {
        k <- as.numeric(input$selected_clusters)
        if (nrow(playlist_data) < max(k, 10)) {
          showNotification("Not enough tracks for clustering. Select playlists with more tracks.",
                           type = "warning", duration = 5)
          return()
        }
        fitted_km <- tryCatch({
          spec <- tidyclust::k_means(num_clusters = k) |>
            parsnip::set_engine("stats", nstart = 25)
          set.seed(123)
          parsnip::fit(spec, ~ ., data = as.data.frame(features))
        }, error = function(e) {
          showNotification(paste("Clustering failed:", e$message), type = "error", duration = NULL)
          NULL
        })
        req(fitted_km)
        cluster_results(fitted_km)
        # .cluster is a factor — convert to int to match DBSCAN's format
        playlist_data$cluster <- as.integer(
          tidyclust::extract_cluster_assignment(fitted_km)$.cluster
        )
      } else {
        db <- tryCatch(
          dbscan::dbscan(features, eps = input$eps, minPts = input$minPts),
          error = function(e) {
            showNotification(paste("DBSCAN failed:", e$message), type = "error", duration = NULL)
            NULL
          }
        )
        req(db)
        n_real <- length(unique(db$cluster[db$cluster > 0]))
        if (n_real > 6) {
          showNotification(
            paste0("DBSCAN found ", n_real, " clusters. Try a higher eps or lower minPts."),
            type = "warning", duration = 8
          )
          return()
        }
        cluster_results(db)
        playlist_data$cluster <- db$cluster
      }

      cluster_playlist(playlist_data)
      clustering_performed(TRUE)
    })

    output$cluster_summary <- renderDT({
      req(cluster_playlist(), input$cluster_filter)
      cluster_playlist() |>
        dplyr::filter(cluster == as.integer(input$cluster_filter)) |>
        dplyr::select(track.name, artist.name) |>
        dplyr::distinct() |>
        datatable(rownames = FALSE)
    })

    output$cluster_features_plot <- renderPlotly({
      req(cluster_playlist())

      numeric_features <- c("popularity", "duration", "explicit", "artist_popularity", "release_year")
      all_data         <- cluster_playlist()[, numeric_features, drop = FALSE]
      clusters         <- sort(unique(cluster_playlist()$cluster))
      palette          <- c("#1DB954", "#FF6B6B", "#4ECDC4", "#FFE66D", "#A8E6CF", "#FF8B94", "#888888")

      fig <- plotly::plot_ly(type = "scatterpolar")

      for (i in seq_along(clusters)) {
        cl    <- clusters[i]
        label <- if (cl == 0) "Unclassified" else paste("Cluster", cl)

        means <- cluster_playlist() |>
          dplyr::filter(cluster == cl) |>
          dplyr::select(dplyr::all_of(numeric_features)) |>
          dplyr::summarise(dplyr::across(dplyr::everything(), \(x) mean(x, na.rm = TRUE)))

        r_vals <- sapply(numeric_features, function(f) {
          vmin <- min(all_data[[f]], na.rm = TRUE)
          vmax <- max(all_data[[f]], na.rm = TRUE)
          if (vmax == vmin) 50 else (means[[f]] - vmin) / (vmax - vmin) * 100
        })

        fig <- fig |> plotly::add_trace(
          r         = c(r_vals, r_vals[1]),
          theta     = c(numeric_features, numeric_features[1]),
          name      = label,
          fill      = "toself",
          fillcolor = paste0(palette[i], "40"),
          line      = list(color = palette[i], width = 2)
        )
      }

      fig |> plotly::layout(
        polar = list(
          radialaxis  = list(range = c(0, 100), ticksuffix = "%", gridcolor = "#444", linecolor = "#444"),
          angularaxis = list(gridcolor = "#444")
        ),
        paper_bgcolor = "#060606",
        plot_bgcolor  = "#1a1a1a",
        font          = list(color = "#ffffff"),
        legend        = list(font = list(color = "#ffffff"))
      )
    })

    output$download_csv <- downloadHandler(
      filename = function() paste0("new_playlist_cluster_", input$cluster_filter, ".csv"),
      content = function(file) {
        req(cluster_playlist())
        cluster_playlist() |>
          dplyr::filter(cluster == as.integer(input$cluster_filter)) |>
          dplyr::transmute(new_playlist = cluster, track.name, artist.name) |>
          dplyr::distinct() |>
          write.csv(file, row.names = FALSE)
      }
    )

    observeEvent(input$create_playlist_btn, {
      req(cluster_playlist(), input$cluster_filter, input$new_playlist_name)
      playlist_name <- trimws(input$new_playlist_name)
      if (nchar(playlist_name) == 0) {
        showNotification("Please enter a name for the playlist.", type = "warning")
        return()
      }
      track_ids <- cluster_playlist() |>
        dplyr::filter(cluster == as.integer(input$cluster_filter)) |>
        dplyr::pull(track.id) |>
        unique()
      tryCatch({
        create_spotify_playlist(user_id_rv(), playlist_name, track_ids, access_token_rv())
        showNotification(
          paste0("Playlist '", playlist_name, "' created in Spotify."),
          type = "message", duration = 6
        )
      }, error = function(e) {
        showNotification(paste("Error creating playlist:", e$message), type = "error", duration = NULL)
      })
    })
  })
}
