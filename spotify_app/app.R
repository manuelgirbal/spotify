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

spotify_get <- function(url, token, params = list()) {
  res <- httr::RETRY(
    "GET", url,
    httr::config(token = token),
    query = params,
    encode = "json",
    terminate_on = c(401, 403, 404)
  )
  httr::stop_for_status(res)
  jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"), flatten = TRUE)
}

fetch_playlist_audio_features <- function(playlist_ids, token) {
  all_tracks <- purrr::map_df(playlist_ids, function(pid) {
    meta  <- spotify_get(paste0("https://api.spotify.com/v1/playlists/", pid), token)
    total <- meta$tracks$total
    if (total == 0) return(dplyr::tibble())

    purrr::map_df(seq(0, total - 1, by = 100), function(offset) {
      res <- spotify_get(
        paste0("https://api.spotify.com/v1/playlists/", pid, "/tracks"),
        token,
        params = list(limit = 100, offset = offset)
      )
      res$items |>
        dplyr::filter(!is.na(track.id)) |>
        dplyr::mutate(playlist_name = meta$name)
    })
  })

  if (nrow(all_tracks) == 0) return(all_tracks)

  artist_ids <- purrr::map_chr(all_tracks$track.artists, function(x) {
    if (is.null(x) || nrow(x) == 0) NA_character_ else x$id[1]
  })

  unique_artist_ids <- unique(stats::na.omit(artist_ids))

  artist_data <- purrr::map_df(
    split(unique_artist_ids, ceiling(seq_along(unique_artist_ids) / 50)),
    function(ids) {
      res <- spotify_get(
        "https://api.spotify.com/v1/artists",
        token,
        params = list(ids = paste(ids, collapse = ","))
      )
      dplyr::tibble(
        artist_id         = res$artists$id,
        artist_popularity = res$artists$popularity,
        genres            = res$artists$genres
      )
    }
  )

  all_genres <- purrr::flatten_chr(artist_data$genres)
  top_genres  <- names(sort(table(all_genres), decreasing = TRUE))[seq_len(min(10, length(unique(all_genres))))]

  all_tracks <- all_tracks |>
    dplyr::mutate(primary_artist_id = artist_ids) |>
    dplyr::left_join(artist_data, by = c("primary_artist_id" = "artist_id"))

  for (g in top_genres) {
    col_name <- paste0("genre_", gsub("[^a-zA-Z0-9]", "_", g))
    all_tracks[[col_name]] <- purrr::map_int(all_tracks$genres, function(gs) {
      if (is.null(gs)) 0L else as.integer(g %in% gs)
    })
  }

  all_tracks
}

fetch_backup_tracks <- function(playlist_id, token) {
  meta  <- spotify_get(paste0("https://api.spotify.com/v1/playlists/", playlist_id), token)
  total <- meta$tracks$total
  if (total == 0) return(dplyr::tibble())

  purrr::map_df(seq(0, total - 1, by = 100), function(offset) {
    res <- spotify_get(
      paste0("https://api.spotify.com/v1/playlists/", playlist_id, "/tracks"),
      token,
      params = list(limit = 100, offset = offset)
    )
    res$items |>
      dplyr::filter(!is.na(track.id)) |>
      dplyr::transmute(
        track_name   = track.name,
        artist_name  = purrr::map_chr(track.artists, function(x) x$name[1]),
        album_name   = track.album.name,
        duration_min = round(track.duration_ms / 60000, 2),
        explicit     = track.explicit,
        release_year = as.integer(substr(track.album.release_date, 1, 4))
      )
  })
}

create_spotify_playlist <- function(user_id, playlist_name, track_ids, token) {
  res <- httr::POST(
    paste0("https://api.spotify.com/v1/users/", user_id, "/playlists"),
    httr::config(token = token),
    httr::content_type_json(),
    body = jsonlite::toJSON(list(name = playlist_name, public = FALSE), auto_unbox = TRUE)
  )
  httr::stop_for_status(res)
  playlist_id <- jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"))$id

  batches <- split(track_ids, ceiling(seq_along(track_ids) / 100))
  for (batch in batches) {
    uris <- paste0("spotify:track:", batch)
    res2 <- httr::POST(
      paste0("https://api.spotify.com/v1/playlists/", playlist_id, "/tracks"),
      httr::config(token = token),
      httr::content_type_json(),
      body = jsonlite::toJSON(list(uris = uris), auto_unbox = TRUE)
    )
    httr::stop_for_status(res2)
  }

  playlist_id
}

ui <- navbarPage(
  "SpotiApp",
  theme = shinytheme("cyborg"),

  tabPanel("Clustering",
    sidebarLayout(
      uiOutput("dynamic_sidebar"),
      mainPanel(
        uiOutput("auth_status"),
        uiOutput("dynamic_main_panel")
      )
    )
  ),

  tabPanel("Backup",
    uiOutput("backup_panel")
  )
)

server <- function(input, output, session) {

  auth_status          <- reactiveVal(FALSE)
  access_token_rv      <- reactiveVal(NULL)
  user_playlists       <- reactiveVal(NULL)
  cluster_results      <- reactiveVal(NULL)
  cluster_playlist     <- reactiveVal(NULL)
  clustering_performed <- reactiveVal(FALSE)
  user_id_rv           <- reactiveVal(NULL)

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

      playlists <- fetch_user_playlists(access_token)
      user_playlists(playlists)
    }, error = function(e) {
      auth_status(FALSE)
      showNotification(paste("Authentication failed:", e$message), type = "error", duration = NULL)
    })
  })

  output$dynamic_sidebar <- renderUI({
    if (auth_status()) {
      sidebarPanel(
        selectizeInput(
          "selected_playlists",
          "Select up to 2 playlists:",
          choices  = if (!is.null(user_playlists())) unique(user_playlists()$name) else NULL,
          multiple = TRUE,
          selected = NULL,
          options  = list(maxItems = 2)
        ),
        radioButtons("algo", "Algorithm:", choices = c("k-means", "DBSCAN"), inline = TRUE),
        actionLink("algo_info", "Which one? \u2139\ufe0f"),
        br(), br(),
        conditionalPanel(
          condition = "input.algo == 'k-means'",
          selectInput("selected_clusters", "Number of playlists:", choices = 2:4)
        ),
        conditionalPanel(
          condition = "input.algo == 'DBSCAN'",
          sliderInput("eps",    "eps",    min = 0.1, max = 2.0, value = 0.5, step = 0.1),
          sliderInput("minPts", "minPts", min = 2,   max = 10,  value = 3)
        ),
        actionButton("perform_cluster", "Perform Cluster Analysis")
      )
    } else {
      sidebarPanel(
        actionButton("auth_button", "Authorize Spotify")
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
              \u274c Can be unpredictable with very mixed playlists<br>
              \u274c Requires tuning eps and minPts<br><br>
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
          do.call(tabsetPanel, c(list(id = "cluster_filter"), tabs)),
          br(),
          downloadButton("download_csv", "Download as CSV"),
          br(), br(),
          textInput("new_playlist_name", "Save to Spotify as:",
                    placeholder = "E.g.: My rock 2000s playlist"),
          actionButton("create_playlist_btn", "Create in Spotify \u25b6"),
          br(), br(),
          tabsetPanel(
            tabPanel("Tracks",
              br(),
              DTOutput("cluster_summary")
            ),
            tabPanel("Characteristics",
              br(),
              helpText(
                "Each bar shows the average value of that feature for this cluster,",
                "normalized relative to the full dataset range.",
                "100% = highest value across all tracks, 0% = lowest.",
                "Hover over the points to see the actual values."
              ),
              plotlyOutput("cluster_features_plot")
            )
          )
        )
      } else {
        req(playlistaudiofeatures_react())
        pnames <- unique(playlistaudiofeatures_react()$playlist_name)
        tabs <- lapply(seq_along(pnames), function(i) {
          tabPanel(pnames[i], br(), DTOutput(paste0("playlist_dt_", i)))
        })
        do.call(tabsetPanel, tabs)
      }
    }
  })

  observe({
    data <- playlistaudiofeatures_react()
    req(data)
    pnames <- unique(data$playlist_name)
    for (i in seq_along(pnames)) {
      local({
        li <- i
        ln <- pnames[li]
        output[[paste0("playlist_dt_", li)]] <- renderDT({
          data |>
            filter(playlist_name == ln) |>
            select(track.name, artist.name) |>
            datatable(rownames = FALSE)
        })
      })
    }
  })

  fetch_user_playlists <- function(access_token) {
    i <- 0
    my_playlists <- tibble()
    repeat {
      batch <- get_my_playlists(authorization = access_token, limit = 50, offset = i)
      my_playlists <- bind_rows(my_playlists, batch)
      if (nrow(batch) < 50) break
      i <- i + 50
    }
    user_id <- get_my_profile(authorization = access_token)$id
    user_id_rv(user_id)
    my_playlists |>
      filter(owner.id == user_id) |>
      select(id, name)
  }

  playlistaudiofeatures_react <- reactive({
    req(user_playlists())
    req(input$selected_playlists)
    playlists_filtered <- user_playlists() |> filter(name %in% input$selected_playlists)
    playlistaudiofeatures <- tryCatch(
      fetch_playlist_audio_features(playlists_filtered$id, access_token_rv()),
      error = function(e) {
        showNotification(paste("Error fetching tracks:", e$message), type = "error", duration = NULL)
        NULL
      }
    )
    req(playlistaudiofeatures)
    playlistaudiofeatures |>
      transmute(
        playlist_name,
        track.id,
        track.name,
        artist.name       = map_chr(track.artists, function(x) x$name[1]),
        popularity        = track.popularity,
        duration          = round(track.duration_ms / 60000, 2),
        explicit          = as.integer(track.explicit),
        release_year      = as.integer(substr(track.album.release_date, 1, 4)),
        artist_popularity,
        across(starts_with("genre_"))
      ) |>
      distinct(.keep_all = TRUE)
  })

  observeEvent(input$perform_cluster, {
    req(playlistaudiofeatures_react())
    playlist_data <- playlistaudiofeatures_react()

    features <- playlist_data |>
      select(popularity, duration, explicit, release_year, artist_popularity, starts_with("genre_")) |>
      scale()

    if (input$algo == "k-means") {
      k <- as.numeric(input$selected_clusters)
      if (nrow(playlist_data) < max(k, 10)) {
        showNotification("Not enough tracks for clustering. Select playlists with more tracks.",
                         type = "warning", duration = 5)
        return()
      }
      set.seed(123)
      km <- tryCatch(
        kmeans(features, centers = k, nstart = 25),
        error = function(e) {
          showNotification(paste("Clustering failed:", e$message), type = "error", duration = NULL)
          NULL
        }
      )
      req(km)
      cluster_results(km)
      playlist_data$cluster <- km$cluster
    } else {
      db <- tryCatch(
        dbscan::dbscan(features, eps = input$eps, minPts = input$minPts),
        error = function(e) {
          showNotification(paste("DBSCAN failed:", e$message), type = "error", duration = NULL)
          NULL
        }
      )
      req(db)
      cluster_results(db)
      playlist_data$cluster <- db$cluster
    }

    cluster_playlist(playlist_data)
    clustering_performed(TRUE)
  })

  output$cluster_summary <- renderDT({
    req(cluster_playlist(), input$cluster_filter)
    cluster_playlist() |>
      filter(cluster == as.integer(input$cluster_filter)) |>
      select(track.name, artist.name) |>
      distinct() |>
      datatable(rownames = FALSE)
  })

  output$cluster_features_plot <- renderPlotly({
    req(cluster_playlist(), input$cluster_filter)

    numeric_features <- c("popularity", "duration", "artist_popularity", "release_year")
    all_data <- cluster_playlist()[, numeric_features, drop = FALSE]

    plot_data <- cluster_playlist() |>
      filter(cluster == as.integer(input$cluster_filter)) |>
      select(all_of(numeric_features)) |>
      summarise(across(everything(), \(x) mean(x, na.rm = TRUE))) |>
      pivot_longer(everything(), names_to = "feature", values_to = "value") |>
      mutate(
        vmin       = sapply(feature, \(f) min(all_data[[f]], na.rm = TRUE)),
        vmax       = sapply(feature, \(f) max(all_data[[f]], na.rm = TRUE)),
        normalized = ifelse(vmax == vmin, 50, (value - vmin) / (vmax - vmin) * 100),
        tooltip    = paste0(feature, ": ", round(value, 1))
      )

    p <- ggplot(plot_data, aes(x = reorder(feature, normalized), y = normalized, text = tooltip)) +
      geom_segment(aes(xend = feature, y = 0, yend = normalized), color = "#1DB954", linewidth = 1) +
      geom_point(color = "#1DB954", size = 4) +
      coord_flip() +
      scale_y_continuous(limits = c(0, 100)) +
      labs(
        title = paste("Feature averages \u2014 Cluster", input$cluster_filter),
        x = NULL, y = "Relative to dataset range (%)"
      ) +
      theme_dark() +
      theme(
        plot.background  = element_rect(fill = "#060606"),
        panel.background = element_rect(fill = "#1a1a1a")
      )

    ggplotly(p, tooltip = "text")
  })

  output$download_csv <- downloadHandler(
    filename = function() paste0("new_playlist_cluster_", input$cluster_filter, ".csv"),
    content = function(file) {
      req(cluster_playlist())
      cluster_playlist() |>
        filter(cluster == as.integer(input$cluster_filter)) |>
        transmute(new_playlist = cluster, track.name, artist.name) |>
        distinct() |>
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
      filter(cluster == as.integer(input$cluster_filter)) |>
      pull(track.id) |>
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

  output$backup_panel <- renderUI({
    if (!auth_status()) {
      return(p("Please authorize the app in the Clustering tab first."))
    }
    fluidRow(
      column(5,
        h4("Export selected playlists"),
        selectizeInput(
          "backup_playlists",
          "Select one or more playlists:",
          choices  = if (!is.null(user_playlists()))
            setNames(user_playlists()$id, user_playlists()$name)
          else NULL,
          multiple = TRUE,
          selected = NULL
        ),
        downloadButton("backup_download", "Export as CSV / ZIP"),
        hr(),
        h4("Full backup"),
        p("Download all your playlists as a ZIP, one CSV per playlist."),
        downloadButton("backup_all", "Download all as ZIP")
      )
    )
  })

  output$backup_download <- downloadHandler(
    filename = function() {
      req(input$backup_playlists)
      if (length(input$backup_playlists) == 1) {
        pname <- user_playlists() |> filter(id == input$backup_playlists) |> pull(name)
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
          pname    <- user_playlists() |> filter(id == pid) |> pull(name)
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
    filename = function() paste0("backup_completo_", format(Sys.Date(), "%Y%m%d"), ".zip"),
    content = function(file) {
      req(user_playlists())
      all_ids <- user_playlists()$id
      tmp_dir <- file.path(tempdir(), paste0("backup_all_", as.integer(Sys.time())))
      dir.create(tmp_dir, showWarnings = FALSE)
      csv_files <- character(0)
      for (pid in all_ids) {
        pname    <- user_playlists() |> filter(id == pid) |> pull(name)
        tracks   <- tryCatch(
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
}

options(shiny.port = 3838)
shinyApp(ui = ui, server = server)
