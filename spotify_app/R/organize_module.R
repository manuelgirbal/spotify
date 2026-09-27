organizeUI <- function(id) {
  ns <- NS(id)
  panel <- function(side) {
    card(
      fill = FALSE,
      card_header(selectInput(ns(side), NULL, choices = NULL, width = "100%")),
      DTOutput(ns(paste0(side, "_table")), fill = FALSE)
    )
  }
  tagList(
    uiOutput(ns("notice")),
    layout_columns(
      col_widths = c(5, 2, 5),
      panel("left"),
      div(
        class = "d-flex flex-column gap-2 mt-5",
        radioButtons(ns("mode"), NULL, c("Move", "Copy"), inline = TRUE),
        actionButton(ns("to_right"), "Selected →"),
        actionButton(ns("to_left"), "← Selected")
      ),
      panel("right")
    ),
    card(
      card_header("Pending changes"),
      uiOutput(ns("pending")),
      div(
        class = "d-flex gap-2",
        actionButton(ns("undo"), "Undo"),
        actionButton(ns("discard"), "Discard all"),
        actionButton(ns("confirm"), "Confirm", class = "btn-success"),
        actionButton(ns("revert"), "Revert last change", class = "btn-outline-warning ms-auto")
      )
    )
  )
}

tracks_table <- function(current, original) {
  table <- current |>
    dplyr::transmute(
      status   = ifelse(track_id %in% original$track_id, "", "added"),
      Track    = track_name,
      Artist   = artist_name,
      Album    = album_name,
      Year     = release_year,
      Minutes  = duration_min,
      Popularity = popularity,
      Genres   = purrr::map_chr(genres, \(g) paste(utils::head(g, 3), collapse = ", ")),
      Added    = added_at
    )
  # Bootstrap 5 paints cell backgrounds with an inset box-shadow, so highlight the same way
  table |>
    datatable(
      rownames = FALSE,
      class    = "compact nowrap hover",
      options  = list(pageLength = 15, dom = "ftip", scrollX = TRUE, columnDefs = list(list(visible = FALSE, targets = 0)))
    ) |>
    formatStyle(names(table)[-1], valueColumns = "status",
                boxShadow = styleEqual("added", "inset 0 0 0 9999px rgba(29, 185, 84, 0.35)"))
}

organizeServer <- function(id, user_id, user_playlists) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    loaded      <- reactiveVal(list())
    ops         <- reactiveVal(list())
    last_commit <- reactiveVal(NULL)

    original <- reactive(purrr::map(loaded(), "tracks"))
    current  <- reactive(apply_ops(original(), ops()))
    diff     <- reactive(pending_diff(original(), current()))
    name_of  <- function(pid) {
      known <- c(stats::setNames(user_playlists()$name, user_playlists()$id), purrr::map_chr(loaded(), "name"))
      unname(ifelse(pid %in% names(known), known[pid], pid))
    }

    load_playlists <- function(pids) {
      if (length(pids) == 0) return()
      withProgress(message = "Loading playlist", value = 0, {
        for (pid in pids) {
          incProgress(1 / length(pids), detail = name_of(pid))
          snapshot <- fetch_playlist_snapshot(pid)
          tracks   <- fetch_tracks_with_artists(dplyr::tibble(id = pid, name = name_of(pid)))
          entry    <- list(tracks = tracks, snapshot = snapshot, name = name_of(pid))
          loaded(utils::modifyList(loaded(), stats::setNames(list(entry), pid)))
        }
      })
    }

    observe({
      choices <- playlist_choices(user_playlists())
      keep    <- function(value, fallback) if (isTRUE(value %in% choices)) value else fallback
      updateSelectInput(session, "left", choices = choices, selected = keep(isolate(input$left), choices[1]))
      updateSelectInput(session, "right", choices = choices, selected = keep(isolate(input$right), choices[2]))
    })

    observeEvent(c(input$left, input$right), {
      missing <- setdiff(c(input$left, input$right), c("", names(loaded())))
      with_notify(load_playlists(missing), "Error loading playlist")
    })

    output$notice <- renderUI(if (is.null(user_id())) login_notice())

    # Staged additions first so they're visible; the table and the selection share this order
    view_of <- function(pid) {
      tracks <- current()[[pid]]
      tracks[order(tracks$track_id %in% original()[[pid]]$track_id), ]
    }

    render_side <- function(side) {
      renderDT({
        pid <- input[[side]]
        req(pid, current()[[pid]])
        tracks_table(view_of(pid), original()[[pid]])
      })
    }
    output$left_table  <- render_side("left")
    output$right_table <- render_side("right")

    stage <- function(from_side, to_side) {
      from <- input[[from_side]]
      to   <- input[[to_side]]
      rows <- input[[paste0(from_side, "_table_rows_selected")]]
      if (identical(from, to)) {
        showNotification("Pick two different playlists.", type = "warning")
        return()
      }
      if (length(rows) == 0) {
        showNotification("Select some tracks first.", type = "warning")
        return()
      }
      ids  <- view_of(from)$track_id[rows]
      dups <- sum(ids %in% current()[[to]]$track_id)
      if (dups > 0) showNotification(paste(dups, "track(s) already in", name_of(to), "were not added again."), type = "warning")
      ops(c(ops(), list(list(mode = input$mode, from = from, to = to, track_ids = ids))))
    }
    observeEvent(input$to_right, stage("left", "right"))
    observeEvent(input$to_left, stage("right", "left"))

    observeEvent(input$undo, ops(utils::head(ops(), -1)))
    observeEvent(input$discard, ops(list()))

    output$pending <- renderUI({
      if (length(ops()) == 0) return(p(class = "text-muted", "No pending changes. Moves and copies stay here until you confirm."))
      steps <- purrr::map(ops(), \(op) tags$li(sprintf("%s %d track(s): %s → %s", op$mode, length(op$track_ids), name_of(op$from), name_of(op$to))))
      net   <- purrr::imap(diff(), \(d, pid) sprintf("%s: +%d / −%d", name_of(pid), length(d$add), length(d$remove)))
      tagList(
        tags$ol(steps),
        p(strong("Will be sent to Spotify: "), if (length(net)) paste(net, collapse = " · ") else "nothing (changes cancel out)")
      )
    })

    reload_after_write <- function(pids) {
      loaded(loaded()[setdiff(names(loaded()), pids)])
      ops(list())
      user_playlists(fetch_user_playlists(user_id()))
      load_playlists(intersect(pids, user_playlists()$id))
    }

    changed_in_spotify <- function(pids, expected) {
      pids[purrr::map_lgl(pids, \(pid) !identical(fetch_playlist_snapshot(pid), expected[[pid]]))]
    }

    observeEvent(input$confirm, {
      if (length(diff()) == 0) {
        showNotification("Nothing to send.", type = "warning")
        return()
      }
      showModal(modalDialog(
        title = "Send changes to Spotify?",
        tags$ul(purrr::imap(diff(), \(d, pid) tags$li(sprintf("%s: add %d, remove %d", name_of(pid), length(d$add), length(d$remove))))),
        footer = tagList(modalButton("Cancel"), actionButton(ns("confirm_ok"), "Confirm", class = "btn-success"))
      ))
    })

    observeEvent(input$confirm_ok, {
      removeModal()
      pending <- diff()
      pids    <- names(pending)
      changed <- changed_in_spotify(pids, purrr::map(loaded(), "snapshot"))
      if (length(changed)) {
        showNotification(paste("Changed in Spotify since loaded:", paste(name_of(changed), collapse = ", "), "- reloaded, pending changes discarded."), type = "warning", duration = NULL)
        with_notify(reload_after_write(changed), "Error reloading")
        return()
      }
      changes <- withProgress(message = "Sending changes to Spotify",
        with_notify(commit_changes(pending, original()), "Error sending changes (reloaded to show the real state)"))
      with_notify(reload_after_write(pids), "Error reloading")
      req(changes)
      last_commit(list(changes = changes, deleted = character(), snapshots = purrr::map(loaded()[pids], "snapshot")))
      showNotification("Changes sent to Spotify.", type = "message")

      empty <- intersect(pids, user_playlists()$id[user_playlists()$total == 0])
      if (length(empty)) {
        showModal(modalDialog(
          title = "Empty playlists",
          p("These playlists are now empty: ", strong(paste(name_of(empty), collapse = ", ")), ". Delete them?"),
          p(class = "text-muted", "You can bring them back with Revert last change."),
          footer = tagList(modalButton("Keep"), actionButton(ns("delete_empty"), "Delete", class = "btn-danger"))
        ))
      }
    })

    observeEvent(input$delete_empty, {
      removeModal()
      empty <- intersect(names(last_commit()$changes), user_playlists()$id[user_playlists()$total == 0])
      deleted <- with_notify({
        purrr::walk(empty, unfollow_playlist)
        empty
      }, "Error deleting playlist")
      req(deleted)
      last_commit(utils::modifyList(last_commit(), list(deleted = deleted)))
      user_playlists(fetch_user_playlists(user_id()))
    })

    observeEvent(input$revert, {
      if (is.null(last_commit())) {
        showNotification("Nothing to revert in this session.", type = "warning")
        return()
      }
      if (length(ops())) {
        showNotification("Confirm or discard pending changes first.", type = "warning")
        return()
      }
      showModal(modalDialog(
        title = "Revert last change?",
        p("Tracks go back to their original playlists and positions. Deleted playlists are restored."),
        footer = tagList(modalButton("Cancel"), actionButton(ns("revert_ok"), "Revert", class = "btn-warning"))
      ))
    })

    observeEvent(input$revert_ok, {
      removeModal()
      commit <- last_commit()
      pids   <- names(commit$changes)
      changed <- changed_in_spotify(pids, commit$snapshots)
      if (length(changed)) {
        showNotification(paste("Can't revert:", paste(name_of(changed), collapse = ", "), "changed in Spotify since."), type = "error", duration = NULL)
        return()
      }
      withProgress(message = "Reverting in Spotify", with_notify(revert_changes(commit), "Error reverting"))
      with_notify(reload_after_write(pids), "Error reloading")
      last_commit(NULL)
      showNotification("Last change reverted.", type = "message")
    })
  })
}
