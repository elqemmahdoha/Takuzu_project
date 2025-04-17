library(shiny)
library(bslib)
library(takuzuu)
library(shinyWidgets)

ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly", base_font = font_google("Quicksand")),

  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('setBodyClass', function(className) {
        document.body.className = className;
      });
    ")),
    tags$style(HTML("
      body.dark-mode {
        background-color: #121212 !important;
        color: #f5f5f5 !important;
      }

      .card, .card-header {
        background-color: inherit !important;
        border: none;
      }

      .grid-cell {
        width: 60px;
        height: 60px;
        font-size: 20px;
        margin: 3px;
        border-radius: 10px !important;
        font-weight: bold;
      }

      .btn-primary, .btn-secondary, .btn-success, .btn-warning {
        margin-bottom: 8px;
        width: 100%;
      }

      .btn-choose {
        width: 100px;
        height: 45px;
        font-size: 18px;
        font-weight: bold;
        margin: 5px;
      }

      .sidebar-controls {
        margin-bottom: 25px;
      }

      .chrono-box {
        background-color: #f8f9fa;
        padding: 10px 15px;
        border-radius: 10px;
        display: inline-block;
        font-weight: bold;
        font-size: 18px;
        color: #333;
        box-shadow: 1px 1px 5px rgba(0,0,0,0.1);
      }

      .chrono-container {
        display: flex;
        justify-content: flex-start;
        margin-bottom: 20px;
      }

      .choose-container {
        display: flex;
        justify-content: center;
        margin-top: 20px;
      }

      @keyframes flash {
        0% { opacity: 1; }
        50% { opacity: 0.2; }
        100% { opacity: 1; }
      }

      .victory-overlay {
        position: absolute;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        font-size: 60px;
        font-weight: bold;
        color: #28a745;
        text-shadow: 3px 3px 8px #000;
        z-index: 9999;
        animation: flash 1s infinite;
        background-color: rgba(255, 255, 255, 0.85);
        padding: 30px 50px;
        border-radius: 20px;
        box-shadow: 0 0 30px rgba(0, 0, 0, 0.3);
      }

      .victory-container {
        position: relative;
        min-height: 300px;
      }
    "))
  ),

  titlePanel(
    div(
      h2("🧠 Jeu Takuzu", class = "text-center"),
      p("Remplis la grille avec des 0 et des 1, sans triplets, avec équilibre, et sans doublons !", class = "text-center")
    )
  ),

  card(
    full_screen = TRUE,
    height = "auto",
    card_header("🎮 Plateau de jeu interactif"),

    layout_sidebar(
      fillable = TRUE,

      sidebar = sidebar(
        title = "🎛 Contrôles du jeu",
        class = "sidebar-controls",

        switchInput("dark_mode", "🌙 Mode sombre", value = FALSE, inline = TRUE),
        tags$hr(),

        selectInput("grid_size", "📐 Taille de la grille :", choices = c("4x4" = 4, "6x6" = 6, "8x8" = 8), selected = 6),
        selectInput("difficulty", "🎯 Difficulté :", choices = c("Facile", "Moyen", "Difficile"), selected = "Moyen"),

        actionButton("regen", "🔄 Nouvelle Grille", class = "btn-primary"),
        actionButton("reset", "♻ Réinitialiser", class = "btn-secondary"),
        tags$hr(),
        actionButton("validate", "✅ Valider la Grille", class = "btn-success"),
        actionButton("hint", "💡 Indice", class = "btn-info"),
        actionButton("erase_errors", "🧹 Effacer les erreurs", class = "btn-warning"),
        actionButton("show_solution", "🧩 Afficher la solution", class = "btn-warning"),
        tags$hr(),
        h4("📝 État du jeu :"),
        textOutput("status")
      ),

      mainPanel(
        class = "text-center",

        div(
          class = "chrono-container",
          div(class = "chrono-box", "⏱ Temps écoulé : ", textOutput("chrono", inline = TRUE))
        ),

        # Grille + message de victoire superposé
        div(
          class = "victory-container",
          uiOutput("grid"),
          uiOutput("victory_message")
        ),

        div(
          class = "choose-container",
          actionButton("choose_0", "Choisir 0", class = "btn-info btn-choose"),
          actionButton("choose_1", "Choisir 1", class = "btn-info btn-choose")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  grid <- reactiveVal(NULL)
  grid_original <- reactiveVal(NULL)
  solution <- reactiveVal(NULL)
  fixed_cells <- reactiveVal(NULL)
  selected_value <- reactiveVal(NULL)
  status_message <- reactiveVal("Cliquez sur 🔄 Nouvelle Grille pour commencer")
  hinted_cells <- reactiveVal(matrix(FALSE, nrow = 8, ncol = 8))

  start_time <- reactiveVal(NULL)
  timer_active <- reactiveVal(FALSE)
  show_feedback <- reactiveVal(FALSE)
  autoInvalidate <- reactiveTimer(1000)

  observeEvent(input$hint, {
    g <- grid()
    fixed <- fixed_cells()
    sol <- solution()
    hints <- hinted_cells()

    if (is.null(g) || is.null(fixed) || is.null(sol)) return()

    empty_cells <- which(is.na(g), arr.ind = TRUE)
    if (nrow(empty_cells) > 0) {
      rand_cell <- empty_cells[sample(nrow(empty_cells), 1), ]
      row <- rand_cell[1]; col <- rand_cell[2]

      g[row, col] <- sol[row, col]
      hints[row, col] <- TRUE
      grid(g)
      hinted_cells(hints)

      fixed[row, col] <- TRUE
      fixed_cells(fixed)

      status_message("💡 Indice : une case a été révélée !")
    } else {
      status_message("❗ Aucune case vide à révéler.")
    }
  })

  generate_new_grid <- function() {
    taille <- as.numeric(input$grid_size)
    hinted_cells(matrix(FALSE, nrow = taille, ncol = taille))

    proportion <- switch(
      input$difficulty,
      "Facile" = 0.5,
      "Moyen" = 0.35,
      "Difficile" = 0.2
    )

    jeu <- generer_takuzu_jouable(taille, proportion_visible = proportion)

    grid(jeu$grille_visible)
    grid_original(jeu$grille_visible)
    solution(jeu$solution)
    fixed_cells(!is.na(jeu$grille_visible))
    status_message("✅ Nouvelle grille générée.")
    start_time(Sys.time())
    timer_active(TRUE)
    show_feedback(FALSE)
  }

  observe({ if (is.null(grid())) generate_new_grid() })
  observeEvent(input$grid_size, { generate_new_grid() })
  observeEvent(input$regen, { generate_new_grid() })

  observeEvent(input$reset, {
    if (!is.null(grid_original())) {
      grid(grid_original())
      fixed_cells(!is.na(grid_original()))
      status_message("♻ Grille réinitialisée.")
      start_time(Sys.time())
      timer_active(TRUE)
      show_feedback(FALSE)
    }
  })

  observeEvent(input$choose_0, { selected_value(0) })
  observeEvent(input$choose_1, { selected_value(1) })

  observe({
    if (isTRUE(input$dark_mode)) {
      session$sendCustomMessage(type = "setBodyClass", message = "dark-mode")
    } else {
      session$sendCustomMessage(type = "setBodyClass", message = "")
    }
  })

  observe({
    g <- grid()
    fixed <- fixed_cells()
    selected <- selected_value()
    if (is.null(g) || is.null(fixed) || is.null(selected)) return()

    n <- nrow(g)
    m <- ncol(g)

    isolate({
      for (i in 1:n) {
        for (j in 1:m) {
          if (fixed[i, j]) next

          local({
            row <- i; col <- j; cell_id <- paste0("cell_", row, "_", col)

            observeEvent(input[[cell_id]], {
              req(input[[cell_id]])
              current_grid <- isolate(grid())
              current_grid[row, col] <- selected
              grid(current_grid)
              show_feedback(FALSE)
            }, ignoreInit = TRUE)
          })
        }
      }
    })
  })

  observeEvent(input$show_solution, {
    sol <- solution()
    if (is.null(sol)) {
      status_message("❗ Solution introuvable.")
      return()
    }
    grid(sol)
    fixed_cells(matrix(TRUE, nrow = nrow(sol), ncol = ncol(sol)))
    status_message("✅ Solution affichée.")
    timer_active(FALSE)
    show_feedback(FALSE)
  })

  observeEvent(input$validate, {
    g <- grid()
    if (is.null(g)) {
      status_message("❗ Veuillez d'abord générer une grille.")
      return()
    }
    show_feedback(TRUE)
    timer_active(FALSE)
    duration <- as.integer(Sys.time() - start_time())
    msg <- if (!check_no_triplets(g)) {
      "❌ Il y a des triplets (000 ou 111) dans la grille."
    } else if (!check_balance(g)) {
      "❌ La répartition des 0 et 1 n'est pas équilibrée."
    } else if (!check_unique_rows_cols(g)) {
      "❌ Il y a des lignes ou colonnes identiques."
    } else if (!check_no_na(g)) {
      "⚠ Il reste des cases vides."
    } else {
      paste("✅ Grille entièrement valide, bravo ! 🎉 Temps :", duration, "secondes")
    }
    status_message(msg)
  })

  observeEvent(input$erase_errors, {
    g <- grid(); sol <- solution(); fixed <- fixed_cells()
    if (is.null(g) || is.null(sol) || is.null(fixed)) return()
    for (i in seq_len(nrow(g))) {
      for (j in seq_len(ncol(g))) {
        if (!fixed[i, j] && !is.na(g[i, j]) && g[i, j] != sol[i, j]) {
          g[i, j] <- NA
        }
      }
    }
    grid(g)
    show_feedback(FALSE)
    status_message("🧼 Erreurs effacées. Continuez à jouer !")
  })

  output$grid <- renderUI({
    g <- grid(); fixed <- fixed_cells(); sol <- solution()
    hints <- hinted_cells()
    if (is.null(g) || is.null(fixed) || is.null(sol)) {
      return(h4("⬅ Cliquez sur 🔄 Nouvelle Grille pour commencer"))
    }

    n <- nrow(g); m <- ncol(g); feedback <- show_feedback()
    grid_html <- tagList()
    for (i in 1:n) {
      row <- tagList()
      for (j in 1:m) {
        cell_id <- paste0("cell_", i, "_", j)
        val <- ifelse(is.na(g[i, j]), "", as.character(g[i, j]))
        color <- "black"
        if (fixed[i, j]) {
          color <- if (hints[i, j]) "green" else "gray"
        } else if (feedback && !is.na(g[i, j])) {
          color <- if (g[i, j] == sol[i, j]) "green" else "red"
        }
        row[[j]] <- actionButton(
          inputId = cell_id,
          label = val,
          style = paste0("width: 50px; height: 50px; margin: 2px; font-size: 18px; color:", color, ";"),
          disabled = fixed[i, j]
        )
      }
      grid_html[[i]] <- div(style = "display: flex;", row)
    }
    do.call(tagList, grid_html)
  })

  output$status <- renderText({ status_message() })

  output$victory_message <- renderUI({
    if (grepl("🎉", status_message())) {
      tags$div(
        class = "victory-overlay",
        "🎉 BRAVOOOO T'as gagné !!! 🎉"
      )
    }
  })

  output$chrono <- renderText({
    autoInvalidate()
    start <- start_time()
    if (is.null(start)) return("0 seconde")
    diff <- as.integer(Sys.time() - start)
    if (timer_active()) {
      paste(diff, "secondes")
    } else {
      if (grepl("🎉", status_message())) return("")
      paste(diff, "secondes (terminé)")
    }
  })
}

shinyApp(ui, server)
