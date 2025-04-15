library(shiny)
library(bslib)
library(takuzuu)

ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly", base_font = font_google("Quicksand")),

  tags$head(
    tags$style(HTML("
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

      # === Sidebar gauche
      sidebar = sidebar(
        title = "🎛 Contrôles du jeu",
        class = "sidebar-controls",

        selectInput("grid_size", "📐 Taille de la grille :", choices = c("4x4" = 4, "6x6" = 6, "8x8" = 8), selected = 6),
        selectInput("difficulty", "🎯 Difficulté :", choices = c("Facile", "Moyen", "Difficile"), selected = "Moyen"),

        actionButton("regen", "🔄 Nouvelle Grille", class = "btn-primary"),
        actionButton("reset", "♻ Réinitialiser", class = "btn-secondary"),
        tags$hr(),
        actionButton("validate", "✅ Valider la Grille", class = "btn-success"),
        actionButton("erase_errors", "🧹 Effacer les erreurs", class = "btn-warning"),
        actionButton("show_solution", "🧩 Afficher la solution", class = "btn-warning"),
        tags$hr(),
        h4("📝 État du jeu :"),
        textOutput("status")
      ),

      # === Grille + chrono + boutons 0/1
      mainPanel(
        class = "text-center",

        # Chrono en haut à gauche
        div(
          class = "chrono-container",
          div(class = "chrono-box", "⏱ Temps écoulé : ", textOutput("chrono", inline = TRUE))
        ),

        # Grille dynamique
        uiOutput("grid"),

        # Boutons de sélection juste sous la grille
        div(
          class = "choose-container",
          actionButton("choose_0", "Choisir 0", class = "btn-info btn-choose"),
          actionButton("choose_1", "Choisir 1", class = "btn-info btn-choose")
        )
      )
    )
  )
)



# === SERVEUR ===
server <- function(input, output, session) {
  # Éléments réactifs pour suivre l'état du jeu
  grid <- reactiveVal(NULL)             # Grille actuelle
  grid_original <- reactiveVal(NULL)    # Grille de départ (pour reset)
  solution <- reactiveVal(NULL)         # Solution complète
  fixed_cells <- reactiveVal(NULL)      # Cases initialement figées
  selected_value <- reactiveVal(NULL)   # Valeur actuellement sélectionnée (0 ou 1)
  status_message <- reactiveVal("Cliquez sur 🔄 Nouvelle Grille pour commencer")

  # Chronomètre
  start_time <- reactiveVal(NULL)
  timer_active <- reactiveVal(FALSE)
  show_feedback <- reactiveVal(FALSE)
  autoInvalidate <- reactiveTimer(1000)  # Répète toutes les 1 sec pour le chrono

  # === Génère une nouvelle grille selon taille et difficulté choisies ===
  generate_new_grid <- function() {
    taille <- as.numeric(input$grid_size)

    # Ajuste la proportion de cases visibles selon la difficulté
    proportion <- switch(
      input$difficulty,
      "Facile" = 0.5,
      "Moyen" = 0.35,
      "Difficile" = 0.2
    )

    # Appel à la fonction de génération du package
    jeu <- generer_takuzu_jouable(taille, proportion_visible = proportion)

    # Mise à jour des variables réactives
    grid(jeu$grille_visible)
    grid_original(jeu$grille_visible)
    solution(jeu$solution)
    fixed_cells(!is.na(jeu$grille_visible))
    status_message("✅ Nouvelle grille générée.")
    start_time(Sys.time())
    timer_active(TRUE)
    show_feedback(FALSE)
  }

  # === Boutons : nouvelle grille, reset ... ===
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

  # === Interaction avec les cases modifiables ===
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
          if (fixed[i, j]) next   # Ignore les cases fixes

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

  # === Affichage de la solution ===
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

  # === Validation de la grille ===
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
        if (feedback && !fixed[i, j] && !is.na(g[i, j])) {
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
  output$chrono <- renderText({
    autoInvalidate()
    start <- start_time()
    if (is.null(start)) return("0 seconde")
    diff <- as.integer(Sys.time() - start)
    if (timer_active()) {
      paste(diff, "secondes")
    } else {
      paste(diff, "secondes (terminé)")
    }
  })
}

# === Lancement ===
shinyApp(ui, server)
