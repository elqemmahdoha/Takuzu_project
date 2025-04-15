library(shiny)
library(bslib)
library(takuzuu)

#Interface utilisateur
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),  # Un thème sympa pour l'apparence
  titlePanel("🧠 Jeu Takuzu"),

  card(
    full_screen = TRUE,
    height = "auto",
    card_header("🎮 Plateau de jeu interactif"),

    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        title = "Actions",
        #Choix de la taille de la grille
        selectInput("grid_size", "Taille de la grille", choices = c("4x4" = 4, "6x6" = 6, "8x8" = 8), selected = 6),
        #Bouton pour générer une nouvelle grille
        actionButton("regen", "🔄 Nouvelle Grille", class = "btn-primary"),
        #Bouton pour réinitialiser la grille actuelle
        actionButton("reset", "♻ Réinitialiser", class = "btn-secondary"),
        #Bouton pour valider la grille
        actionButton("validate", "✅ Valider la Grille", class = "btn-success"),
        #Afficher la solution
        actionButton("show_solution", "🧩 Afficher la solution", class = "btn-warning"),
        #Choix de la valeur à insérer
        actionButton("choose_0", "Choisir 0", class = "btn-info"),
        actionButton("choose_1", "Choisir 1", class = "btn-info"),
        #Chronomètre
        h4("⏱ Temps écoulé :"),
        textOutput("chrono"),
        #Messages d'état du jeu
        textOutput("status")
      ),
      #Affichage de la grille
      uiOutput("grid")
    )
  )
)

#Serveur
server <- function(input, output, session) {
  #Variables réactives pour stocker la grille, la solution, les cases fixes, etc.
  grid <- reactiveVal(NULL)
  grid_original <- reactiveVal(NULL)
  solution <- reactiveVal(NULL)
  fixed_cells <- reactiveVal(NULL)
  selected_value <- reactiveVal(NULL)
  status_message <- reactiveVal("Cliquez sur 🔄 Nouvelle Grille pour commencer")
  start_time <- reactiveVal(NULL)
  timer_active <- reactiveVal(FALSE)
  autoInvalidate <- reactiveTimer(1000)  # Mise à jour toutes les secondes pour le chrono

  #Générer une grille automatiquement au démarrage si vide
  observe({
    if (is.null(grid())) {
      generate_new_grid()
    }
  })

  #Fonction pour générer une nouvelle grille selon la taille choisie
  generate_new_grid <- function() {
    taille <- as.numeric(input$grid_size)
    proportion <- switch(
      as.character(taille),
      "4" = 0.6,
      "6" = 0.4,
      "8" = 0.35
    )
    jeu <- generer_takuzu_jouable(taille, proportion_visible = proportion)
    grid(jeu$grille_visible)
    grid_original(jeu$grille_visible)
    solution(jeu$solution)
    fixed_cells(!is.na(jeu$grille_visible))
    status_message("✅ Nouvelle grille générée.")
    start_time(Sys.time())  # Démarrer le chrono
    timer_active(TRUE)
  }

  #Génère une nouvelle grille lorsqu'on change la taille ou clique sur "Nouvelle Grille"
  observeEvent(input$grid_size, generate_new_grid)
  observeEvent(input$regen, generate_new_grid)

  #Bouton pour réinitialiser la grille à l'état de départ
  observeEvent(input$reset, {
    if (!is.null(grid_original())) {
      grid(grid_original())
      fixed_cells(!is.na(grid_original()))
      status_message("♻ Grille réinitialisée.")
      start_time(Sys.time())
      timer_active(TRUE)
    }
  })

  #Choix du chiffre à insérer
  observeEvent(input$choose_0, { selected_value(0) })
  observeEvent(input$choose_1, { selected_value(1) })

  #Permet de modifier la grille en cliquant sur les cases non fixes
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
            row <- i
            col <- j
            cell_id <- paste0("cell_", row, "_", col)

            observeEvent(input[[cell_id]], {
              req(input[[cell_id]])  # S'assurer que le bouton existe
              current_grid <- isolate(grid())
              current_grid[row, col] <- selected
              grid(current_grid)
            }, ignoreInit = TRUE)
          })
        }
      }
    })
  })

  #Affiche la solution complète de la grille
  observeEvent(input$show_solution, {
    sol <- solution()
    if (is.null(sol)) {
      status_message("❗ Solution introuvable.")
      return()
    }
    grid(sol)
    fixed_cells(matrix(TRUE, nrow = nrow(sol), ncol = ncol(sol)))  # Bloquer toute la grille
    status_message("✅ Solution affichée.")
    timer_active(FALSE)
  })

  #Génération de l'affichage visuel de la grille dans l'UI
  output$grid <- renderUI({
    g <- grid()
    fixed <- fixed_cells()
    if (is.null(g) || is.null(fixed)) {
      return(h4("⬅ Cliquez sur 🔄 Nouvelle Grille pour commencer"))
    }

    n <- nrow(g)
    m <- ncol(g)
    grid_html <- tagList()

    for (i in 1:n) {
      row <- tagList()
      for (j in 1:m) {
        cell_id <- paste0("cell_", i, "_", j)
        val <- ifelse(is.na(g[i, j]), "", as.character(g[i, j]))

        row[[j]] <- actionButton(
          inputId = cell_id,
          label = val,
          style = "width: 50px; height: 50px; margin: 2px; font-size: 18px;",
          disabled = fixed[i, j]
        )
      }
      grid_html[[i]] <- div(style = "display: flex;", row)
    }

    do.call(tagList, grid_html)
  })

  #Validation de la grille avec retour de message selon le cas
  observeEvent(input$validate, {
    g <- grid()
    if (is.null(g)) {
      status_message("❗ Veuillez d'abord générer une grille.")
      return()
    }

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

  #Affichage du message d'état
  output$status <- renderText({ status_message() })

  #Affichage du chrono en temps réel
  output$chrono <- renderText({
    autoInvalidate()  # Force le rafraîchissement toutes les secondes
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

shinyApp(ui, server)
