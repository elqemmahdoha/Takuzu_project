library(shiny)
library(shinyjs)
library(bslib)
library(takuzuu)  # Votre bibliothèque contenant generate_grid() et check_*

ui <- fluidPage(
<<<<<<< HEAD
  useShinyjs(),  # Activation de shinyjs
  theme = bs_theme(bootswatch = "flatly"),
  
  # Ajout du style CSS pour des animations fluides
=======
  useShinyjs(),  
  theme = bs_theme(bootswatch = "flatly"),
>>>>>>> 16e1d3c8ed2d96c87946ec3019af9b7d0c9f3044
  tags$style(HTML("
    .action-button {
      transition: background-color 0.2s ease-in-out;
    }
  ")),
  
  titlePanel("🧠 Jeu Takuzu"),
  
  card(
    full_screen = TRUE,
    height = "auto",
    card_header("🎮 Plateau de jeu interactif"),
    
    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        title = "Actions",
        selectInput("grid_size", "Taille de la grille", choices = c("4x4" = 4, "6x6" = 6, "8x8" = 8), selected = 8),
        actionButton("regen", "🔄 Nouvelle Grille", class = "btn-primary"),
        actionButton("validate", "✅ Valider la Grille", class = "btn-success"),
        textOutput("status"),
        actionButton("choose_0", "Choisir 0", class = "btn-info"),
        actionButton("choose_1", "Choisir 1", class = "btn-info")
      ),
      
      uiOutput("grid")
    )
  ),
  
  navset_card_tab(
    title = "📘 Informations sur le jeu Takuzu",
    height = "auto",
    full_screen = TRUE,
    
    nav_panel(
      "📜 Règles du jeu",
      card_body(
        tags$ul(
          tags$li("La grille contient uniquement des 0 et des 1."),
          tags$li("Pas plus de deux 0 ou deux 1 consécutifs."),
          tags$li("Chaque ligne et chaque colonne contient autant de 0 que de 1."),
          tags$li("Aucune ligne ou colonne ne peut être identique à une autre.")
        )
      )
    ),
    
    nav_panel(
      "🎯 Stratégies pour gagner",
      card_body(
        tags$ul(
          tags$li(span("🧠 Détecter les triples : ", style = "font-weight: bold;"),
                  "si deux 0 ou deux 1 se suivent, la case suivante doit contenir l’autre chiffre."),
          tags$li(span("⚖ Équilibrer les 0 et les 1 : ", style = "font-weight: bold;"),
                  "une ligne ou colonne ne peut pas contenir plus de la moitié du même chiffre."),
          tags$li(span("🔍 Comparer les lignes et colonnes complètes : ", style = "font-weight: bold;"),
                  "ajustez les chiffres pour éviter les doublons.")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  grid <- reactiveVal(NULL)
  fixed_cells <- reactiveVal(NULL)  # Mémorise les cases fixes
<<<<<<< HEAD
  status_message <- reactiveVal("Sélectionnez une taille de grille pour commencer.")
  
  # Générer une nouvelle grille
=======
  selected_value <- reactiveVal(NULL)  
  status_message <- reactiveVal("Sélectionnez une taille de grille pour commencer.")
  
>>>>>>> 16e1d3c8ed2d96c87946ec3019af9b7d0c9f3044
  generate_new_grid <- function() {
    selected_size <- as.numeric(input$grid_size)
    filled <- switch(
      as.character(selected_size),
      "4" = 6,
      "6" = 12,
      "8" = 20
    )
    new_grid <- generate_grid(size = selected_size, filled_cases = filled)
    grid(new_grid)
<<<<<<< HEAD
    fixed_cells(!is.na(new_grid))  # TRUE si la case est fixée
    status_message("Nouvelle grille générée.")
  }
  
  # Actions pour la sélection de taille et régénération de la grille
=======
    fixed_cells(!is.na(new_grid))  
    status_message("Nouvelle grille générée.")
  }
  
>>>>>>> 16e1d3c8ed2d96c87946ec3019af9b7d0c9f3044
  observeEvent(input$grid_size, {
    generate_new_grid()
  })
  
  observeEvent(input$regen, {
    generate_new_grid()
  })
  
<<<<<<< HEAD
  # Interaction avec les cases modifiables via shinyjs
  observe({
    g <- grid()
    fixed <- fixed_cells()
    if (is.null(g) || is.null(fixed)) return()
=======
  observeEvent(input$choose_0, {
    selected_value(0)
  })
  
  observeEvent(input$choose_1, {
    selected_value(1)
  })

  observe({
    g <- grid()
    fixed <- fixed_cells()
    selected <- selected_value()
    if (is.null(g) || is.null(fixed) || is.null(selected)) return()
>>>>>>> 16e1d3c8ed2d96c87946ec3019af9b7d0c9f3044
    
    n <- nrow(g)
    m <- ncol(g)
    
    for (i in 1:n) {
      for (j in 1:m) {
        if (fixed[i, j]) next  # Ignorer les cases fixes
        
        local({
          row <- i
          col <- j
          cell_id <- paste0("cell_", row, "_", col)
          
          observeEvent(input[[cell_id]], {
            current_grid <- isolate(grid())
<<<<<<< HEAD
            current_value <- current_grid[row, col]
            
            # Cycle entre NA -> 0 -> 1 -> NA pour UNE seule cellule
            new_value <- if (is.na(current_value)) 0 else if (current_value == 0) 1 else NA
            
            # Mettre à jour UNIQUEMENT la cellule cliquée dans la grille
            current_grid[row, col] <- new_value
            grid(current_grid)
            
            # Mettre à jour le bouton visuellement via shinyjs
            shinyjs::runjs(sprintf("
              $('#%s').text('%s').css('background-color', '%s');
            ", cell_id, ifelse(is.na(new_value), "", new_value),
                                   ifelse(is.na(new_value), "white", ifelse(new_value == 0, "lightblue", "lightgreen"))))
=======
            current_grid[row, col] <- selected  
            grid(current_grid) 
            shinyjs::runjs(sprintf("
              $('#%s').text('%s').css('background-color', 'lightblue');
            ", cell_id, selected))
>>>>>>> 16e1d3c8ed2d96c87946ec3019af9b7d0c9f3044
          }, ignoreInit = TRUE)
        })
      }
    }
  })
  
<<<<<<< HEAD
  # Affichage de la grille
=======
>>>>>>> 16e1d3c8ed2d96c87946ec3019af9b7d0c9f3044
  output$grid <- renderUI({
    g <- grid()
    fixed <- fixed_cells()
    
    if (is.null(g) || is.null(fixed)) {
      return(h4("⬅ Choisissez une taille de grille pour commencer"))
    }
    
    n <- nrow(g)
    m <- ncol(g)
    grid_html <- tagList()
    
    for (i in 1:n) {
      row <- tagList()
      for (j in 1:m) {
        cell_id <- paste0("cell_", i, "_", j)
        cell_value <- ifelse(is.na(g[i, j]), "", as.character(g[i, j]))
        
        row[[j]] <- actionButton(
          inputId = cell_id,
          label = cell_value,
          style = "width: 50px; height: 50px; margin: 2px; font-size: 18px; text-align: center;",
          disabled = fixed[i, j]
        )
      }
      grid_html[[i]] <- div(style = "display: flex;", row)
    }
    
    do.call(tagList, grid_html)
  })
  
<<<<<<< HEAD
  # Validation de la grille
=======
  
>>>>>>> 16e1d3c8ed2d96c87946ec3019af9b7d0c9f3044
  observeEvent(input$validate, {
    g <- grid()
    if (is.null(g)) {
      status_message("❗ Veuillez d'abord générer une grille.")
      return()
    }
    
    msg <- if (!check_no_triplets(g)) {
      "❌ Il y a des triplets (000 ou 111) dans la grille."
    } else if (!check_balance(g)) {
      "❌ La répartition des 0 et 1 n'est pas équilibrée."
    } else if (!check_unique_rows_cols(g)) {
      "❌ Il y a des lignes ou colonnes identiques."
    } else if (!check_no_na(g)) {
      "⚠ Il reste des cases vides."
    } else {
      "✅ Grille entièrement valide, bravo ! 🎉"
    }
    
    status_message(msg)
  })
  
  output$status <- renderText({
    status_message()
  })
}

shinyApp(ui, server)
<<<<<<< HEAD
=======


>>>>>>> 16e1d3c8ed2d96c87946ec3019af9b7d0c9f3044
