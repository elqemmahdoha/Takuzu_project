library(shiny)
library(bslib)
library(takuzuu)  

ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel("🧠 Jeu Takuzu"),
  
  # Carte principale avec layout_sidebar()
  card(
    full_screen = TRUE,
    height = "auto",
    card_header("🎮 Plateau de jeu interactif"),
    
    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        title = "Actions",
        actionButton("regen", "🔄 Nouvelle Grille", class = "btn-primary"),
        actionButton("validate", "✅ Valider la Grille", class = "btn-success"),
        textOutput("status")
      ),
      
      # Contenu principal à droite de la sidebar
      uiOutput("grid")
    )
  ),
  
  # Carte avec les règles du jeu et stratégies
  navset_card_tab(
    title = "📘 Informations sur le jeu Takuzu",
    height = "auto",
    full_screen = TRUE,
    
    nav_panel(
      "📜 Règles du jeu",
      card_body(
        tags$ul(
          tags$li("La grille contient uniquement des 0 et des 1."),
          tags$li("Pas plus de deux 0 ou deux 1 consécutifs (horizontalement ou verticalement)."),
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
                  "si deux 0 ou deux 1 se suivent, la case suivante doit forcément contenir l’autre chiffre."),
          tags$li(span("⚖️ Équilibrer les 0 et les 1 : ", style = "font-weight: bold;"), 
                  "une ligne ou une colonne ne peut pas contenir plus de la moitié des cases d’un même chiffre."),
          tags$li(span("🔍 Comparer les lignes et colonnes complétées : ", style = "font-weight: bold;"), 
                  "si une ligne ou une colonne est presque remplie et qu’une autre est similaire, il faut ajuster les chiffres pour éviter les doublons.")
        )
      )
    )
  )
)



# Serveur
server <- function(input, output, session) {
  # Grille réactive initialisée avec une fonction de génération
  grid <- reactiveVal(generate_grid(n = 8, filled_cases = 20))
  status_message <- reactiveVal("") 
  
  # Bouton : Générer une nouvelle grille
  observeEvent(input$regen, {
    grid(generate_grid(n = 8, filled_cases = 20))  
    status_message("")  
  })
  
  # Générer une grille interactive avec des boutons
  output$grid <- renderUI({
    g <- grid()
    n <- nrow(g)
    m <- ncol(g)
    grid_html <- tagList()  
    
    for (i in 1:n) {
      row <- tagList() 
      for (j in 1:m) {
        cell_value <- ifelse(is.na(g[i, j]), "", as.character(g[i, j]))
        
        row[[j]] <- actionButton(
          inputId = paste0("cell_", i, "_", j),
          label = cell_value,
          style = "width: 50px; height: 50px; margin: 2px; text-align: center; font-size: 18px;"
        )
      }
      grid_html[[i]] <- div(style = "display: flex;", row)  
    }
    do.call(tagList, grid_html)
  })
  
  # Observer les clics sur les cellules de la grille
  observe({
    g <- grid()  
    n <- nrow(g)
    m <- ncol(g)
    
    
    
    update_cell <- function(g, i, j, session, cell_id) {
      current_value <- g[i, j]
      
      if (is.na(current_value)) {
        new_value <- 0
      } else if (current_value == 0) {
        new_value <- 1
      } else if (current_value == 1) {
        new_value <- 0
      } else {
        stop("Valeur inattendue dans g[i, j]")
      }
      
      g[i, j] <- new_value
      
      updateActionButton(
        session,
        inputId = cell_id,
        label = ifelse(is.na(g[i, j]), "", as.character(g[i, j]))
      )
      
      return(g)
    }
    
    
    
    
    for (i in 1:n) {
      for (j in 1:m) {
        cell_id <- paste0("cell_", i, "_", j)
        if (!is.null(input[[cell_id]]) && input[[cell_id]] > 0) {
          g <- update_cell(g, i, j, session, cell_id)
        }
      }
    }
    
    
    
  })
  
  # Bouton : Valider la grille
  observeEvent(input$validate, {
    g <- grid()
    
    # Les règles du jeu Takuzu
    msg <- if (!check_no_triplets(g)) {
      "❌ Il y a des triplets (000 ou 111) dans la grille."
    } else if (!check_balance(g)) {
      "❌ La répartition des 0 et 1 n'est pas équilibrée."
    } else if (!check_unique_rows_cols(g)) {
      "❌ Il y a des lignes ou colonnes identiques."
    } else if (!check_no_na(g)) {
      "⚠️ Il reste des cases vides."
    } else {
      "✅ Grille entièrement valide, bravo ! 🎉"
    }
    status_message(msg)  
  })
  
  # Message d'état de la grille
  output$status <- renderText({
    status_message()
  })
}

# Lancer l'application
shinyApp(ui, server)