library(shiny)
library(takuzuu)

ui <- fluidPage(
  titlePanel("Jeu Takuzu"),
  actionButton("regen", "Nouvelle Grille"),
  actionButton("validate", "Valider la Grille"),
  textOutput("status"),
  tableOutput("grid")
)

server <- function(input, output, session) {
  grid <- reactiveVal(generate_grid())
  status_message <- reactiveVal("")

  observeEvent(input$regen, {
    grid(generate_grid())
    status_message("")
  })

  observeEvent(input$validate, {
    g <- grid()

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

  output$grid <- renderTable({
    grid()
  }, rownames = TRUE)

  output$status <- renderText({
    status_message()
  })
}

shinyApp(ui, server)
