library(shiny)
library(gezellig)

ui <- fluidPage(
   titlePanel("Gezellig demos"),
   tabsetPanel(
     tabPanel("Tabular loader",
       tabular_data_loader_input("tabular"),
       tableOutput("tabular_output")
     )
   )
)


server <- function(input, output, session) {
  df <- callModule(tabular_data_loader, "tabular")

  output$tabular_output <- renderTable(df(), rownames = TRUE)
}

shinyApp(ui = ui, server = server)

