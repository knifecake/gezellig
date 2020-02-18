library(shiny)
library(gezellig)

ui <- fluidPage(
   titlePanel("Gezellig demos"),
   tabsetPanel(
     # Tabular loader
     tabPanel(
       "Tabular loader",
       wellPanel(
         tabular_data_loader_input("tabular", ncols = 3)
       ),

       h3("Preview"),
       div(
         tableOutput("tabular_output"),
         class = "table-responsive"
       )
     ),

     # Table input (ti)
     tabPanel("Table input",
       ti_input("table_input")
     )
   )
)


server <- function(input, output, session) {
  # Tabular loader
  df <- callModule(tabular_data_loader, "tabular")

  output$tabular_output <- renderTable(df(), rownames = TRUE)

  # Table input
  tb <- callModule(ti, "table_input",
                   fields = ti_fields,
                   data = ti_data)
}

ti_fields <- list(ti_label("Task"),
                  ti_checkbox("Done"),
                  ti_dropdown("Priority", c("normal" = "Normal",
                                            "high" = "High")),
                  ti_text("Contents"))

ti_data <- data.frame(Task = c("Clean up the house", "Walk the dog", "Groceries", "P = NP?"),
                      Done = c(TRUE, FALSE, FALSE, FALSE),
                      Priority = c("normal", "high", "normal", "high"),
                      Contents = c("Toilets done last week", "Potty training: 90%", "Cheese and bread", "Iteration #1928832"))

shinyApp(ui = ui, server = server)
