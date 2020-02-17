library(shiny)
library(gezellig)

ui <- fluidPage(
   titlePanel("Gezellig demos"),
   tabsetPanel(
     # Tabular loader
     tabPanel("Tabular loader",
       tabular_data_loader_input("tabular"),
       tableOutput("tabular_output")
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

ti_fields <- list(ti_label("Name"),
                  ti_checkbox("Done"),
                  ti_dropdown("Priority", c("normal" = "Normal",
                                            "high" = "High")),
                  ti_text("Contents"))

ti_data <- data.frame(Name = c("Task 1", "Task 2"),
                      Done = c(TRUE, FALSE),
                      Priority = c("normal", "high"),
                      Contents = c("My first task", "Another task"))

shinyApp(ui = ui, server = server)
