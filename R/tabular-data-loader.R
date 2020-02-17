#' Tabular data loader input
#'
#' A set of inputs that allows loading tabular data into an R
#' \code{\link{data.frame}}.
#'
#' For any of the parameters, \code{NULL} may be passed and in that case the
#' particular control will by hidden from the user and the default value in
#' \code{\link{read.table}} will be used. For boolean parameters like
#' \code{allow_transposition} a non-\code{NULL} value will be used as the
#' default for that control. For the rest of parameters, a list of options must
#' be provided per the instructions in the \code{choices} parameter description
#' in \code{link{radioButtons}} or the like. In a list of choices, the first one
#' is the default value.
#'
#' @param id an identifier unique in the namespace
#' @param file_label the label for the file input
#' @param allowed_field_separators the characters that may be used to separate
#'   fields within a row
#' @param allowed_field_delimiters the characters that may be used for field
#'   quoting (see the parameter \code{quote} in \code{\link{read.table}})
#' @param allow_header_toggle whether to give the option to treat the first row
#'   as a header
#' @param allow_rownames_toggle wether to give the option to treat the first
#'   column as a header
#' @param na_string the string to be used for NA values
#' @param allowed_decimal_separators the characters that may be used to separate
#'   the integral and fractional parts in a number
#' @param allow_transposition whether to give the option to transpose the table
#'
#' @return a list of tags that can be included in a shiny UI definition
#'
#' @seealso \code{\link{tabular_data_loader}} for the server function
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#'   ui <- fluidPage(
#'     tabular_data_loader_input("tabular"),
#'     tableOutput("tabular_output")
#'   )
#'   server <- function(input, output) {
#'     df <- callModule(tabular_data_loader, "tabular")
#'     output$tabular_output <- renderTable(df(), rownames = TRUE)
#'   }
#'   shinyApp(ui, server)
#' }
#'
#' @export
tabular_data_loader_input <-
  function(id,
           file_label = "Select file",
           allowed_field_separators = c("TAB" = "\t",
                                        "Space" = " ",
                                        "Comma" = ",",
                                        "Semicolon" = ";"),
           allowed_field_delimiters = c("Double quote" = "\"",
                                        "Single quote" = "'",
                                        "None" = ""),
           allow_header_toggle = FALSE,
           allow_rownames_toggle = FALSE,
           na_string = "NA",
           allowed_decimal_separators = c("Dot" = ".",
                                          "Comma" = ","),
           allow_transposition = FALSE) {
  ns <- NS(id)

  tagList(
    fileInput(ns("file"), file_label),
    if (!is.null(allowed_field_separators))
      radioButtons(ns("sep"),
                   "Field separator",
                   choices = allowed_field_separators),
    if (!is.null(allowed_field_delimiters))
      radioButtons(ns("quote"),
                   "Field delimiter",
                   choices = allowed_field_delimiters),
    if (!is.null(allow_header_toggle))
      checkboxInput(ns("header"),
                    "First row is header",
                    value = allow_header_toggle),
    if (!is.null(allow_rownames_toggle))
      checkboxInput(ns("rownames"),
                    "First column is header",
                    value = allow_rownames_toggle),
    if (!is.null(allowed_decimal_separators))
      radioButtons(ns("dec"),
                   "Decimal separator",
                   choices = allowed_decimal_separators),
    if (!is.null(na_string))
      textInput(ns("na_string"),
                "NA String",
                value = na_string),
    if (!is.null(allow_transposition))
      checkboxInput(ns("transpose"),
                    "Transpose table",
                    value = allow_transposition),

    # dummy tag to avoid a syntax error
    tags$div()
  )
}

#' Tabular data loader
#'
#' @param input the shiny \code{input} object
#' @param output the shiny \code{output} object
#' @param session the shiny \code{session} object
#' @param id the id given to the input
#'
#' @return a \code{\link{data.frame}} wrapped inside a reactive context
#'
#' @seealso \code{\link{tabular_data_loader_input}} for the input function and
#'   an example
#'
#' @export
tabular_data_loader <- function(input, output, session, id) {
  reactive({
    req(input$file)

    df <- read.table(input$file$datapath,
                     header = input$header,
                     sep = input$sep,
                     quote = input$quote,
                     na.strings = input$na_string,
                     row.names = if (input$rownames) 1 else NULL)

    if (isTruthy(input$transpose)) t(df) else df
  })
}
