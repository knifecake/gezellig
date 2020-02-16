
#' Tabular input
#'
#' A table which has inputs on its cells
#'
#' @param id the id to give to this instance of a table input
#'
#' @return a list of tags that can be used as part of a shiny UI definition
#'
#' @seealso \code{\link{ti}} for the server part of this module
ti_input <- function(id) {
  # create a namespace to avoid name collisions with other modules / other
  # instances of this one
  ns <- NS(inputId)

  tagList(
    ti_assets(),
    tags$div(
      tags$table(
        # the table content is build on the client through the shiny binding
        class = 'table table-bordered table-condensed'
      ),
      class = 'table-responsive settings-table',
      id = ns('settingsTable')
    )
  )
}

#' Table input (server function)
#'
#' @param input the shiny \code{input} object
#' @param output the shiny \code{output} object
#' @param session the shiny \code{session} object
#' @param id the id given to the input
#' @param fields a list of fields (see \code{\link{ti_inputs}}) to include on
#'   each row
#'
#' @seealso \code{\link{ti_input}} for the UI function and
#'   \code{\link{update_ti}} for updating the ti on the fly (similarly to
#'   \code{updateSelectInput} for instance).
ti = function(input, output, session, id, fields) {
  session$sendInputMessage(id, list('fields' = fields, 'data' = list()))

  return(reactive({
    if (isTruthy(input$settingsTable))
      jsonlite::fromJSON(input$settingsTable)
    else
      NULL
  }))
}

#' Update a table input
#'
#' @param session the shiny session object
#' @param id the id of the table input to update
#' @param fields a list of the fields present in each row
#' @param data a matrix representing the state of the fields
#'
#' @export
#' @seealso \code{\link{ti}}
update_ti = function(session, id = 'namespace', fields, data) {
  ns = NS(id)

  session$sendInputMessage(ns('settingsTable'),
                           jsonlite::toJSON(list('fields' = fields, 'data' = data),
                                            auto_unbox = F))
}

#' Inputs for table input (ti)
#'
#' @param name the input name
#' @param options a list of options for radio and dropdown fields matching the
#'   format of the parameter \code{choices} in \code{\link{radioButtons}}.
#'
#' @name ti_inputs

#' @rdname ti_inputs
#' @export
ti_label <- function(name) {
  list('name' = name,
       'type' = 'label')
}

#' @rdname ti_inputs
#' @export
ti_checkbox = function(name) {
  list('name' = name,
       'type' = 'checkbox')
}

#' @rdname ti_inputs
#' @export
ti_dropdown = function(name, options) {
  list('name' = name,
       'type' = 'dropdown',
       'options' = hash2json(options))
}

#' @rdname ti_inputs
#' @export
ti_radio = function(name, options) {
  list('name' = name,
       'type' = 'radio',
       'options' = hash2json(options))
}

#' Table input asset manifest
#'
#' An \code{\link{htmlDependency}} object describing the assets (JS and CSS
#' files) needed for this module to run.
ti_assets <- function() {
  htmlDependency(name = "ti-assets", version = "0.1",
                 package = "gezellig",
                 src = "assets",
                 script = "js/ti-binding.js",
                 stylesheet = c("css/ti-style.css")
  )
}

#' Hash to JSON converter
#'
#' Turns a (possibly nested) named list into a \code{\link{data.frame}} ready to
#' be converted into JSON by \code{\link{toJSON}}.
#'
#' @param hash a (possibly nested) named list
#' @return a data.frame that behaves as expected with \code{jsonlite::toJSON}.
hash2json <- function(hash) {
  values <- names(hash)
  labels <- unname(hash)

  as.data.frame(list('value' = values, 'label' = labels))
}
