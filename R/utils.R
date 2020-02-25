#' Display content in multiple columns automatically
#'
#' This function uses CSS to spread content accross multiple columns. It
#' provides a more automatic solution as opposed to manually defining columns
#' using Shiny's \code{\link[shiny]{column}} function.
#'
#' @param ncols number of columns
#' @param ... content to spread
#' @param unit_element a CSS selector (e.g. a tag name or a class) that
#'   specifies the elements that you want to spread over columns. This is used
#'   try to not break them accross multiple columns but rather keep them
#'   together in the same one.
#'
#' @export
multicolumn <- function(ncols, ..., unit_element = NULL) {
  shiny::tags$div(
    multicolumn_css(ncols, unit_element),
    ...,
    class = "multicolumn"
  )
}

multicolumn_css <- function(ncols, unit_element = NULL) {
  wrapper_css = sprintf("
.multicolumn {
  column-count: %d;
  column-gap: 1rem;
}", ncols)

  inner_css = ""

  if (!is.null(unit_element)) {
    inner_css = sprintf("
.multicolumn %s {
  break-inside: avoid;
  margin-left: 1px;
}", unit_element)
  }


  list(
    shiny::tags$head(
      shiny::tags$style(
        shiny::HTML(paste0(wrapper_css, inner_css)))))
}
