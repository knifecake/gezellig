#' Demo launcher
#'
#' @export

start_demo <- function() {
  shiny::runApp(system.file('gezellig_demos', package='gezellig'))
}
