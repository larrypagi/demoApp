#' run_app
#'
#' @return NA
#' @export
#'

run_app <- function() {
  shiny::shinyApp(ui = demoApp::app_ui(), server = demoApp::app_server)
}