#' @import shiny
app_ui <- function() {
  tagList(
    # Leave this function for adding external resources
    # from inst/app/www
    # List the first level UI elements here 
    fluidPage(
      h1("demoApp")
    )
  )
}

golem_add_external_resources <- function(){
  
  addResourcePath(
    'www', system.file('app/www', package = 'demoApp')
  )
 
  tagList(
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    #tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}