.onLoad <- function(...) {
  shiny::addResourcePath('www', system.file('www', package = 'demoApp'))
  shiny::addResourcePath('data', system.file('data', package = 'demoApp'))
  allzips <- demoApp::getZipcode()
  zipdata <- allzips[sample.int(nrow(allzips), 10000),]
  # By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
  # will be drawn last and thus be easier to see
  zipdata <- zipdata[order(zipdata$centile),]
}