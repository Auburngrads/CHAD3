#' Runs the COVID19 App
#' @export
#' @importFrom shiny shinyAppDir
covid19_app <- function(app_name = "covid19", 
                        width = "100%", 
                        height = "800px", 
                        launch.browser = TRUE,...) 
{
  
  dir <- system.file("apps", app_name, package = "CHAD3")
  
  shiny::shinyAppDir(appDir = dir, 
                     options = list(height = height, 
                                    width = width, 
                                    launch.browser = launch.browser, ...))
  
}
