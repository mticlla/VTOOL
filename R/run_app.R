#' Run the Shiny Application
#'
#' @export
#' @importFrom shiny shinyApp
run_app <- function(){
  shinyApp(ui = ui, server = server)
}
