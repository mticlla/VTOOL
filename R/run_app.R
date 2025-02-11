#' Run the Shiny Application
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(onStart=NULL, options=list(), enableBookmarking=NULL){
    shinyApp(
      ui = ui,
      server = server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
    )
}
