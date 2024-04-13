#' Run the Shiny Application
#'
#' @param ... A series of options to be used inside the app.
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  link_excel = NULL,
  ...
) {
  
  if(is.null(link_excel) || link_excel == "") {
    link_excel <- '/Users/danielmartincruz/Documents/chaidy/data-raw'
  }
  
  data_list <- data_formation(link_excel)

  server_func <- app_server_factory(data_list)
  
  ui <- app_ui(NULL, data_list[[1]])
  
  with_golem_options(
    app = shinyApp(
      ui = ui, 
      server = server_func
    ), 
    golem_opts = list(...)
  )
}
