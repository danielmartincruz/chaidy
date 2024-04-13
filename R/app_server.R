#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here

}

#' @description Factory function for create shiny 'server' functions.
#' This factory enables passing data to server functions without changing the 
#' server() function signature Shiny expects ( like server(input, output, session) )
#' 
#' @param data Any data that will be used inside the shiny server function
#' 
#' @return A shiny server function.
#' 
#' @noRd
app_server_factory <- function(data) {
  
  new_app_server <- function( input, output, session) {
    callModule(mod_tabla_base_server, "tabula_ui", data[[1]])

  }
  
  return(new_app_server)
}