#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request, exercise_table) {

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    shinydashboardPlus::dashboardPagePlus(
      
      header = shinydashboardPlus::dashboardHeaderPlus(
        title = "chaidy",
        enable_rightsidebar = FALSE
      ),
      # Create our navigation menu that links to each of the tabs we defined
      sidebar = shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
          # Setting id makes input$tabs give the tabName of currently-selected tab
          id = "tabs",
          shinydashboard::menuItem("Japanese", tabName = "af", icon = icon("affiliatetheme"))

        )
      ),
      # Show the appropriate tab's content in the main body of our dashboard when we select it
      body = shinydashboard::dashboardBody(
        shinydashboard::tabItems(
          shinydashboard::tabItem("af", mod_tabla_base_ui("tabula_ui", exercise_table))
        )
      ),
      rightsidebar = NULL,
      title = "nihontool"
    )
    
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'chaidy'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

