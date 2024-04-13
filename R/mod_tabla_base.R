#' tabla_base UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#'
#' @import DT
#' @import magrittr
#' @import shinydashboard

#' @importFrom shiny NS tagList 
mod_tabla_base_ui <- function(id, exercise_table){
  ns <- NS(id)
  tagList(
 
    fluidRow(
      column(width = 9,
             shinydashboard::box(title = "chaidy", collapsible = TRUE, width = NULL, solidHeader = FALSE,
                     DT::DTOutput(outputId = ns("exercise_table"))
             )
      ),
      column(width = 3,
             shinydashboard::box(title = "Filters", width = NULL, solidHeader = TRUE, status = "warning",
                 selectizeInput(inputId = ns("ejercicio_filter") ,
                             label = "ejercicio", choices = unique(exercise_table$ejercicio), selected = NULL, multiple = TRUE
                            ),
                 
                 checkboxGroupInput(
                   inputId = ns("columnas_filter"),
                   label = "Columnas mostradas", choices = unique(names(exercise_table))[unique(names(exercise_table)) != "id"],
                   selected = unique(names(exercise_table))[unique(names(exercise_table)) != "id"]
                 )
                 

                 
             )
      )
      )
    
    
    

  )
}
    
#' tabla_base Server Function
#'
#' @noRd 
mod_tabla_base_server <- function(input, output, session, data){
  ns <- session$ns
  filtered_exercise_table <- reactive({
    data <- filter_jap_table(data, columnas_filter = input$columnas_filter,
                             ejercicio_filter = input$ejercicio_filter)
    return(data)
  })
  
  output$exercise_table <- DT::renderDT({
    temp_data <- filtered_exercise_table()

    columns_2_hide <- c("id")
    final_table <- get_datatable(temp_data, columns_2_hide) 
      
    return(final_table)
  })
  
  observeEvent(input$exercise_table_cell_clicked, {
    req(length(input$exercise_table_cell_clicked) != 0) #Event must not being executed unless a cell is clicked
    removeModal()
    showModal(modal())
    
  })
  
  modal <- function() {
    ns <- session$ns

    modalDialog(
      p("Detailed table", style = "font-size:25px"),
      DT::dataTableOutput(ns("cell_click_table")),
      size = "l"
    )
    
  }
  
  output$cell_click_table <- DT::renderDataTable({
    
    test_res <- filtered_exercise_table()
    row <- input$exercise_table_cell_clicked$row
    col <- input$exercise_table_cell_clicked$col
    id_clicked <- test_res[row]$id
    col_clicked <- names(test_res[, col, with = F])
    data_clicked <- copy(data) %>%
      .[id == id_clicked] %>%
      .[, id := NULL]
    setcolorder(data_clicked, col_clicked)
    DT::datatable(data_clicked)
    
  })
 
}
    
## To be copied in the UI
# mod_tabla_base_ui("tabla_base_ui_1")
    
## To be copied in the server
# callModule(mod_tabla_base_server, "tabla_base_ui_1")
 
