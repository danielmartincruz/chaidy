get_datatable <- function(temp_data, columns_2_hide){
  columns_2_hide <- which(names(temp_data) %in% columns_2_hide)
  final_table <- temp_data %>%
    datatable(
      extensions = c('Buttons'),
      options = list(dom = 'Bfrtip',
                     searching = TRUE,
                     ordering = TRUE,
                     buttons = c('copy', 'csv', 'excel', 'pdf'),
                     # scrollX = TRUE, scrollY = TRUE,
                     pageLength = 15, info = FALSE,
                     lengthMenu = list(c(5,15, -1), c("5","15", "All")),
                     class = 'white-space: nowrap',
                     columnDefs = list(list(visible=FALSE, targets=columns_2_hide)),
                     initComplete = JS(c(
                       "function(settings){",
                       "  var table = settings.oInstance.api();",
                       "  var ncols = table.columns().count();",
                       "  var nrows = table.rows().count();",
                       "  for(var i=0; i<nrows; i++){",
                       "    var rowname = table.cell(i,1).data();",
                       "    for(var j=1; j<ncols; j++){",
                       "      var headerName = table.column(j).header().innerText;",
                       "      var cell = table.cell(i,j);",
                       "      cell.node().setAttribute('title', rowname + ', ' + headerName);",
                       "    }",
                       "  }",
                       "}"))
                     # fixedColumns = list(leftColumns = 3),
                     # autoWidth = FALSE,
                     # columnDefs = list(list(visible=FALSE, targets=1))
      ),
      
      selection=list(mode="single", target="cell"), class = 'cell-border stripe', escape = F,
      filter = list(
        position = 'top', clear = FALSE
      )
    )
  
  return(final_table)
}

remove_list_columns_dt <- function(dt){
  
  cols_to_convert <- which(sapply(dt, class) == "list")
  
  dt[, (cols_to_convert) := lapply(.SD, function(lst_col) {
    sapply(lst_col, function(lst_elem) {
      paste(lst_elem, collapse = ",")
    })
  }), .SDcols = cols_to_convert]
  
}

sort_custom_numeric_character <- function(dt, column_2_split) {
  all_items <- unlist(strsplit(dt[[column_2_split]], split = ";"))
  
  # Extract and sort numbers
  numbers <- unique(as.numeric(all_items[!is.na(as.numeric(all_items))]))
  numbers <- sort(numbers)
  
  # Extract and sort strings
  strings <- unique(all_items[is.na(as.numeric(all_items))])
  strings <- sort(strings)
  
  # Combine them
  result_vector <- c(strings, as.character(numbers))
  
}
