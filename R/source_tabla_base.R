filter_jap_table <- function(data, columnas_filter, ejercicio_filter){
  if(is.null(data)){stop("No data")}
  if(is.null(columnas_filter)){stop("No columns selected")}

  columnas_filter <- c("id",columnas_filter)
  if(is.null(ejercicio_filter)){ejercicio_filter <- unique(data$ejercicio)}

  data <- data %>%
    .[ejercicio %in% ejercicio_filter] %>%
    .[,..columnas_filter] 
  
  return(data)
}
