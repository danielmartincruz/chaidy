#' @import data.table
#' @importFrom googlesheets4 read_sheet 
data_formation <- function(link_excel) {
  # path_json <- file.path(getwd(), "gs4.json")
  # options(gargle_verbosity = "debug")
  list.files(".secrets/")
  # futile.logger::flog.info("Connecting to gs")
  # googlesheets4::gs4_auth(path = path_json)

  # futile.logger::flog.info("Connected to gs!")

  googlesheets4::gs4_auth(cache = ".secrets", email = "nadrieci@gmail.com")
  
  dt <- googlesheets4::read_sheet(link_excel, sheet = "Raw")
  setDT(dt)
  dt <- dt[, id := .I]

  setcolorder(dt, "id")
  
  dt <- remove_list_columns_dt(dt)
  # dt <- as.data.table(iris)
  # dt[, ejercicio := "tipo"]
  # dt[, id := .I]
  return(list(dt))
}
