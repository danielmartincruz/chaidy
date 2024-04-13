# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

#
options(encoding = "UTF-8")


pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options( "golem.app.prod" = TRUE)
link_excel <- Sys.getenv("LINK_EXCEL")
link_excel <- "https://docs.google.com/spreadsheets/d/1ABE1eIsWzC_koKSSarSQ4mBZM-c8_lAb5hgwN3qxIxM/edit#gid=0"
link_excel <- "https://docs.google.com/spreadsheets/d/1-DMw55R69bPJGeUHTkmcwDzSa-BL5JcxNwXiI3eMQGc/edit#gid=328046223"
chaidy::run_app(link_excel) # add parameters here (if any)
# rsconnect::deployApp('/Users/danielmartincruz/Documents/chaidy/glossainy')
