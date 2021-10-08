#' Colors the selected row in a DT within a shiny app.  Call in the ui
#' 
#' @param colour A character vector of rgb,rgba,colour name
#' 
#' @return A character vector of html style containg the televant css
#' @examples
#' DT_selected_row_colour()
#' DT_selected_row_colour(colour='blue')
#' _


DT_selected_row_colour <- function( colour='pink') {
  
  shiny::tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {background-color: ',colour,' !important;}'))
  
}