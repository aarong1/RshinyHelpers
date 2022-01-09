#' Colors the selected row in a DT within a shiny app.  Call in the ui
#' element of a shiny app
#' @param colour A character vector of rgb,rgba,colour name
#' 
#' @return A character vector of html style containing the relevant css
#' @examples
#' DT_selected_row_colour()
#' DT_selected_row_colour(colour='blue')
#' @export
DT_selected_row_colour <- function( colour='pink') {
  
  shiny::tags$style(
    htmltools::HTML(paste0('table.dataTable tr.selected td, table.dataTable td.selected {
                    background-color: ',colour,' !important;}')))
  
}
