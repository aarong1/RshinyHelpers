#' Colors the selected row in a DT within a shiny app.  Call in the ui
#' element of a shiny app
#' @param colour A character vector of rgb,rgba,colour name
#' 
#' @return A character vector of html style containing the relevant css
#' @examples
#' DT_selected_row_colour()
#' DT_selected_row_colour(colour='blue')
#' @import htmltools
#' @import shiny
#' @export
DT_selected_row_colour <- function( colour='pink') {
  
  shiny::tagList(
  shiny::tags$style(
    htmltools::HTML(paste0('table.dataTable tr.selected td, table.dataTable td.selected {
                    background-color: ',colour,' !important;}'))),
  
  shiny::tags$style(
      htmltools::HTML(paste0(
        ".dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing,.dataTables_wrapper .dataTables_paginate .paginate_button, .dataTables_wrapper .dataTables_paginate .paginate_button.disabled {
            color: ",colour," !important;
        }"
      )
    )
  )
  )
  
}

#' Colors the hover row in a DT within a shiny app.  Call in the ui
#' element of a shiny app
#' @param colour A character vector of rgb,rgba,colour name
#' 
#' @return A character vector of html style containing the relevant css
#' @examples
#' DT_hover_row_colour()
#' DT_hover_row_colour(colour='blue')
#' @export
DT_hover_row_colour <- function(colour = 'pink'){
  shiny::tags$style(
      htmltools::HTML(paste0(
        'table.dataTable.hover tbody tr:hover, table.dataTable.display tbody tr:hover {
                                  background-color: ',colour,' !important;}'
      )
  )
  )
      
      }
