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
DT_selected_row_colour <- function(colour = 'pink') {
  shiny::tagList(
    shiny::tags$style(
      htmltools::HTML(
    paste0(
      'table.dataTable tr.selected td, table.dataTable td.selected {
                    background-color: ',
      colour,
      ' !important;}
      table.dataTable tr.active td, table.dataTable td.active {
                    background-color: ',
      colour,
      ' !important;}
      '
    )
  ))
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
DT_hover_row_colour <- function(colour = 'pink') {
  shiny::tags$style(htmltools::HTML(
    paste0(
      'table.dataTable.hover tbody tr:hover, table.dataTable.display tbody tr:hover {
                                  background-color: ',
      colour,
      ' !important;}'
    )
  ))
  
}
#' Colors the colour of the peripheral element in a DT within a shiny app.  Call in the ui
#' element of a shiny app
#' @param colour A character vector of rgb,rgba,colour name
#'
#' @return A character vector of html style tag containing the relevant css
#' @examples
#' DT_peripheral_colour()
#' DT_peripheral_colour(colour='blue')
#' @export
DT_peripheral_colour <- function(colour = 'pink') {
  shiny::tags$style(htmltools::HTML(
    paste0(
      ".page-item.active .page-link{
    z-index: 3;
    color: #fff;
    background-color: ",colour,";
    border-color: ",colour,";
}
      .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing,.dataTables_wrapper .dataTables_paginate .paginate_button, .dataTables_wrapper .dataTables_paginate .paginate_button.disabled {
          color: ",
      colour,
      " !important;
      }
      
    .page-item .page-link {
    z-index: 3;
    color: rgb(255, 255, 255);
    background-color:" ,'transparent',";
    border-color: transparent;
    color:",colour,";
      
    "
  #ff003b
    )
  ))
}
