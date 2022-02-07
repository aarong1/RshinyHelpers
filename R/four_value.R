#' builds the div for a more complicated multiple grouped text-value
#' boxes
#' @param text The labels -  character of length four
#' @param values The value - A character vector of length four
#' @return A tagList of the div tree to be rendered in the ui of a  shiny app
#' @examples
#' text=c('BTC','ETH','DOG','KLI')
#' values=c('$3,034','$4,353','$496','$629')
#' four_value(text,values)
#' @export
four_value <- function(text,values) {
  shiny::tagList(
    div(
    style = 'text-align:center;font-family:sans-serif;',
    
    shiny::fillRow(
      div(
        class = 'hovernow click',
        style = 'border-style:solid;
               border-color:lightgrey;
               border-width:1px;
               border-radius:15px 0px 0px 15px;',
        
        shiny::tags$h3(value[1]),
        shiny::tags$p(text[1])
      ),
      shiny::tags$div(
        class = 'hovernow click2',
        style = 'border-style:solid;
               border-color:lightgrey;

               border-width:1px 1px 1px 0px',
        shiny::tags$h3(value[2]),
        shiny::tags$p(text[2])
      ),
      
      shiny::tags$div(
        class = 'hovernow click3',
        style = 'border-style:solid;
               border-color:lightgrey;

               border-width:1px 0px',
        shiny::tags$h3(value[3]),
        shiny::tags$p(text[3])
      ),
      shiny::tags$div(
        class = 'hovernow click4',
        style = 'border-style:solid;
               border-color:lightgrey;
               border-width:1px;

                   border-radius:0px 15px 15px 0px',
        shiny::tags$h3(value[4]),
        shiny::tags$p(text[4])
      )
      )
    )
  )
  
}
