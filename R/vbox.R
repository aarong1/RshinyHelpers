#' builds the div for a simple yet visually effective shiny value box
#' 
#' @param label The label -  character of length one
#' @param textid The value - A character vector of length one, optionally the textOutput of a reactive value
#' @param col Bkgrd col- A character of rbg(), rgba(), or a color name to 
#' 
#' @return A character vector of the div tree to be rendered in the ui of a  shiny app
#' @examples
#' vbox()
#' 
vbox <-
  function(label = 'label',
           textid = 'textOutput_here',
           col = 'rgba(60,130,180,0.5)') {
    shiny::tags$div(
      style =
        "border-radius: 15px;
                      border-style: solid;
                      border-color:white;
            padding:0px;
            margin:0px 10px;
            color:white;
                  font-weight: 300;
                  text-color:blue;
                  text-align:center;
                  background-color:{col};
            ",
      shiny::tags$h5(label, style = 'color:white;'),
      shiny::tags$h2(style = 'font-weight:bold;display: inline-block;vertical-align:top;color:white;',
         textid)
    )
  }
