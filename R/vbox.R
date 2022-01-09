#' builds the div for a simple yet visually effective shiny value box
#' 
#' @param label The label -  character of length one
#' @param textid The value - A character vector of length one, optionally the textOutput of a reactive value
#' if using vbox_reactive
#' @param col Bkgrd col- A character of rbg(), rgba(), or a chtml olor name to recognixed in R
#' 
#' @return A character vector of the div tree to be rendered in the ui of a  shiny app
#' @examples
#' vbox()
#' @export
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


#' builds the div for a simple yet visually effective shiny value box
#' 
#' @param label The value box header label to appear-  character of length one
#' @param textid The value - A character vector of length one, optionally the textOutput of a reactive value
#' if using vbox_reactive
#' @param col Bkgrd col- A character of rbg(), rgba(), or a chtml olor name to recognixed in R
#' 
#' @return A character vector of the div tree to be rendered in the ui of a  shiny app
#' @examples
#' vbox_reactive()
#' 
#' @export
#' 

vbox_reactive <-
  function(label = 'label',
           textid = 'textOutput_here',
           col = 'rgba(60,130,180,0.5)') {
    shiny::tags$div(
      style =paste0(
        "border-radius: 15px;
                      border-style: solid;
                      border-color:white;
            padding:0px;
            margin:0px 10px;
            width:200px;
            color:white;
                  font-weight: 300;
                  text-color:blue;
                  text-align:center;
                  background-color:",col,";"),
      shiny::tags$h5(label, style = 'color:white;'),
      shiny::tags$h2(style = 'font-weight:bold;display: inline-block;vertical-align:top;color:white;',
        shiny::textOutput(textid))
    )
  }