#' builds the div for a simple yet visually effective shiny value box
#' 
#' @param label The label -  character of length one
#' @param textid The value - A character vector of length one, optionally the textOutput of a reactive value
#' if using vbox_reactive
#' @param col Bkgrd col- A character of rbg(), rgba(), or a chtml olor name to recognixed in R
#' 
#' @return A character vector of the div tree to be rendered in the ui of a  shiny app
#' @examples
#' vbox('Label','£Value')
#' @export
vbox <-
  function(label = 'label',
           textid = 'textOutput_here',
           col = 'rgba(60,130,180,0.5)') {
    shiny::tags$div(
      style =paste0(
        "border-radius: 15px;
                      border-style: solid;
                      border-color:transparent;
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
         textid)
    )
  }


#' builds the div for a reactive shiny value box
#' 
#' @param label The value box header label to appear-  character of length one
#' @param textid The value - A character vector of length one, optionally the textOutput of a reactive value
#' if using vbox_reactive
#' @param col Bkgrd col- A character of rbg(), rgba(), or a chtml olor name to recognixed in R
#' 
#' @return A character vector of the div tree to be rendered in the ui of a  shiny app
#' @examples
#' vbox_reactive('Label','YourTextOutputIdHere')
#' 
#' @export
vbox_reactive <-
  function(label = 'label',
           textid = 'textOutput_here',
           col = 'rgba(60,130,180,0.5)') {
    shiny::tags$div(
      style =paste0(
        "border-radius: 15px;
                      border-style: solid;
                      border-color:transparent;
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


saveplot <- function(vector=1:10,save_name=NULL) {
  
      df <- data.frame(x=seq_along(vector)-1,y=vector)
  
      p <- ggplot2::ggplot(df)+
      ggplot2::geom_line(ggplot2::aes(x,y),col='black',fill='black',size=15)+
      ggplot2::theme_void() +
      ggplot2::theme(title = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          panel.background = ggplot2::element_rect(fill = "transparent",colour = NA),
          plot.background = ggplot2::element_rect(fill = "transparent",colour = NA),
          axis.line = ggplot2::element_line(size = 12, colour = "white", linetype=1,lineend = 'butt')
          )
    
  ggplot2::ggsave(paste0("./www/plot_value_",save_name,".png"), p, bg = "transparent")

}


#' builds the div for a shiny value box with a simple sparklines-like graphic
#' 
#' @param label The label -  character of length one
#' @param textid The value - A character vector of length one, optionally the textOutput of a reactive value
#' if using vbox_reactive
#' @param col Bkgd col- A character of rbg(), rgba(), or a html color name  recognized in R
#' @param graph_title - Sparkline title, character of length one
#' @param value the sparklines values to be plotted - a numeric vector
#' 
#' @return A character vector of the div tree to be rendered in the ui of a shiny app
#' @examples
#' vbox_graphic(label = 'label',textid = 'textOutput_here',col = 'rgba(60,130,180,0.5)',
#' graph_title='nowcast',value=1:10)
#' @export
vbox_graphic <-
  function(label = 'label',
           textid = 'textOutput_here',
           col = 'rgba(60,130,180,0.5)',
           graph_title='nowcast',
           value=1:10) {
    
    saveplot(vector=value,save_name=label)
    
    shiny::tags$div(
      style =
        paste0("border-radius: 15px;
                      border-style: solid;
                      border-color:transparent;
            padding:0px 0px 0px 10px;
            margin:0px 0px 0px 10px;
            color:white;
            width:230px;
            height:130px;
                  font-weight: 300;
                  text-color:blue;
                  text-align:left;
                  background-color:",col,";
            "),
      shiny::fillRow(shiny::tags$div(
      shiny::tags$h5(label, style = 'color:white;'),
      shiny::tags$h1(style = 'font-weight:bold;display: inline-block;vertical-align:top;color:white;',
         textid)),shiny::tags$div(
      shiny::tags$p(graph_title,style='float:right;padding-top:10px;margin-right:30px;'),
      shiny::tags$img(src=paste0('plot_value_',label,'.png'),height=70,style='float:right;padding:5px;margin:5px;')))
    )
  }

