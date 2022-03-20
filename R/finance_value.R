
#' Changes bootstrap slider input color individually.
#' Note there is currently no solution to change all of them simultaneously
#' Stick this function in a for loop for the range of slider_index arguments
#' to make this work
#' 
#' @param toplot A numeric vector for the sparklines 
#' @param col A character of length one that takes names, rgb or rgba character vectors of the box background
#' @param label A character vector for length one for the box name
#' @param textid A character vector for the currency value. 


#' @return A tagList of the div tree to be rendered to the shiny ui
#' @examples
#' finance_vbox(label = 'Important metric',textid = '$463')
#' @export
finance_vbox <-
  function(toplot=c(1,2,19,20,90,100,50,200),
          label = 'label',
           textid = 'textOutput_here',
           col = 'rgba(0,0,0,0.6)') {#'rgba(60,130,180,0.5)'
    
    first_value <- toplot[1]
    print(first_value)
    
    last_value <- toplot[length(toplot)]
    print(last_value)
    change <- -1*(first_value-last_value)
    print(change)
    perc_change <- paste(-1*(first_value-last_value)/first_value*100,'%')
    print(perc_change)
    
      shiny::div(style='background-color:white;text-color:white;color:white;','hello')
    shiny::tags$div(
      style =paste0(#box-shadow: 0 4px 10px 0 rgba(0, 0, 0,0.5), 0 4px 10px 0;
        "padding:20px;
            border-style :solid;
            border-color:transparent;
            border-radius:25px;
            margin: 20px;
            width:200px;
            color:black;
            line-height:8px;
                  font-weight: 200;
                  font-size:13px;
                  transform: scale(1, 1.1);
                  text-align:middle;
                  font-family:helvetica;
                  background-color:",col,";"),
      shiny::tags$h6(label, style = 'padding:0px,;margin:0px;color:white;'),
      shiny::icon('sort-down',
           lib='font-awesome',
           style=paste0('font-size:20px;padding:0px;display:inline;color:',ifelse(change<0,'#f3172d;','lightgreen'))),
        shiny::tags$h4(style = 'line-height:3px;display: inline-block;vertical-align:top;color:white;float:middle;',#font-weight:bold;
         textid),br(),br(),

      shiny::span(class=ifelse(change<0,'badge badge-danger','badge badge-success'),
           change,
           style='color:white;font-weight:bold;padding:5px;'),
      #;display:inline;display:inline
      shiny::span(class=ifelse(change<0,'badge badge-danger','badge badge-success'),
           perc_change,style='color:white;font-weight:bold;padding:5px;margin:5px;'),#float:right;
      
      shiny::fillRow(height='10px'),#hr(),
      sparklines::sparkline(toplot,width='100%', "line",
                list(fillColor='white',#NA
                     spotColor='white',
                     lineColor='white',
                     lineWidth=4,
                minSpotColor='#f3172d',
                maxSpotColor='#76ff03',
                spotRadius='3')
    ))
  }