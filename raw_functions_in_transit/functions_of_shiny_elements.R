vbox <-function(label = 'label',
           textid = 'textOutput_here',
           col = 'rgba(60,130,180,0.5)') {
    div(
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
      h5(label, style = 'color:white;'),
      h2(style = 'font-weight:bold;display: inline-block;vertical-align:top;color:white;',
         textid)
    )
  }


rmPWCap <- function(x, rm_all_white_space = F) {
  if (rm_all_white_space == F) {
    y <- x %>% removePunctuation() %>%   #rm P
      str_squish() %>%   #removes peripheral Wsp and collapses double wsp
      toupper() #capitalise
  } else{
    y <- x %>% removePunctuation() %>%   #rm P
      str_squish() %>% #removes peripheral Wsp and collapses double wsp
      str_remove_all(string = ., pattern = ' ') %>%    #removes all whitespace
      toupper()
  }#capitalise
  return(y)
}

bootstrap_slider_colour <- function(slider_index=0, colour='mediumseagreen') {
  tags$head(
  tags$style(
    HTML('.js-irs-',slider_index,'  .irs-single, .js-irs-',slider_index,' .irs-bar-edge, .js-irs-0 .irs-bar {
                                                    background: ',colour,';
                                                    border-top: 1px solid ',colour,' ;
                                                    border-bottom: 1px solid ',colour,' ;}
  
                              /* changes the colour of the number tags */
                             .irs-from, .irs-to, .irs-single { background: ',colour,' }')))

}

DT_selected_row_colour <- function( colour='pink') {
  
  shiny::tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {background-color: ',colour,' !important;}'))

}


chk_nas <- function(x){
  #takes atomic vectors
  y <- x%>%
    is.na()%>%
    which()
  return(y)
  #returns indices of nas 
}

chk_dups <- function(x){
  #takes atomic vectors
  y <- x%>%
    duplicated()%>%
    which()
  return(y)
  #returns indices, 2nd or higher of repeated values.
}
