#' Changes bootstrap slider input color individually.
#' Note there is currently no solution to change all of them simultaneously
#' Stick this function in a for loop for the range of slider_index arguments
#' to make this work
#' 
#' @param slider_index A numeric of length one 
#' @param colour A character of length one that takes names, rgb or rgba character vectors of rthe slider color
#' 
#' @return A HTML head element incliuding the necessary css.
#' @examples
#' bootstrap_slider_colour()
#' bootstrap_slider_colour(colour='blue')

#' @export

bootstrap_slider_colour <- function(slider_index=0, colour='mediumseagreen') {
  y <- 
    shiny::tags$head(
    shiny::tags$style(
      htmltools::HTML(paste0('.js-irs-',slider_index,'  .irs-single, .js-irs-',slider_index,' .irs-bar-edge, .js-irs-0 .irs-bar {
                                                    background: ',colour,';
                                                    border-top: 1px solid ',colour,' ;
                                                    border-bottom: 1px solid ',colour,' ;}
  
                              /* changes the colour of the number tags */
                             .irs-from, .irs-to, .irs-single { background: ',colour,' }'))))
  
  return(y)
}
