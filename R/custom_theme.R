#' Themes a ggplot object

#' @return A themed *ggplot* object
#' @examples
#'# if(!require(dplyr)){library(dplyr)}
#' #if(!require(ggplot2)){library(ggplot2)}
#' 
#' mtcars <- dplyr::mutate(mtcars,am=as.factor(am))
#' ggplot_obj <-
#'      ggplot2::ggplot( mtcars) + 
#'      ggplot2::facet_wrap(~am)+
#'      ggplot2::labs(title = 'Graph Title')+
#'      ggplot2::geom_point(ggplot2::aes(disp,mpg,col=mpg))
#'      
#' ggplot_obj + custom_theme()
#'
#' @import ggplot2
#' @export
custom_theme <- function(){ggplot2::theme(
    text = ggplot2::element_text(face = , hjust = 0),
    title = ggplot2::element_text(size = 15,
                                  colour = 'dimgrey'),
    panel.background = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(colour = 'black', size = 8),
    strip.text = ggplot2::element_text(face = 'bold.italic', size = 12),
    strip.background = ggplot2::element_rect(
      colour = 'black',
      fill = 'white',
      size = 1
    ),
    legend.position = 'bottom',
    legend.key.width = unit(0.1, units = 'npc'),
    
    legend.key = ggplot2::element_rect(fill = NA)
  )}

