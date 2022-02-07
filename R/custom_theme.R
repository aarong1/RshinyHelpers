#' Themes a ggplot object
#' @param ggplot A ggplot object
#' @return A themed *ggplot* object
#' @examples
#' ggplot_obj <- mtcars%>%
#'               mutate(am=as.factor(am))%>%
#'               ggplot( ) + 
#'      facet_wrap(~am)+
#'      labs(title = 'Graph Title')+
#'      geom_point(aes(disp,mpg,col=mpg))+
#'      custom_theme
#'      
# custom_theme(ggplot_obj = ggplot_obj)
#
#' @import ggplot2
#' @export
#' 
custom_theme <- ggplot2::theme(
    text = ggplot2::element_text(face = , hjust = 0),
    title = ggplot2::element_text(size = 15,
                                  colour = 'dimgrey'),
    panel.background = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(colour = 'black', size = 8),
    strip.text = ggplot2::element_text(face = 'bold.italic', size = 12),
    strip.background = ggplot2::element_rect(
      colour = 'grey',
      fill = 'grey',
      size = 2
    ),
    legend.position = 'bottom',
    legend.key.width = unit(0.1, units = 'npc'),
    
    legend.key = ggplot2::element_rect(fill = NA)
  )

