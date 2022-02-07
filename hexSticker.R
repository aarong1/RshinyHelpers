# file creates the package logo
library(hexSticker)
library(tidyverse)
p <- ggplot(mtcars)+
    geom_dotplot(aes(carb),)+theme_void()#col=NA,fill='white'
    # set transparency

s <- sticker(subplot=p,
             spotlight = F,
          package="RshinyHelpers", 
          s_x=1.0,
          s_y=1.2,
          s_width=1.5, 
          s_height=1.2,
          p_color = 'white',
          p_fontface = 'bold',
          p_size=13,
          p_y=1.3,
          h_fill = 'lightblue',
          h_color = 'steelblue',
          url = 'https://github.com/aarong1/RshinyHelpers#readme',
          h_size = 10,
          white_around_sticker = T,
          filename="inst/figures/logo.png")+theme_transparent()
s
