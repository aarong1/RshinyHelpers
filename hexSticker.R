# file creates the package logo
library(hexSticker)
library(tidyverse)
p <- ggplot(mtcars)+
    geom_dotplot(aes(carb),)+theme_void()#col=NA,fill='white'
    # set transparency

s <- #theme_transparent()+
  sticker(subplot=p,
             spotlight = F,
          package="RshinyHelpers", 
          s_x=1.0,
          s_y=1.1,
          s_width=1, 
          s_height=1,
          p_color = 'navy',
          p_fontface = 'bold',
          p_size=5,
          #p_y=1.3,
          h_fill = 'lightblue',
          h_color = 'steelblue',
          url = 'https://github.com/aarong1/RshinyHelpers#readme',
          #h_size = 10,
          white_around_sticker = F,
          filename="inst/figures/logo.png")
s
ggplot()+
  geom_hexagon(size = 10,color = 'lightblue')+
  #theme_transparent()+
  geom_pkgname('RShinyHelp')+
  #geom_subview()+
  hexSticker::theme_sticker()


library(ggplot2)

p <- ggplot(aes(x = mpg, y = wt), data = mtcars) + geom_point()
p <- p + theme_void() + theme_transparent()

s <- sticker(p, package="hexSticker", p_size=20, s_x=1, s_y=.75, s_width=1.3, s_height=1,
        filename="inst/figures/ggplot2.png")
print(s)
