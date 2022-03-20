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


#pretty printing of lists of many to one relationships
#best performed after a left_join
collapse_rows <- function(df, group_by_var, many_mapping_col) {
  newdf <- df%>%
    distinct({{group_by_var}},{{many_mapping_col}})%>%
    group_by({{group_by_var}})%>%
    mutate(wot=all(is.na({{many_mapping_col}})))%>%
    group_by({{group_by_var}})%>%
    summarise(ls=list({{many_mapping_col}}),wot)%>%
    mutate(pretty_col=paste0(ls))%>%
    mutate(pretty_col=str_replace_all(pretty_col,pattern = '\\(',replacement=''))%>%
    mutate(pretty_col=str_replace_all(pretty_col,pattern = '\\)',replacement=''))%>%
    mutate(pretty_col=str_replace_all(pretty_col,pattern = '"',replacement=''))%>%
    mutate(pretty_col=str_replace_all(pretty_col,pattern = 'c',replacement=''))%>%
    mutate(pretty_col=ifelse(grepl('NA',pretty_col),NA,pretty_col))%>%
    distinct()
  
  return(newdf)
}

#nest(data=list(many_mapping_col))

# EmployerClusters <- EmployerClusters%>%
#   left_join(.,Cclusters1[c('CaseNumber','clusters')])


#This generates programmatically the required length of arguments 
#used primarialy for the tooltips of maps and graphs can repurpose for any
#many to one relationships

#----
#example data 
nodes_tot=data.frame(
  FirstName=sample(c('A','B','C'),size = 50,replace=T),
  LastName=sample(c('1','2','3'),size = 50,replace=T),
  Latitude=sample(c('D','E','F'),size = 50,replace=T),
  Longitude=sample(c('4','5','6'),size = 50,replace=T))
#---------

collapse_rows(nodes_tot,FirstName,Longitude)

nodes_tot%>%
  #slice_head(n = 50)%>%
  unite(FirstName,LastName,col='FullName')%>%
  group_by(Latitude,Longitude)%>%
  mutate(label=c(FullName))%>%
  #mutate(label1=list(FullName))%>%
  mutate(label2=paste(as.vector(label),sep=' ',collapse= '<br>' ))%>%View()
# mutate(label3=paste(label,sep=' ',collapse= ''))%>%
# mutate(label4=paste(as.vector(label1),sep=' ',collapse= ''))%>%
# mutate(label5=paste(label1,sep=' ',collapse= ''))%>%



# dedup a table based on the values of two or more fields
# order does not matter.
#it does this by creating an additional
#intermediate field and orders alphabetically

#useful for undirected graph applications. used in covid trnasimssion modelling
# to quantify the number of 'handshakes' and interactions,
#between people and between features of people.
#-------
#example data
edges <- data.frame(
  to=sample(c('A','B','C','D'),size = 50,replace=T),
  from=sample(c('A','B','C','D','E','F'),size = 50,replace=T))

count(edges,to,from)

#-----

similar_permutations <- function(edges, to, from) {
  
  x <- edges%>%
    rowwise()%>%
    #-----------
    # important as that the selection is from each ROW and col
    # and we are not grabbing the entire field
    #------------
    mutate(to_from=list(c({{to}},{{from}})))
  
  
  y <-  map(x$to_from,sort)%>% #sort alphabetically
    as.data.frame()%>% #prep for transpose
    t()%>% #transpose
    as.data.frame() #coerce back to data.frame- doesn't also preserve df
  
  names(y) <- c('to_ordered','from_ordered')
  
  orderedxy <- cbind(x,y)%>%
    rownames_to_column(var='rn')%>%
    select(-rn)
  
  #we can then get the ordered fields and count them 
  orderedxy <- orderedxy%>%
    group_by(to_ordered,from_ordered)%>%#,label
    add_count()%>%
    filter(n>1)%>%
    ungroup()%>%
    select(-c(n,to_ordered,from_ordered,to_from))
  
  return(orderedxy)
}


#-------ggplot themes

library(tidyverse)
library(plotly)

setcol <- RColorBrewer::brewer.pal(n = 3,'Reds')[2]

produce_plot <- function() {
  p <- ggplot(mtcars%>%
                 mutate(am=as.factor(am)
                        )
               )+
    #geom_point(aes(disp,mpg,col=am))+#,fill=setcol
      geom_bar(aes(am))+
    labs(title = 'Graph Title',
         #subtitle= 'Subtitle',
         # caption = 'caption',
         # tag = 'tag'
         )#coord_cartesian(clip='off')+
    #facet_wrap(~cyl)+
      invisible(x <- class(p$data[[p$labels$colour]]))
                p <- p+
    theme(
          text = element_text(face = 'bold'),#,hjust = 1
          title = element_text(size = 15,
                               colour='dimgrey',
                               # margin = margin(t = 0, r = 0, b = 10, l = 0, 
                               #                 unit = "pt")
                               ),
          panel.background = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_text(colour='black',size=8),
          strip.text = element_text(face = 'bold.italic',size = 12),
          strip.background=element_rect(colour='grey',fill = 'grey',size = 2),
          #legend.box.just = 'right',
          #legend.box = 'horizontal',
          #legend.direction = 'horizontal',
          legend.position = 'bottom',
          legend.key.width = unit(0.1,units='npc'),
          #legend.box.background = element_blank(),
          legend.key = element_rect(fill=NA),
          #legend.title.align = 1,
          #legend.background = element_blank(),
          #legend.box.background = element_blank()
          
          )+
    
    if(x=='character'|x=='factor') {
            scale_colour_brewer(palette='Set2',aesthetics=c('fill','colour'))
      #scale_colour_viridis(discrete = T)
    }else {if(is.na(x)){scale_color_manual(values=setcol)}
        else{scale_colour_viridis(discrete=F)}
    }
                
                return(p)
}
  
x <- class(p$data[[p$labels$colour]])

ggplotly(p)

  #method for discrete variables 
  ggplot(mtcars%>%
                 mutate(am=as.factor(am)
                        )
               )+
    #geom_point(aes(disp,mpg,col=am))+#,fill=setcol
      geom_point(aes(disp,mpg))+
    labs(title = 'Graph Title',
         #subtitle= 'Subtitle',
         # caption = 'caption',
         # tag = 'tag'
         )+#facet_wrap(~am)+
    
               
    theme(
          text = element_text(face = ,hjust = 0),
          title = element_text(size = 15,
                               colour='dimgrey',
                               # margin = margin(t = 0, r = 0, b = 10, l = 0, 
                               #                 unit = "pt")
                               ),
          panel.background = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_text(colour='black',size=8),
          strip.text = element_text(face = 'bold.italic',size = 12),
          strip.background=element_rect(colour='grey',fill = 'grey',size = 2),
          #legend.box.just = 'right',
          #legend.box = 'horizontal',
          #legend.direction = 'horizontal',
          legend.position = 'bottom',
          legend.key.width = unit(0.1,units='npc'),
          #legend.box.background = element_blank(),
          legend.key = element_rect(fill=NA))+
    scale_color_brewer(palette="Dark2")
  
  
  ggplot_obj <- ggplot(mtcars%>%
                 mutate(am=as.factor(am)
                        )
               ) +    labs(title = 'Graph Title',
         #subtitle= 'Subtitle',
         # caption = 'caption',
         # tag = 'tag'
         )+#facet_wrap(~am)+
    #geom_point(aes(disp,mpg,col=am))+#,fill=setcol
      geom_point(aes(disp,mpg,col=mpg))
  
  custom_theme(ggplot_obj = ggplot_obj)
    # 

    # theme(
    #       text = element_text(face = ,hjust = 0),
    #       title = element_text(size = 15,
    #                            colour='dimgrey',
    #                            # margin = margin(t = 0, r = 0, b = 10, l = 0, 
    #                            #                 unit = "pt")
    #                            ),
    #       panel.background = element_blank(),
    #       axis.ticks = element_blank(),
    #       axis.text = element_text(colour='black',size=8),
    #       strip.text = element_text(face = 'bold.italic',size = 12),
    #       strip.background=element_rect(colour='grey',fill = 'grey',size = 2),
    #       #legend.box.just = 'right',
    #       #legend.box = 'horizontal',
    #       #legend.direction = 'horizontal',
    #       legend.position = 'bottom',
    #       legend.key.width = unit(0.1,units='npc'),
    #       #legend.box.background = element_blank(),
    #       legend.key = element_rect(fill=NA))+
    # scale_color_gradient(low = 'darkblue',high='darksalmon')
    # 

    
  rollavg <- function(x, length = 3) {
    
    y <- stats::filter(x, rep(1 / length, length), sides = 1)
    return(y)
  }
  
  geo_read <- function(filename = "LGD_2012.geojson") {
    #' Paste two items
  #' 
  #' @description This function pastes two items
  #' together.  
  #'
  #' By using the description tag you'll notice that I
  #' can have multiple paragraphs in the description section
  #' 
  #' @param x character. The first item to paste
  #' @param y character. The second item to paste Defaults to "!" but
  #' "?" would be pretty great too
  #' @usage mypaste(x, y)
  #' @return The inputs pasted together as a character string.
  #' @details The inputs can be anything that can be input into
  #' the paste function.
  #' @note And here is a note. Isn't it nice?
  #' @section I Must Warn You:
  #' The reference provided is a good read.
  #' \subsection{Other warning}{
  #'   It is completely irrelevant to this function though.
  #' }
  #' 
  #' @references Tufte, E. R. (2001). The visual display of 
  #' quantitative information. Cheshire, Conn: Graphics Press.
  #' @examples
  #' mypaste(1, 3)
  #' mypaste("hey", "you")
  #' mypaste("single param")
  #' @export
  #' @importFrom sf st_read
  
    shape <- sf::st_read(filename)
    return(shape)
  }
  
  x <- sf::st_read('OSNI')
  
  ggplot(x)+geom_sf()
  
  
  # time series
  ts <-  as.Date("2022-01-03"):as.Date(Sys.Date())
  ts <- sort(
    as.Date(
      ts[sample(c(T,F),size = 101,replace = T,prob = c(0.7,0.3))],
      origin='1970-01-01')
    )
 
  df <- tibble::tibble(time=as.Date(ts),
               col1=sample(replace=T,letters[c(1:5)],size=length(ts)),
               col2=sample(replace=T,1:26,size=length(ts)),
               )
  
  #we want to expand and fill empty occurrances with zero
  
  length(ts)*n_distinct(df$col2)
  
  time_name <- sapply(df,class)[sapply(df,class)=='Date']%>%names()
  gp_var <- 2
  val_var <- 3
  #new_df <- tibble(time_col=as.Date(min(ts):max(ts)))
  
  complete_time <- function(df) {
    time_name <- sapply(df,class)[sapply(df,class)=='Date']%>%names()

    new_df <- as.data.frame(as.Date(min(df[[time_name]]):max(df[[time_name]]),origin='1970-01-01'))
    names(new_df) <- c(time_name)
    new_df <- merge(new_df,df[c(time_name,'col1','col2')],all.x=TRUE)
    new_df$col2[is.na(new_df$col2)] <- 0
    new_df$col2[is.na(new_df$col1)] <- 0

    return(new_df)
  }
  
  complete_time_factors <- function(df) {
    time_name <- sapply(df,class)[sapply(df,class)=='Date']%>%names()

    new_df <- expand.grid(as.Date(min(df[[time_name]]):max(df[[time_name]]),origin='1970-01-01'),unique(df$col1))
    names(new_df) <- c(time_name,'col1')
    new_df <- merge(new_df,dplyr::distinct(df[c(time_name,'col1','col2')]),all.x=TRUE)
    new_df$col2[is.na(new_df$col2)] <- 0
    return(new_df)
  }
  
  new_df <- complete_time_factors(df)
  #new_df <- complete_time(df)
#similar to expand grid - but in expand.grid the 
#algo does not infer missing dates from the timeseries
# our function does !!
  
  df%>%
    count(time,wt=col2)%>%
    mutate(n-lag(n,1))%>%head(10)
  
  new_df%>%
    count(time,wt=col2)%>%
    mutate(n-lag(n,1))%>%head(10)
  
  #compare BEFORE and
  
  ggplot2::ggplot(df)+geom_line(aes(time,col2,col=col1))+facet_wrap(~col1)+theme_minimal()
  
  #... and after
  
  ggplot2::ggplot(new_df)+geom_line(aes(time,col2,col=col1))+facet_wrap(~col1)+theme_minimal()
  
# functions for formatting numeric types into 
# character strings

  
forCur <- function(x){
   format(round(x,digits = 0),scientific = F,big.mark = ',')
}

forCurGBP <- function(x){
   paste('\U00a3',format(round(x,digits = 0),scientific = F,big.mark = ','))
}

forCurArea <- function(x){
   paste(format(round(x,digits = 0),scientific = F,big.mark = ','),'m\u00b2')
}

forCurAreaGBP <- function(x){
   paste('\U00a3',format(round(x,digits = 0),scientific = F,big.mark = ','),'m\u00b2')
   
}
  

 # https://www.msn.com/en-gb/money/watchlist?id=bvcjvh&cvid=62c3be5076604903bfa9cf481f97a975&ocid=winp1taskbar&duration=1D

