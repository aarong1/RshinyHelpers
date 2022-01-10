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
character_list <- function(df, group_by_var, many_mapping_col) {
  Cclusters1 <- df%>%
    distinct(group_by_var,many_mapping_col)%>%
    group_by(group_by_var)%>%
    mutate(wot=all(is.na(many_mapping_col)))%>%
    group_by(group_by_var)%>%
    summarise(ls=list(many_mapping_col),wot)%>%
    mutate(pretty_col=paste0(ls))%>%
    mutate(pretty_col=str_replace_all(pretty_col,pattern = '\\(',replacement=''))%>%
    mutate(pretty_col=str_replace_all(pretty_col,pattern = '\\)',replacement=''))%>%
    mutate(pretty_col=str_replace_all(pretty_col,pattern = '"',replacement=''))%>%
    mutate(pretty_col=str_replace_all(pretty_col,pattern = 'c',replacement=''))%>%
    mutate(pretty_col=ifelse(grepl('NA',pretty_col),NA,pretty_col))%>%
    distinct()
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
edges_tot <- data.frame(
  to=sample(c('A','B','C','D'),size = 50,replace=T),
  from=sample(c('A','B','C','D','E','F'),size = 50,replace=T))

count(edges_tot,to,from)
#-----
x <- edges_tot %>%
  #group_by(to,from)%>%
  rowwise()%>%
  #-----------
  # important os that the selection is from each ROW and col
  # and we are not grabbing the entire field
  #------------
  mutate(to_from=list(c(to,from)))#%>%View
# mutate(map_chr(.$to_from,.f = ~order(.x)))

# mutate(map(to_from= order(to_from)))#%>%
# View

y <-  map(x$to_from,sort)%>% #sort aphabetically
  as.data.frame()%>% #prep for transpose
  t()%>% #tranpose
  as.data.frame() #coerce back to data.frame- doesnt also preserve df

names(y) <- c('to_ordered','from_ordered')

orderedxy <- cbind(x,y)%>%
  rownames_to_column(var='rn')%>%
  select(-rn)

#we can then get the ordered fields and count them 
orderedxy <- orderedxy%>%
  group_by(to_ordered,from_ordered)%>%#,label
  add_count()%>%
  filter(n>1)%>%
  select(-c(n,to_ordered,from_ordered,to_from))


#-------ggplot themes

library(tidyverse)
library(plotly)

setcol <- RColorBrewer::brewer.pal(n = 3,'Reds')[2]
(p <- ggplot(mtcars%>%
               mutate(cyl=as.factor(cyl)
                      )
             )+
  geom_point(aes(disp,mpg,col=am))+#,fill=setcol
  labs(title = 'Graph Title',
       #subtitle= 'Subtitle',
       # caption = 'caption',
       # tag = 'tag'
       )+#coord_cartesian(clip='off')+
  #facet_wrap(~cyl)+
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
        legend.key = element_blank(),
        #legend.title.align = 1,
        #legend.background = element_blank(),
        #legend.box.background = element_blank()
        
        )+scale_colour_brewer(palette='Set2',aesthetics=c('fill','colour'))
    #scale_colour_viridis(discrete = T)
  )

ggplotly(p)

  
  
  
