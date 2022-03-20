#' similar to expand grid - but in expand.grid the 
#' algo does not infer missing dates from the timeseries
#'  our function does !!
#' @param df The dataframe or tibble to operate on
#' @param value The value - A character vector of length four
#' @return A tagList of the div tree to be rendered in the ui of a  shiny app
#' @examples
#' 
#'# time series
#'  ts <-  as.Date("2022-01-03"):as.Date(Sys.Date())
#'  ts <- sort(
#'    as.Date(
#'      ts[sample(c(T,F),size = 101,replace = T,prob = c(0.7,0.3))],
#'      origin='1970-01-01')
#'    )
#' 
#'  df <- tibble::tibble(time=as.Date(ts),
#'               col1=sample(replace=T,letters[c(1:5)],size=length(ts)),
#'               col2=sample(replace=T,1:26,size=length(ts)),
#'               )
#'
#'  time_name <- sapply(df,class)[sapply(df,class)=='Date']%>%names()
#'  
#'  new_df <- complete_time_factors(df)
#'
#'  
#'#similar to expand grid - but in expand.grid the 
#'#algo does not infer missing dates from the timeseries
#'# our function does !!
#'  
#'  df%>%
#'    count(time,wt=col2)%>%
#'    mutate(n-lag(n,1))%>%head(10)
#'  
#'  new_df%>%
#'    count(time,wt=col2)%>%
#'    mutate(n-lag(n,1))%>%head(10)
#'  
#'  #compare BEFORE and
#'  
#'  ggplot2::ggplot(df)+geom_line(aes(time,col2,col=col1))+facet_wrap(~col1)+theme_minimal()
#'  
#'  #... and after
#'  
#'  ggplot2::ggplot(new_df)+geom_line(aes(time,col2,col=col1))+facet_wrap(~col1)+theme_minimal()
#' 
#'
#' @import shiny
#' @export
complete_time <- function(df) {
    time_name <- sapply(df,class)[sapply(df,class)=='Date']%>%names()

    new_df <- as.data.frame(as.Date(min(df[[time_name]]):max(df[[time_name]]),origin='1970-01-01'))
    names(new_df) <- c(time_name)
    new_df <- merge(new_df,df[c(time_name,'col1','col2')],all.x=TRUE)
    new_df$col2[is.na(new_df$col2)] <- 0
    new_df$col2[is.na(new_df$col1)] <- 0

    return(new_df)
}

#' @export
complete_time_factors <- function(df) {
    time_name <- sapply(df,class)[sapply(df,class)=='Date']%>%names()

    new_df <- expand.grid(as.Date(min(df[[time_name]]):max(df[[time_name]]),origin='1970-01-01'),unique(df$col1))
    names(new_df) <- c(time_name,'col1')
    new_df <- merge(new_df,dplyr::distinct(df[c(time_name,'col1','col2')]),all.x=TRUE)
    new_df$col2[is.na(new_df$col2)] <- 0
    return(new_df)
  }