#' similar to expand grid - but in expand.grid the 
#' algo does not infer missing dates from the timeseries
#'  our function does !!
#' @param df The dataframe or tibble to operate on
#' @return An expanded data.frame of all time, and optionally all factor permutations
#' @examples
#' 
#' require(ggplot2)
#' require(tibble)
#' require(dplyr)
#' 
#'# time series
#'  ts <-  as.Date("2022-01-03"):as.Date(Sys.Date())
#'  ts <- sort(
#'    as.Date(
#'      ts[sample(c(TRUE,FALSE),size = 101,replace = TRUE,prob = c(0.7,0.3))],
#'      origin='1970-01-01')
#'    )
#' 
#'  df <- tibble::tibble(time=as.Date(ts),
#'               col1=sample(replace=TRUE,letters[c(1:5)],size=length(ts)),
#'               col2=sample(replace=TRUE,1:26,size=length(ts)),
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
#'    dplyr::count(time,wt=col2)%>%
#'    dplyr::mutate(n-lag(n,1))%>%head(10)
#'  
#'  new_df%>%
#'    dplyr::count(time,wt=col2)%>%
#'    dplyr::mutate(n-lag(n,1))%>%head(10)
#'  
#'  #compare BEFORE and
#'  
#'  ggplot2::ggplot(df)+
#'  ggplot2::geom_line(ggplot2::aes(time,col2,col=col1))+
#'  ggplot2::facet_wrap(~col1)+
#'  ggplot2::theme_minimal()
#'  
#'  #... and after
#'  
#'  ggplot2::ggplot(new_df)+
#'  ggplot2::geom_line(ggplot2::aes(time,col2,col=col1))+
#'  ggplot2::facet_wrap(~col1)+
#'  ggplot2::theme_minimal()
#' 
#'
#' @import dplyr
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

#' @describeIn complete_time Apply every combination of variable factors for complete time series.
#' @inheritParams complete_time
#' @export
complete_time_factors <- function(df) {
    time_name <- sapply(df,class)[sapply(df,class)=='Date']%>%names()

    new_df <- expand.grid(as.Date(min(df[[time_name]]):max(df[[time_name]]),origin='1970-01-01'),unique(df$col1))
    names(new_df) <- c(time_name,'col1')
    new_df <- merge(new_df,dplyr::distinct(df[c(time_name,'col1','col2')]),all.x=TRUE)
    new_df$col2[is.na(new_df$col2)] <- 0
    return(new_df)
  }