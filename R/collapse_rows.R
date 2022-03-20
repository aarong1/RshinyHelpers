  #' collapse row values from a table into a list for pretty printing
  #' 
  #' @description TThis function folds values of a many to one relationship together in a
  #' single character composed of comma separated values.
  #'
  #' @param df dataframe, tibble, The data structure to work on
  #' @param group_by_var tidy-eval, character The variable to group by
  #' @param many_mapping_col The column of values that will collapse into a list.
  #' @return The inputs pasted together as a character string.
  #' @details The inputs can be anything that casn be input into
  #' the paste function.
  #' @note ensure the total values to fold are sufficiently low tp preserve the pretty print aesthetic
  #' @section experimant first if it helps:
  #' \subsection{not well supported }{
  #'   on multiple data types
  #' }
 
  #' @examples
  #' 
  #'#example data 
  #'nodes_tot=data.frame(
  #'  FirstName=sample(c('A','B','C'),size = 50,replace=TRUE),
  #'  LastName=sample(c('1','2','3'),size = 50,replace=TRUE),
  #'  Latitude=sample(c('D','E','F'),size = 50,replace=TRUE),
  #'  Longitude=sample(c('4','5','6'),size = 50,replace=TRUE))
  #'#---------
  #'
  #'collapse_rows(nodes_tot,FirstName,Longitude)
  #'  
  #' @import dplyr
  #' @export
collapse_rows <- function(df, group_by_var, many_mapping_col) {
  newdf <- df%>%
    dplyr::distinct({{group_by_var}},{{many_mapping_col}})%>%
    dplyr::group_by({{group_by_var}})%>%
    dplyr::mutate(wot=all(is.na({{many_mapping_col}})))%>%
    dplyr::group_by({{group_by_var}})%>%
    dplyr::summarise(ls=list({{many_mapping_col}}),wot)%>%
    dplyr::mutate(pretty_col=paste0(ls))%>%
    dplyr::mutate(pretty_col=stringr::str_replace_all(pretty_col,pattern = '\\(',replacement=''))%>%
    dplyr::mutate(pretty_col=stringr::str_replace_all(pretty_col,pattern = '\\)',replacement=''))%>%
    dplyr::mutate(pretty_col=stringr::str_replace_all(pretty_col,pattern = '"',replacement=''))%>%
    dplyr::mutate(pretty_col=stringr::str_replace_all(pretty_col,pattern = 'c',replacement=''))%>%
    dplyr::mutate(pretty_col=ifelse(grepl('NA',pretty_col),NA,pretty_col))%>%
    dplyr::distinct()
  
  return(newdf)
}

