  #' find the distinct _permutations_ of columns
  #' @description find the distinct _permutations_ of columns, particularly in graph applications were
  #' describing relationships between unordered nodes, or other applications were the order
  #' does not matter
  #' 
  #' @param edges dataframe, tibble, The data structure to work on
  #' @param to tidy-eval, character The variable to group by
  #' @param from The column of values that will collapse into a list.
  #' @return The inputs pasted together as a character string.
  #' @details The inputs can be anything that casn be input into
  #' the paste function.
  #' @note ensure the total values to fold are sufficiently low tp preserve the pretty print aesthetic
  #' @section experimant first if it helps:
  #' \subsection{not well supported }{
  #'   on multiple data types
  #' }
 
  #' @examples
  #' if(!require(dplyr)){library(dplyr)}
  #' edges <- data.frame(
  #'   to=sample(c('A','B','C','D'),size = 50,replace=TRUE),
  #'   from=sample(c('A','B','C','D','E','F'),size = 50,replace=TRUE))
  #' 
  #' dplyr::count(edges,to,from)
  #' 
  #' ordered = similar_permutations(edges,to,from)
  #'  
  #' dplyr::count(ordered, to ,from)
  #' @import dplyr
  #' @import tidyr
  #' @import tibble
  #' @import purrr
  #'@export
similar_permutations <- function(edges, to, from) {
  
  x <- edges%>%
    dplyr::rowwise()%>%
    #-----------
    # important as that the selection is from each ROW and col
    # and we are not grabbing the entire field
    #------------
    dplyr::mutate(to_from=list(c({{to}},{{from}})))
  
  
  y <-  purrr::map(x$to_from,sort)%>% #sort alphabetically
    as.data.frame()%>% #prep for transpose
    t()%>% #transpose
    as.data.frame() #coerce back to data.frame- doesn't also preserve df
  
  names(y) <- c('to_ordered','from_ordered')
  
  orderedxy <- cbind(x,y)%>%
    tibble::rownames_to_column(var='rn')%>%
    dplyr::select(-rn)
  
  #we can then get the ordered fields and count them 
  orderedxy <- orderedxy%>%
    dplyr::group_by(to_ordered,from_ordered)%>%#,label
    dplyr::add_count()%>%
    dplyr::filter(n>1)%>%
    dplyr::ungroup()%>%
    dplyr::select(-c(n,to_ordered,from_ordered,to_from))
  
  return(orderedxy)
}

