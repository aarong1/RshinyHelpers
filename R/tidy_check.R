#' checks and diagnoses NA presence in vectors
#' 
#' @param x A vector of *any* type
#' 
#' @return A numeric indicating NA entries, or length 0 vector if none
#' @examples
#' x <- c(3,4,5,6,NA,7,NA,8,NA,9)
#'chk_nas(x)
#' @export
chk_nas <- function(x){
  #takes atomic vectors
  y <-is.na(x)
  y <- which(y)
  return(y)
  #returns indices of nas 
}

#' checks and diagnoses NA presence in vectors
#' 
#' @param x A vector of *any* type
#' 
#' @return A vector indicating duplicated entries, or length 0 vector if none
#' @examples
#' x <- c(3,4,5,6,6,7,7,8,3,9)
#'chk_dups(x)
#' @export
chk_dups <- function(x){
  #takes atomic vectors
  y <-duplicated(x)
  y <- which(y)
  return(y)
  #returns indices, 2nd or higher of repeated values.
}
