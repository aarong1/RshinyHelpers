 #' Sterilises strings, capitalising, removing punct. and spaces. Mostly for matching applications
  #' 
  #' @param text_to_clean A character vector.
  #' @param rm_all_white_space A boolean indicating whether all (not just trailing and double whitespace)
  #' should be stripped.  Setting to false obviously preserves semantic meaning, but in eaither instance
  #' peripherial and sequential white space is always stripped.
  #' 
  #' @return A character vector \code{x} capitalised and devoid of Punctuation and trailing,  double
  #'  (and, optionally all whitespace)
  #' @examples
  #' rmPWCap(c('He1l0 ! WoR1D'))
  #' rmPWCap(c('He1l0 ! WoR1D'),rm_all_white_space=TRUE)
  #' @export
rmPWCap <- function(text_to_clean, rm_all_white_space = F) {
 
  if (rm_all_white_space == FALSE) {
    
    y <- tm::removePunctuation(text_to_clean)    #rm P
    y <- stringr::str_squish(y)    #removes peripheral Wsp and collapses double wsp
    y <- toupper(y) #capitalise
    
  } else{
    y <- tm::removePunctuation(text_to_clean)  #rm P
    y <- stringr::str_squish(y) #removes peripheral Wsp and collapses double wsp
    y <- stringr::str_remove_all(string = y, pattern = ' ')    #removes all whitespace
    y <- toupper(y)
  }#capitalise
  return(y)
}
