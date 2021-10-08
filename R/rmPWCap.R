rmPWCap <- function(text_to_clean, rm_all_white_space = F) {
  #' Sterilises strings for matching
  #' 
  #' @param x A character vector.
  #' 
  #' @return A character vector \code{x} capitalised and devoid of Punctuation and trailing,  double
  #'  (and, optionally all whitespace)
  #' @examples
  #' rmPWCap(c('He1l0 ! '))
  #' rmPWCap(c('He1l0 ! '),rm_all_white_space=)
  #' _
  if (rm_all_white_space == F) {
    y <- text_to_clean %>% 
      tm::removePunctuation() %>%   #rm P
      stringr::str_squish() %>%   #removes peripheral Wsp and collapses double wsp
      toupper() #capitalise
  } else{
    y <- text_to_clean %>% 
      tm::removePunctuation() %>%   #rm P
      stringr::str_squish() %>% #removes peripheral Wsp and collapses double wsp
      stringr::str_remove_all(string = ., pattern = ' ') %>%    #removes all whitespace
      toupper()
  }#capitalise
  return(y)
}
