#'Formats a numeric vector to the desired units and concommitant styling
#' 
#' @param x A numeric of any length
#' 
#' @return A formated character string
#' @examples
#' forCur(405556)
#' forCurGBP(346236)
#' forCurArea(452)
#' forCurAreaGBP(236)
#' @export
forCur <- function(x){
   format(round(x,digits = 0),scientific = F,big.mark = ',')
}

#' @inheritParams forCur
#' @export
forCurGBP <- function(x){
   paste('\U00a3',format(round(x,digits = 0),scientific = F,big.mark = ','))
}

#' @inheritParams forCur
#' @export
forCurArea <- function(x){
   paste(format(round(x,digits = 0),scientific = F,big.mark = ','),'m\u00b2')
}

#' @inheritParams forCur
#' @export
forCurAreaGBP <- function(x){
   paste('\U00a3',format(round(x,digits = 0),scientific = F,big.mark = ','),'m\u00b2')
   
}