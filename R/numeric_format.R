#'Formats a numeric vector to the desired units and concommitant styling
#' 
#' @param x A numeric of any length
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
#' @describeIn forCur Add Sterling sign
#' @export
forCurGBP <- function(x){
   paste('\U00a3',format(round(x,digits = 0),scientific = F,big.mark = ','))
}

#' @inheritParams forCur
#' @describeIn forCur Add meter sq. signs for areas
#' @export
forCurArea <- function(x){
   paste(format(round(x,digits = 0),scientific = F,big.mark = ','),'m\u00b2')
}

#' @inheritParams forCur
#' @describeIn forCur Add Sterling and per meter sq. signs for cost per area

#' @export
forCurAreaGBP <- function(x){
   paste('\U00a3',format(round(x,digits = 0),scientific = F,big.mark = ','),'m\u00b2')
   
}