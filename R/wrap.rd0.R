#' Round a number (with leading zeros)
#'
#' @description Rounds a number, includes the leading zero, and retains
#' at least one significant digit when possible. (This is a helper function
#' that will be called by other functions in this package.)
#'
#' @param num The number that will be rounded
#' @param places The number of decimal places to which to round the number
#'
#' @examples
#' wrap.rd0(num = 0.0145, places = 3)
#'
#' @export
wrap.rd0 <- function(num,places=2) {
  if(places<0 | places%%1!=0) {
    return("Argument places must be an integer greater than or equal to zero.")
  }

  temp <- abs(signif(num,places))
  if(round(temp,places)<10^(-places)) {
    if(temp!=0) {
      if(num!=0) {
        a <- num/10^floor(log10(abs(num)))
      }
      if(num==0) {
        a <- 0
      }
      b <- floor(log10(abs(num)))
      if(abs(a)>=5) {
        a <- 10*sign(a); num <- a*10^b
      }
    }
    if(temp==0) {
      if(places==0) {
        return(paste("0"))
      }

      if(places>0) {
        zeros <- paste(rep(0,places),sep="",collapse="")
        return(paste("0.",zeros,sep=""))
      }
    }
    if(temp!=0) {
      return(paste(signif(num,1)))
    }
  }
  return(format(round(num,places),nsmall=places))
}
