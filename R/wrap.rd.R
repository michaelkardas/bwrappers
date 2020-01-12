#' Round a number (without leading zeros)
#'
#' @description Rounds a number, omits the leading zero, and retains at
#' least one significant digit when possible. (This is a helper function
#' that will be called by other functions in this package.)
#'
#' @param num The number that will be rounded
#' @param places The number of decimal places to which to round the number
#'
#' @examples
#' wrap.rd(num = 0.0145, places = 3)
#'
#' @keywords internal
#' @export
wrap.rd <- function(num,places=2) {
  if(places<0 | places%%1!=0) {
    return("Argument places must be an integer greater than or equal to zero.")
  }

  if(round(abs(num),places)<10^(-places)) {
    if(num!=0) {a <- num/10^floor(log10(abs(num)))}
    if(num==0) {a <- 0}; b <- floor(log10(abs(num)))
    if(abs(a)>=5) {a <- 10*sign(a); num <- a*10^b}
    temp <- signif(num,1); if(temp<0) {
      return(paste("-.",substr(temp,4,nchar(temp)),sep=""))}
    if(temp>0) {return(paste(".",substr(temp,3,nchar(temp)),sep=""))}
    if(temp==0) {if(places==0) {return(paste("0"))};if(places>0) {
      zeros <- paste(rep(0,places),sep="",collapse="")
      return(paste(".",zeros,sep=""))
      }
    }
  }
  if(substr(format(round(num,places),nsmall=places),1,1)=="0") {
    return(substr(format(round(num,places),nsmall=places),2,nchar(format(round(num,places),nsmall=places))))
  }
  if(substr(format(round(num,places),nsmall=places),1,2)=="-0") {
    return(paste("-",substr(format(round(num,places),nsmall=places),3,nchar(format(round(num,places),nsmall=places))),sep=""))
  }
  if(substr(format(round(num,places),nsmall=places),1,1)!="0"&substr(format(round(num,places),nsmall=places),1,2)!="-0") {
    return(substr(format(round(num,places),nsmall=places),1,nchar(format(round(num,places),nsmall=places))))
  }
}
