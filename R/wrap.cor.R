#' Correlation test
#'
#' @description Tests for a correlation between two dependent variables. The function
#' delegates the primary computations to \code{\link[stats]{cor.test}}.
#'
#' @param dv1,dv2 Column vectors containing the dependent variables
#'
#' @seealso \code{\link[stats]{cor.test}}
#'
#' @examples
#' wrap.cor(dv1 = bdata$DV3_T1, dv2 = bdata$DV3_T2)
#'
#' @import stringr
#' @importFrom psychometric CIr
#' @importFrom clipr write_clip
#' @export
wrap.cor <- function(dv1, dv2) {

  options(scipen=999)

  # Error checks
  if(is.null(dv1)) {return(paste("Cannot find the column vector inputted to parameter dv1."))}
  if(is.null(dv2)) {return(paste("Cannot find the column vector inputted to parameter dv2."))}
  if(is.null(dv1)==F) {if(is.numeric(dv1)==F) {return("Argument dv1 must be numeric.")}}
  if(is.null(dv2)==F) {if(is.numeric(dv2)==F) {return("Argument dv2 must be numeric.")}}
  if(substitute(dv1)==substitute(dv2)) {return("Error: You entered the same column twice.")}
  complete.cases <- complete.cases(cbind(dv1,dv2))
  if(any(complete.cases==F)) {print("Note: Your inputs include one or more NA entries. The function will ignore the rows containing these entries.")}
  dv1 <- dv1[complete.cases]
  dv2 <- dv2[complete.cases]

  # correlations
  a <- cor.test(dv1, dv2)

  # confidence intervals
  b <- CIr(a$estimate,n=length(dv1),level=.95)

  if(a$p.value < .001) {
    write_clip(allow_non_interactive = TRUE, content = paste(" # ","r = ",wrap.rd(a$estimate,2),", t(",a$parameter,") = ",wrap.rd0(a$statistic,2),", p < .001, 95% CI = [",wrap.rd(b[1],2),", ",wrap.rd(b[2],2),"]",sep=""))
    return(cat(" # ","r = ",wrap.rd(a$estimate,2),", t(",a$parameter,") = ",wrap.rd0(a$statistic,2),", p < .001, 95% CI = [",wrap.rd(b[1],2),", ",wrap.rd(b[2],2),"]",sep=""))
  }

  else {
    write_clip(allow_non_interactive = TRUE, content = paste(" # ","r = ",wrap.rd(a$estimate,2),", t(",a$parameter,") = ",wrap.rd0(a$statistic,2),", p = ",wrap.rd(a$p.value,3),", 95% CI = [",wrap.rd(b[1],2),", ",wrap.rd(b[2],2),"]",sep=""))
    return(cat(" # ","r = ",wrap.rd(a$estimate,2),", t(",a$parameter,") = ",wrap.rd0(a$statistic,2),", p = ",wrap.rd(a$p.value,3),", 95% CI = [",wrap.rd(b[1],2),", ",wrap.rd(b[2],2),"]",sep=""))
  }
}
