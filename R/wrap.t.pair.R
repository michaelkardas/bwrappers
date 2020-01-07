#' Paired-samples t test
#'
#' @description Performs paired-samples t tests. The function delegates the
#' primary computations to \code{\link[stats]{t.test}}.
#'
#' @param dv1,dv2 Column vectors containing the dependent variables
#'
#' @seealso \code{\link[stats]{t.test}}
#'
#' @examples
#' wrap.t.pair(dv1 = bdata$DV3_T1, dv2 = bdata$DV3_T2)
#'
#' @import effsize stringr stats
#' @importFrom clipr write_clip
#' @export
wrap.t.pair  <- function(dv1, dv2) {
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

  ## t-test results
  a <- t.test(dv1, dv2, alternative = "two.sided", mu = 0, paired=T, conf.level = 0.95)

  # cohen's d
  b <- (cohen.d(dv1,dv2,paired=T,na.rm=T)$estimate)

  if(a$p.value < .001) {
    write_clip(allow_non_interactive = TRUE, content = paste("# paired t(",a$parameter,") = ",wrap.rd0(a$statistic,2),", p < .001, 95% CIdifference = [",wrap.rd0(a$conf.int[1],2),", ",wrap.rd0(a$conf.int[2],2),"], d = ",wrap.rd0(b,2),sep=""))
    return(cat("\n","# paired t(",a$parameter,") = ",wrap.rd0(a$statistic,2),", p < .001, 95% CIdifference = [",wrap.rd0(a$conf.int[1],2),", ",wrap.rd0(a$conf.int[2],2),"], d = ",wrap.rd0(b,2),sep=""))
  }

  else {
    write_clip(allow_non_interactive = TRUE, content = paste("# paired t(",a$parameter,") = ",wrap.rd0(a$statistic,2),", p = ",wrap.rd(a$p.value,3),", 95% CIdifference = [",wrap.rd0(a$conf.int[1],2),", ",wrap.rd0(a$conf.int[2],2),"], d = ",wrap.rd0(b,2),sep=""))
    return(cat("# paired t(",a$parameter,") = ",wrap.rd0(a$statistic,2),", p = ",wrap.rd(a$p.value,3),", 95% CIdifference = [",wrap.rd0(a$conf.int[1],2),", ",wrap.rd0(a$conf.int[2],2),"], d = ",wrap.rd0(b,2),sep=""))
  }
}
