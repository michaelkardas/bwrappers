#' T Test (One Sample)
#'
#' @description Performs one-sample t tests. The function delegates the primary
#' operations to \code{\link[stats]{t.test}}.
#'
#' @param dv1 Column vector containing the dependent variable
#' @param mu Mean against which to compare responses on the dependent variable
#'
#' @seealso \code{\link[stats]{t.test}}
#'
#' @examples
#' wrap.t.one(dv1 = bdata$DV5, mu = 5)
#' @import effsize stringr stats
#' @importFrom clipr write_clip
#' @export
wrap.t.one  <- function(dv1, mu = 0) {
  options(scipen=999)

  # Error checks
  if(is.null(dv1)) {return(paste("Error: Cannot find the column vector inputted to parameter dv1."))}
  if(is.null(dv1)==F) {if(is.numeric(dv1)==F) {return("Error: Argument dv1 must be numeric.")}}
  if(is.null(mu)==T) {return("Error: Must enter numerical value for mu.")}
  if(is.null(mu)==F) {if(is.numeric(mu)==F|length(mu)>1) {return("Error: Must enter a numerical value for mu.")}}
  if(any(complete.cases(dv1)==F)) {print("Note: Your inputs include one or more NA entries. The function will ignore the rows containing these entries.")}
  dv1 <- dv1[complete.cases(dv1)]

  # Perform t test
  a <- t.test(dv1, NULL, alternative = c("two.sided"), mu, paired = FALSE, var.equal = TRUE,conf.level = 0.95)

  # compute Cohen's d
  b <- ((mean(dv1,na.rm=T)-mu)/sd(dv1,na.rm=T))

  # Produce output
  if(a$p.value < .001) {
    write_clip(allow_non_interactive = TRUE, content = paste("# one-sample t(",a$parameter,") = ",wrap.rd0(a$statistic,2),", p < .001", ", 95% CI = [",wrap.rd0(a$conf.int[1],2),", ",wrap.rd0(a$conf.int[2],2),"], d = ",wrap.rd0(b,2),sep=""))
    return(cat("# one-sample t(",a$parameter,") = ",wrap.rd0(a$statistic,2),", p < .001", ", 95% CI = [",wrap.rd0(a$conf.int[1],2),", ",wrap.rd0(a$conf.int[2],2),"], d = ",wrap.rd0(b,2),sep=""))
  }
  else {
    write_clip(allow_non_interactive = TRUE, content = paste("# one-sample t(",a$parameter,") = ",wrap.rd0(a$statistic,2),", p = ",wrap.rd(a$p.value,3),", 95% CI = [",wrap.rd0(a$conf.int[1],2),", ",wrap.rd0(a$conf.int[2],2),"], d = ",wrap.rd0(b,2),sep=""))
    return(cat("# one-sample t(",a$parameter,") = ",wrap.rd0(a$statistic,2),", p = ",wrap.rd(a$p.value,3),", 95% CI = [",wrap.rd0(a$conf.int[1],2),", ",wrap.rd0(a$conf.int[2],2),"], d = ",wrap.rd0(b,2),sep=""))
  }
}
