#' T test (independent samples)
#'
#' @description Performs independent-samples t tests. The function delegates
#' the primary computations to \code{\link[stats]{t.test}}. If variance differs
#' significantly by condition in your data, the function also displays the results
#' of Levene's test for equality of variances with centering at the median.
#'
#' @param dv1 Column vector containing the dependent variable
#' @param iv1 Column vector containing the independent variable
#' @param var.equal A logical argument: If TRUE, the function assumes equality of variances across conditions; if FALSE, the function does not assume equality of variances
#'
#' @seealso \code{\link[stats]{t.test}}
#'
#' @examples
#' wrap.t.ind(dv1 = bdata$DV5, iv1 = bdata$IV1)
#'
#' @import effsize stringr stats lawstat
#' @importFrom clipr write_clip
#' @importFrom utils capture.output
#' @export
wrap.t.ind  <- function(dv1, iv1, var.equal = T) {
  options(scipen=999)

  # Error checks
  if(is.null(dv1)) {return(paste("Error: Cannot find the column vector inputted to parameter dv1."))}
  if(is.null(iv1)) {return(paste("Error: Cannot find the column vector inputted to parameter iv1."))}
  if(is.null(iv1)==F) {if(is.factor(iv1)==F) {return("Error: Argument iv1 must be a factor variable.")}}
  if(is.null(dv1)==F) {if(is.numeric(dv1)==F) {return("Error: Argument dv1 must be a numeric variable.")}}
  if(is.null(var.equal)==T) {return("Error: var.equal must equal TRUE or FALSE.")}
  if(is.null(var.equal)==F) {if(length(var.equal)>1) {return("Error: var.equal must equal TRUE or FALSE.")}}
  if(is.null(var.equal)==F) {if(var.equal!=T&var.equal!=F) {return("Error: var.equal must equal TRUE or FALSE.")}}
  complete.cases <- complete.cases(cbind(dv1,iv1))
  if(any(complete.cases==F)) {print("Note: Your inputs include one or more NA entries. The function will ignore the rows containing these entries.")}
  dv1 <- dv1[complete.cases]
  iv1 <- iv1[complete.cases]

  iv1 <- factor(iv1)
  if(nlevels(iv1)!=2){return(print("# Error: iv1 must have exactly 2 levels"))}

  # collect condition names
  condition1 <- levels(iv1)[1]; condition2 <- levels(iv1)[2]

  # transform the inputs so that you have one df representing iv1_level1 and one df representing iv1_level2
  df <- data.frame(iv1,dv1)
  iv1.list <- as.list(rep(0,2))
  for (j in 1:2) {
    iv1.list[[j]] <- subset(df,iv1==levels(iv1)[j])
  }

  x <- as.numeric(unlist(iv1.list[[1]][2])) # DV data points associated with iv1_level1
  y <- as.numeric(unlist(iv1.list[[2]][2])) # DV data points associated with iv1_level2

  # Test for equality of variance
  output <- lawstat::levene.test(dv1,iv1,location="median")
  levene_string <- ""
  if(output$p.value<=.05) {
    levene_output <- capture.output(wrap.levene(dv1,iv1,"median_do_not_write_clipboard"))
    levene_string <- (paste("Note: In your data, variance differs significantly by condition, ",substr(levene_output,3,nchar(levene_output)),".",sep=""))
  }

  # t-test results
  a <- t.test(x, y, alternative = "two.sided", mu = 0, paired = FALSE, var.equal = var.equal,conf.level = 0.95)

  # cohen's d
  b <- (cohen.d(x,y,na.rm=T)$estimate)

  if(var.equal==F) {
    print("Note: The outputs below assume unequal variances across conditions.")
  }
  if(var.equal==T) {
    print("Note: The outputs below assume equal variances across conditions.")
  }
  if(nchar(levene_string)>0) {
    print(levene_string)
  }
  
  if(var.equal==F) {
    if(a$p.value < .001) {
      write_clip(allow_non_interactive = TRUE, content = paste("# t(",wrap.rd0(a$parameter,2),") = ",wrap.rd0(a$statistic,2),", p < .001, 95% CIdifference = [",wrap.rd0(a$conf.int[1],2),", ",wrap.rd0(a$conf.int[2],2),"], d = ",wrap.rd0(b,2),sep=""))
      return(cat("\n","# t(",wrap.rd0(a$parameter,2),") = ",wrap.rd0(a$statistic,2),", p < .001, 95% CIdifference = [",wrap.rd0(a$conf.int[1],2),", ",wrap.rd0(a$conf.int[2],2),"], d = ",wrap.rd0(b,2),sep=""))
    }

    else {
      write_clip(allow_non_interactive = TRUE, content = paste("# t(",wrap.rd0(a$parameter,2),") = ",wrap.rd0(a$statistic,2),", p = ",wrap.rd(a$p.value,3),", 95% CIdifference = [",wrap.rd0(a$conf.int[1],2),", ",wrap.rd0(a$conf.int[2],2),"], d = ",wrap.rd0(b,2),sep=""))
      return(cat("\n","# t(",wrap.rd0(a$parameter,2),") = ",wrap.rd0(a$statistic,2),", p = ",wrap.rd(a$p.value,3),", 95% CIdifference = [",wrap.rd0(a$conf.int[1],2),", ",wrap.rd0(a$conf.int[2],2),"], d = ",wrap.rd0(b,2),sep=""))
    }
  }

  if(var.equal==T) {
    if(a$p.value < .001) {
      write_clip(allow_non_interactive = TRUE, content = paste("# t(",a$parameter,") = ",wrap.rd0(a$statistic,2),", p < .001, 95% CIdifference = [",wrap.rd0(a$conf.int[1],2),", ",wrap.rd0(a$conf.int[2],2),"], d = ",wrap.rd0(b,2),sep=""))
      return(cat("\n","# t(",a$parameter,") = ",wrap.rd0(a$statistic,2),", p < .001, 95% CIdifference = [",wrap.rd0(a$conf.int[1],2),", ",wrap.rd0(a$conf.int[2],2),"], d = ",wrap.rd0(b,2),sep=""))
    }

    else {
      write_clip(allow_non_interactive = TRUE, content = paste("# t(",a$parameter,") = ",wrap.rd0(a$statistic,2),", p = ",wrap.rd(a$p.value,3),", 95% CIdifference = [",wrap.rd0(a$conf.int[1],2),", ",wrap.rd0(a$conf.int[2],2),"], d = ",wrap.rd0(b,2),sep=""))
      return(cat("\n","# t(",a$parameter,") = ",wrap.rd0(a$statistic,2),", p = ",wrap.rd(a$p.value,3),", 95% CIdifference = [",wrap.rd0(a$conf.int[1],2),", ",wrap.rd0(a$conf.int[2],2),"], d = ",wrap.rd0(b,2),sep=""))
    }
  }
}
