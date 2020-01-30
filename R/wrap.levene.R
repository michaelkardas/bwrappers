#' Levene's test for equality of variances
#' 
#' @description Performs Levene's test for equality of variances with one
#' between-subjects factor. The function delegates the primary computations
#' to \code{\link[ez]{ezANOVA}}. Note that this function does not use
#' bootstrapping and does not apply corrections. In the output, hp2 denotes
#' partial eta squared.
#' 
#' @param dv1 Column vector containing the dependent variable
#' @param iv1 Column vector containing the independent variable
#' @param location Character string specifying the location from which
#' to compute the variance within each group (\code{"median"} or \code{"mean"})
#' 
#' @seealso \code{\link[ez]{ezANOVA}}

#' @examples
#' wrap.levene(dv1 = bdata$DV5, iv1 = bdata$IV2, location = "median")
#' 
#' @import stringr ez effsize
#' @importFrom clipr write_clip
#' @export
wrap.levene <- function(dv1,iv1,location="median") {
  options(scipen=999)
  
    # Error checks
    if(is.null(dv1)) {return(paste("Error: Cannot find the column vector inputted to parameter dv1."))}
    if(is.null(iv1)) {return(paste("Error: Cannot find the column vector inputted to parameter iv1."))}
    if(is.null(location)==T) {return(paste("Error: Must input median or mean to location parameter."))}
    if(location!="mean"&location!="median") {return(paste("Error: Must input median or mean to parameter location."))}
    if(is.null(iv1)==F) {if(is.factor(iv1)==F) {return("Error: Argument iv1 must be a factor variable.")}}
    if(is.null(dv1)==F) {if(is.numeric(dv1)==F) {return("Error: Argument dv1 must be a numeric variable.")}}
    complete.cases <- complete.cases(cbind(dv1,iv1))
    if(any(complete.cases==F)) {print("Note: Your inputs include one or more NA entries. The function will ignore the rows containing these entries.")}
    dv1 <- dv1[complete.cases]
    iv1 <- iv1[complete.cases]
  
    x <- options('contrasts') # store original contrasts
    options(contrasts = c('contr.sum','contr.poly')) 
    df <- as.data.frame(cbind(dv1,iv1,0,0,0))
    names(df) <- c("dv1","iv1","average","deviation","userAnovaIDNum0")
    for (i in 1:nrow(df)) {
      if(location=="median") {df$average[i] <- median(df$dv1[df$iv1==df$iv1[i]])}
      if(location=="mean") {df$average[i] <- mean(df$dv1[df$iv1==df$iv1[i]])}
      df$userAnovaIDNum0[i] <- i
    }
    df$userAnovaIDNum0 <- factor(df$userAnovaIDNum0)
    df$iv1 <- factor(df$iv1)
    df$deviation <- abs(df$dv1-df$average)
    ez <- ezANOVA(data=df,dv=deviation,wid=userAnovaIDNum0,between=iv1,detailed=T,type=3)
    Anova <- ez$ANOVA
    if(Anova$Effect[1]!="(Intercept)") {
      Anova <- rbind(NA,Anova); rownames(Anova) <- 1:nrow(Anova)
    }
    options(contrasts= c(x$contrasts[1],x$contrasts[2])) # reset contrasts to whatever they were before calling this function
    write_clip(allow_non_interactive = TRUE, content = 
      paste("# F(",Anova$DFn[2],", ",Anova$DFd[2],") = ",wrap.rd0(Anova$F[2],2),", p",if (as.numeric(Anova$p[2]) < .001) {" < .001"},if (as.numeric(Anova$p[2]) >= .001) {" = "},if (as.numeric(Anova$p[2]) >= .001) {wrap.rd(Anova$p[2],3)},", hp2 = ",wrap.rd(Anova$SSn[2]/(Anova$SSn[2]+Anova$SSd[2]),2),
          sep="")
    )
    return(
      cat("# F(",Anova$DFn[2],", ",Anova$DFd[2],") = ",wrap.rd0(Anova$F[2],2),", p",if (as.numeric(Anova$p[2]) < .001) {" < .001"},if (as.numeric(Anova$p[2]) >= .001) {" = "},if (as.numeric(Anova$p[2]) >= .001) {wrap.rd(Anova$p[2],3)},", hp2 = ",wrap.rd(Anova$SSn[2]/(Anova$SSn[2]+Anova$SSd[2]),2),
          sep="")
    )
}