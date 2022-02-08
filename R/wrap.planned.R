#' ANOVA (planned contrasts)
#'
#' @description Performs planned contrasts for a one-way, between-subjects ANOVA.
#' This function assumes categorical (i.e., unordered) independent variables, fixed
#' effects, and equality of variances across conditions. If variance differs significantly
#' by condition in your data, the function also displays the results of Levene's test
#' for equality of variances with centering at the median. Note that the confidence 
#' interval and Cohen's d use mean-square error to estimate variance.
#'
#' @param dv1 Column vector containing the dependent variable
#' @param iv1 Column vector containing the between-subjects independent variable
#' @param levels String vector containing two or more factor levels of the independent variable
#' @param weights Numeric vector containing the contrast weights. Must sum to +1 and -1.
#'
#' @examples
#' ## Contrast between two levels of the independent variable
#' wrap.planned(dv1 = bdata$DV5, iv1 = bdata$IV2, levels = c("PhotoA", "PhotoB"),
#' weights = c(-1, 1))
#'
#' ## Contrast between three levels of the independent variable
#' wrap.planned(dv1 = bdata$DV5, iv1 = bdata$IV2, levels = c("PhotoA", "PhotoB",
#' "PhotoC"), weights = c(-1, 0.5, 0.5))
#'
#' @import stringr lawstat
#' @importFrom clipr write_clip
#' @importFrom utils capture.output
#' @export
wrap.planned <- function(dv1,iv1,levels,weights) {

  options(scipen=999)

  # Error checks
  if(is.null(dv1)) {return(paste("Cannot find the column vector inputted to parameter dv1."))}
  if(is.null(iv1)) {return(paste("Cannot find the column vector inputted to parameter iv1."))}
  if(length(levels)!=length(weights)) {return("levels and weights parameters must have equal length.")}
  if(is.null(iv1)==F) {if(is.factor(iv1)==F) {return("Argument iv1 must be a factor variable.")}}
  if(is.null(dv1)==F) {if(is.numeric(dv1)==F) {return("Argument dv1 must be numeric.")}}
  if(length(levels)>nlevels(iv1)) {return("You entered more levels than exist in iv1.")}
  for (i in 1:length(levels)) {
    if(levels[i] %in% levels(factor(iv1)) ==F) {return(paste("Error: ",levels[i]," is not a level in iv1.",sep=""))}
  }
  if(length(levels)!=length(unique(levels))) {return("Error: You entered one or more duplicate levels.")}
  complete.cases <- complete.cases(cbind(dv1,iv1))
  if(any(complete.cases==F)) {print("Note: Your inputs include one or more NA entries. The function will ignore the rows containing these entries.")}
  dv1 <- dv1[complete.cases]
  iv1 <- iv1[complete.cases]

  positive <- 0; negative <- 0
  for (i in 1:length(weights)) {
    if(sign(weights[i])==1) {positive <- positive + weights[i]}
    if(sign(weights[i])==-1) {negative <- negative + abs(weights[i])}
  }

  if(positive!=1|negative!=1) {return("Contrast weights must sum to +1 and -1.")}
  
  # Test for equality of variance
  output <- lawstat::levene.test(dv1,iv1,location="median")
  levene_string <- ""
  if(output$p.value<=.05) {
    levene_output <- capture.output(wrap.levene(dv1,iv1,"median_do_not_write_clipboard"))
    levene_string <- paste("Note: In your data, variance differs significantly by condition (across all levels of iv1), ",substr(levene_output,3,nchar(levene_output)),".",sep="")
  }

  x <- options('contrasts') # store original contrasts
  options(contrasts = c('contr.sum','contr.poly'))
  rownames <- rownames(contrasts(iv1))
  input_contrasts <- cbind(levels,weights)
  nlevels_iv1 <- nlevels(iv1)

  # assign contrast weights to contrasts(iv1)
  for (i in 1:nlevels_iv1) {
    contrasts(iv1)[i] <- 0
    if(length(which(input_contrasts[,1]==rownames[i]))>0) {
      contrasts(iv1)[i] <- as.numeric(input_contrasts[which(input_contrasts[,1]==rownames[i]),2])
    }
  }

  # Code to prevent singularities in the matrix
  if(contrasts(iv1)[1,1]==0&contrasts(iv1)[2,1]==0) {temp1 <- which(contrasts(iv1)[,1]>0)[1]; temp2 <- which(contrasts(iv1)[temp1,]!=0)[2]; contrasts(iv1)[1,temp2] <- 1}
  for (i in 1:(nrow(contrasts(iv1))-1)) {for (j in (i+1):nrow(contrasts(iv1))) {if(contrasts(iv1)[i]==contrasts(iv1)[j]) {contrasts(iv1)[j,ncol(contrasts(iv1))] <- contrasts(iv1)[j,ncol(contrasts(iv1))]+1}}}
  if((length(iv1)-nlevels(iv1))!=aov(dv1~iv1)$df.residual) {for (i in 1:nrow(contrasts(iv1))) {contrasts(iv1)[i,ncol(contrasts(iv1))] <- contrasts(iv1)[i,ncol(contrasts(iv1))]+runif(1)}}
  for (i in 2:ncol(contrasts(iv1))) {for (j in 1:nrow(contrasts(iv1))) {contrasts(iv1)[j,i] <- runif(1)}}
  if(abs(sum(contrasts(iv1)[1:nlevels_iv1]))>10^-8) {return("Error: Weights in the contrast matrix do not sum to 0. Are you missing values in dv1 or iv1?")}

  # compute the ANOVA and contrast statistics
  lm <- summary.lm(aov(dv1~iv1)); aov <- aov(dv1~iv1); dfRES <- aov$df.residual; MSRES <- summary(aov)[[1]][["Mean Sq"]][2]
  temp <- 0; (for (i in 1:nlevels_iv1) {temp <- temp + contrasts(iv1)[i]^2/sum(iv1==rownames[i])}) # temp is computing the (1/N1+1/N2) part of SE, but generalizes this for arbitrary contrast weights
  SE <- sqrt(MSRES)*sqrt(temp)
  means <- NULL; for (i in 1:length(levels)) {means <- c(means,mean(dv1[iv1==levels[i]],na.rm=T))}; estimate <- sum(means*weights)
  t <- estimate/SE; df <- lm$df[2]; p <- 2*pt(abs(t),dfRES,lower.tail=F)
  CIlower <- estimate-SE*qt(c(.025,.975),dfRES)[2]; CIupper <- estimate+SE*qt(c(.025,.975),dfRES)[2]
  d <- estimate/sqrt(MSRES)
  options(contrasts= c(x$contrasts[1],x$contrasts[2])) # reset contrasts to whatever they were before calling this function
  if((length(iv1)-nlevels(iv1))!=dfRES) {warning("WARNING: Degrees of freedom should, but do not equal the length of iv1 minus the number of levels in iv1. Are you missing data in either iv1 or dv1?")}

  print("ASSUMPTIONS: The function assumes categorical (i.e., unordered) independent variables, fixed effects, and equality of variances across conditions. Note that the confidence interval and Cohen's d use mean-square error to estimate variance.")
  if(nchar(levene_string)>0) {print(levene_string)}
  if (p >= .001) {
    write_clip(allow_non_interactive = TRUE, content = paste("# t(",df,") = ",wrap.rd0(t,2),", p = ",wrap.rd(p,3),", 95% CIdifference = [",wrap.rd0(CIlower,2),", ",wrap.rd0(CIupper,2),"], d = ",wrap.rd0(d,2),sep=""))
    return(cat("\n","# t(",df,") = ",wrap.rd0(t,2),", p = ",wrap.rd(p,3),", 95% CIdifference = [",wrap.rd0(CIlower,2),", ",wrap.rd0(CIupper,2),"], d = ",wrap.rd0(d,2),sep=""))
  }

  if (p < .001) {
    write_clip(allow_non_interactive = TRUE, content = paste("# t(",df,") = ",wrap.rd0(t,2),", p < .001, 95% CIdifference = [",wrap.rd0(CIlower,2),", ",wrap.rd0(CIupper,2),"], d = ",wrap.rd0(d,2),sep=""))
    return(cat("\n","# t(",df,") = ",wrap.rd0(t,2),", p < .001, 95% CIdifference = [",wrap.rd0(CIlower,2),", ",wrap.rd0(CIupper,2),"], d = ",wrap.rd0(d,2),sep=""))
  }
}
