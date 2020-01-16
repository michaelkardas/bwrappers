#' Chi-square tests
#'
#' @description Performs one-way goodness-of-fit and two-way contingency tests. The
#' function delegates the primary operations to \code{\link[stats]{chisq.test}}.
#'
#' @param dv1 Column vector containing the categorical dependent variable
#' @param iv1 Column vector containing the categorical independent variable (contingency tests, only)
#' @param p A vector of probabilities representing expected cell frequencies (goodness-of-fit tests, only). By default, the function assumes equal expected cell frequencies for all levels of the dependent variable.
#' @param correct A logical argument: If FALSE, the function does not apply Yates's correction for 2×2 contingency tables; if TRUE, the function applies Yates's correction for 2×2 contingency tables.
#'
#' @seealso \code{\link[stats]{chisq.test}}
#'
#' @examples
#' ## One-way goodness-of-fit test
#' wrap.chi(dv1 = bdata$DV2)
#'
#' ## Two-way contingency test
#' wrap.chi(dv1 = bdata$DV2, iv1 = bdata$IV2)
#'
#' @import stringr stats
#' @importFrom clipr write_clip
#' @export
wrap.chi <- function(dv1,iv1=NULL,p=rep(1/nlevels(factor(dv1)),nlevels(factor(dv1))),correct=F) {
  options(scipen=999)

  # Error checks
  if(is.null(dv1)) {return(paste("Cannot find the column vector inputted to parameter dv1."))}
  if(is.null(substitute(iv1))==F&is.null(iv1)==T) {return(paste("Cannot find the column vector inputted to parameter iv1."))}
  if(is.factor(dv1)==F) {return("Must enter a factor variable for dv1.")}
  if(is.null(iv1)==F) {
    if(is.factor(iv1)==F) {return("Must enter a factor variable for iv1.")}
  }
  if(is.null(iv1)==T) {if(any(is.na(dv1))) {dv1 <- dv1[complete.cases(dv1)]; print("Note: Your inputs include one or more NA entries. The function will ignore the rows containing these entries.")}}
  if(is.null(iv1)==F) {
    complete.cases <- complete.cases(cbind(dv1,iv1))
    if(any(complete.cases==F)) {print("Note: Your inputs include one or more NA entries. The function will ignore the rows containing these entries.")}
    dv1 <- dv1[complete.cases]
    iv1 <- iv1[complete.cases]
  }
  linebreak <- F

  if(correct!=T&correct!=F) {return("Argument correct must be TRUE or FALSE.")}

  n <- sum(dv1!=""&is.na(dv1)==F&is.null(dv1)==F)
  dv1 <- factor(dv1); if(is.null(iv1)==F) {iv1 <- factor(iv1)}
  nlevels_dv1 <- nlevels(dv1)

  # Two-way contingency tests
  if(is.null(iv1)==F) {
    df <- data.frame(dv1,iv1)

    string <- "matrix <- aggregate(dv1 ~ iv1, data = df, function(x) c("
    for (i in 1:(nlevels(dv1)-1)) {
      string <- paste(string,"dv1",i," = sum(x==levels(dv1)[",i,"]), ",sep="")
    }
    for (i in (nlevels(dv1)):(nlevels(dv1))) {
      string <- paste(string,"dv1",i," = sum(x==levels(dv1)[",i,"])))",sep="")
    }
    eval(parse(text=string))
    b <- chisq.test(matrix[[2]],correct=correct)
    if(nrow(matrix[[2]])==2&ncol(matrix[[2]])==2) {
      if(correct==F) {linebreak <- T; print("Note that the function has NOT applied Yates's correction.")}
      if(correct==T) {linebreak <- T; print("Note that the function HAS applied Yates's correction.")}
    }
  }

  # One-way goodness of fit tests
  if(is.null(iv1)==T) {
    df <- dv1
    matrix <- matrix(data=NA,nrow=nlevels_dv1)

    for (i in 1:nlevels_dv1) {
      matrix[i] <- sum(dv1==levels(dv1)[i])
    }
    b <- chisq.test(matrix,correct=correct,p=p)
  }

  if(b$p.value < .001) {
    write_clip(allow_non_interactive = TRUE, content = paste("# X2(",b$parameter,", N = ",n,") = ",wrap.rd0(b$statistic,2),", p < .001",sep=""))
    return(cat(if(linebreak==T) {"\n"},"# X2(",b$parameter,", N = ",n,") = ",wrap.rd0(b$statistic,2),", p < .001",sep=""))
  }
  else {
    write_clip(allow_non_interactive = TRUE, content = paste("# X2(",b$parameter,", N = ",n,") = ",wrap.rd0(b$statistic,2),", p = ",wrap.rd(b$p.value,3),sep=""))
    return(cat(if(linebreak==T) {"\n"},"# X2(",b$parameter,", N = ",n,") = ",wrap.rd0(b$statistic,2),", p = ",wrap.rd(b$p.value,3),sep=""))
  }
}
