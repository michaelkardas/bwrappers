#' ANOVA (simple main effects)
#'
#' @description Computes simple main effects for a two-way, between-subjects ANOVA.
#' The function delegates the primary computations to \code{\link[phia]{testInteractions}}.
#' Note that this function assumes categorical (i.e., unordered) independent variables
#' and fixed effects. In the output, hp2 denotes partial eta squared.
#'
#' @param dv1 Column vector containing the dependent variable
#' @param iv1,iv2 Column vectors containing the between-subjects independent variables.
#' The function will test for simple main effects of \code{iv1} separately at each level
#' of \code{iv2}.
#' @param adjustment Character string representing the method of p value adjustment
#' ("none", "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", or "fdr").
#'
#' @seealso \code{\link[phia]{testInteractions}}
#'
#' @examples
#' wrap.simple(dv1 = bdata$DV5, iv1 = bdata$IV1, iv2 = bdata$IV2)
#'
#' @import stringr effsize stats
#' @importFrom clipr write_clip
#' @export
wrap.simple <- function(dv1,iv1,iv2,adjustment="none") {

  options(scipen=999)
  x <- options('contrasts') # store original contrasts
  options(contrasts = c('contr.sum','contr.poly'))

  if (!requireNamespace("phia", quietly = TRUE)) {
    stop("This function requires first installing the phia package. Please install this package. (To ensure that the bwrappers package would remain compatible with older versions of R, the package did not attempt to download phia during the initial installation from GitHub.",
         call. = FALSE)
  }
  if (!requireNamespace("car", quietly = TRUE)) {
    stop("This function requires first installing the car package. Please install this package. (To ensure that the bwrappers package would remain compatible with older versions of R, the package did not attempt to download car during the initial installation from GitHub.",
         call. = FALSE)
  }

  requireNamespace("phia"); requireNamespace("car")

  # Error checks
  if(is.null(dv1)) {return(paste("Cannot find the column vector inputted to parameter dv1."))}
  if(is.null(iv1)) {return(paste("Cannot find the column vector inputted to parameter iv1."))}
  if(is.null(iv2)) {return(paste("Cannot find the column vector inputted to parameter iv2."))}
  if(is.null(iv1)==F) {if(is.factor(iv1)==F) {return("Error: Argument iv1 must be a factor variable.")}}
  if(is.null(iv2)==F) {if(is.factor(iv2)==F) {return("Error: Argument iv2 must be a factor variable.")}}
  if(is.null(dv1)==F) {if(is.numeric(dv1)==F) {return("Error: Argument dv1 must be a numeric variable.")}}
  if(length(adjustment)>1) {return("Adjustment parameter must have length equal to 1.")}
  if(adjustment %in% c("none", "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr")==F) {return("Error: Adjustment argument must be one of the following: none, holm, hochberg, hommel, bonferroni, BH, BY, fdr.")}
  if(is.data.frame(dv1)==T) {if(ncol(dv1)>1) {return("Error: Must enter exactly one column for dv1.")}}
  if(is.data.frame(iv1)==T) {if(ncol(iv1)>1) {return("Error: Must enter exactly one column for iv1.")}}
  if(is.data.frame(iv2)==T) {if(ncol(iv2)>1) {return("Error: Must enter exactly one column for iv2.")}}
  if(substitute(iv1)==substitute(iv2)) {return("Error: You entered the same column for iv1 and iv2.")}
  complete.cases <- complete.cases(cbind(dv1,iv1,iv2))
  if(any(complete.cases==F)) {print("Note: Your inputs include one or more NA entries. The function will ignore the rows containing these entries.")}
  dv1 <- dv1[complete.cases]
  iv1 <- iv1[complete.cases]
  iv2 <- iv2[complete.cases]

  iv2 <- factor(iv2); iv1 <- factor(iv1)

  levels_iv2 <- nlevels(iv2)

  # compute simple effects
  anovamodel <- lm(dv1 ~ iv2*iv1)
  interaction <- phia::testInteractions(anovamodel,fixed=names(anovamodel$xlevels)[1],across=names(anovamodel$xlevels)[2],adjustment=adjustment)

  # subset the dv1 for each level of the IV, so that you can analyze each level separately
  df <- data.frame(iv2,iv1,dv1)
  colnames(df) <- c("fixed","variable","dv1")
  hp2.list <- as.list(rep(0,levels_iv2))

  for (i in 1:levels_iv2) {
    hp2.list[[i]] <- interaction["Sum of Sq"][[1]][i]/(interaction["Sum of Sq"][[1]][i]+interaction["Sum of Sq"][[1]][levels_iv2+1])
  }

  print("ASSUMPTIONS: The function assumes categorical (i.e., unordered) independent variables and fixed effects. In the output, hp2 denotes partial eta squared.")
  clip <- ""
  for (i in 1:levels_iv2) {
    clip <- paste(clip,"# ",rownames(interaction)[i],": ","F(",interaction$Df[i],", ",interaction$Df[levels_iv2+1],") = ",wrap.rd0(interaction$F[i],2),", p",if(as.numeric(interaction$'Pr(>F)'[i]) < .001) {" < .001"},if(as.numeric(interaction$'Pr(>F)'[i]) >= .001) {" = "},if (as.numeric(interaction$'Pr(>F)'[i]) >= .001) {wrap.rd((interaction$'Pr(>F)'[i]),3)},", hp2 = ",wrap.rd(hp2.list[[i]],2),"\n",sep="")
  }
  clip <- substr(clip,1,nchar(clip)-1)
  write_clip(allow_non_interactive = TRUE, content = clip)
  options(contrasts= c(x$contrasts[1],x$contrasts[2])) # reset contrasts to whatever they were before calling this function

  return(
    for (i in 1:levels_iv2) {
      cat("\n","# ",rownames(interaction)[i],": ","F(",interaction$Df[i],", ",interaction$Df[levels_iv2+1],") = ",wrap.rd0(interaction$F[i],2),", p",if(as.numeric(interaction$'Pr(>F)'[i]) < .001) {" < .001"},if(as.numeric(interaction$'Pr(>F)'[i]) >= .001) {" = "},if (as.numeric(interaction$'Pr(>F)'[i]) >= .001) {wrap.rd((interaction$'Pr(>F)'[i]),3)},", hp2 = ",wrap.rd(hp2.list[[i]],2),sep="")
    }
  )
}
