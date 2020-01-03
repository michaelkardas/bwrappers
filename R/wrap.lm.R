#' Linear regression
#'
#' @description Performs linear regression analyses. The function delegates
#' the primary computations to \code{\link[stats]{lm}} and
#' \code{\link[lm.beta]{lm.beta}}. In the output, R^2 is not adjusted.
#'
#' @param model The linear model
#' @param standardized A logical argument: if \code{FALSE}, the function
#' returns unstandardized coefficients; if \code{TRUE}, the function returns
#' standardized coefficients
#'
#' @seealso \code{\link[stats]{lm}}, \code{\link[lm.beta]{lm.beta}}
#'
#' @examples
#' ## Linear regression with unstandardized coefficients
#' wrap.lm(model = bdata$DV7 ~ bdata$DV5 * bdata$DV6, standardized = FALSE)
#'
#' # Linear regression with standardized coefficients
#' wrap.lm(model = bdata$DV7 ~ bdata$DV5 * bdata$DV6, standardized = TRUE)
#' @import stringr lm.beta stats
#' @export
wrap.lm <- function(model,standardized=FALSE) {

  # Error checks
  if(standardized!=FALSE&standardized!=TRUE) {return("Argument \"standardized\" must be equal to FALSE or TRUE.")}
  data <- lm(model)$model
  if(nrow(data)!=rownames(data)[nrow(data)]) {print("Note: Your inputs include one or more NA entries. The function will ignore the rows containing these entries.")}
  summary <- summary(lm.beta(lm(model)))

  df_name <- ""
  if(regexpr("\\$",rownames(summary$coefficients)[2])>0) {
    df_name <- substr(rownames(summary$coefficients)[2],1,regexpr("\\$",rownames(summary$coefficients)[2])[1]-1)
    df_name <- paste(df_name,"\\$",sep="")
  }

  p <- pf(summary$fstatistic[1], summary$fstatistic[2], summary$fstatistic[3],lower.tail = FALSE)
  if(p < .001) {
    clip <- paste("# Model: R^2 = ",wrap.rd(summary$r.squared,2),", F(",summary$fstatistic[2],", ",summary$fstatistic[3],") = ",wrap.rd0(summary$fstatistic[1],2),", p < .001","\n","\n",sep="")
  }
  if(p >= .001) {
    clip <- paste("# Model: R^2 = ",wrap.rd(summary$r.squared,2),", F(",summary$fstatistic[2],", ",summary$fstatistic[3],") = ",wrap.rd0(summary$fstatistic[1],2),", p = ",wrap.rd(p,3),"\n","\n",sep="")
  }
  temp_clip <- paste("\n",clip,sep="")

  # Unstandardized regression coefficients
  if(standardized==F) {
    for (i in 1:(nrow(summary$coefficients))) {
      clip <- paste(clip,
                    "# ",gsub(df_name,"",gsub(":"," x ",rownames(summary$coefficients)[i])),": b = ",wrap.rd0(summary$coefficients[i,1],2),", t(",summary$df[2],") = ",wrap.rd0(summary$coefficients[i,4],2),", p",if (as.numeric(summary$coefficients[i,5]) < .001) {" < .001"},if (as.numeric(summary$coefficients[i,5]) >= .001) {" = "},if (as.numeric(summary$coefficients[i,5]) >= .001) {wrap.rd(summary$coefficients[i,5],3)},"\n",
                    sep="")
    }
    clip <- paste(substr(clip,1,nchar(clip)-1))
    wrap.writeClipboard(clip)

    return(
      for (i in 1:(nrow(summary$coefficients))) {
        cat(if(i==1){temp_clip},
          "# ",gsub(df_name,"",gsub(":"," x ",rownames(summary$coefficients)[i])),": b = ",wrap.rd0(summary$coefficients[i,1],2),", t(",summary$df[2],") = ",wrap.rd0(summary$coefficients[i,4],2),", p",if (as.numeric(summary$coefficients[i,5]) < .001) {" < .001"},if (as.numeric(summary$coefficients[i,5]) >= .001) {" = "},if (as.numeric(summary$coefficients[i,5]) >= .001) {wrap.rd(summary$coefficients[i,5],3)},"\n",
          sep="")
      }
    )
  }

  # Standardized regression coefficients
  if(standardized==T) {
    for (i in 1:(nrow(summary$coefficients))) {
      if(i==1&rownames(summary$coefficients)[i]=="(Intercept)") {clip <- paste(clip,"# (Intercept): Beta = 0.00","\n",sep="")}
      if(i>1|rownames(summary$coefficients)[i]!="(Intercept)") {
        clip <- paste(clip,
                      "# ",gsub(df_name,"",gsub(":"," x ",rownames(summary$coefficients)[i])),": Beta = ",wrap.rd0(summary$coefficients[i,2],2),", t(",summary$df[2],") = ",wrap.rd0(summary$coefficients[i,4],2),", p",if (as.numeric(summary$coefficients[i,5]) < .001) {" < .001"},if (as.numeric(summary$coefficients[i,5]) >= .001) {" = "},if (as.numeric(summary$coefficients[i,5]) >= .001) {wrap.rd(summary$coefficients[i,5],3)},"\n",
                      sep="")
      }
    }
    clip <- paste(substr(clip,1,nchar(clip)-1))
    wrap.writeClipboard(clip)
    clip <- ""
    return(
      for (i in 1:(nrow(summary$coefficients))) {
        if(i==1) {clip <- temp_clip}
        if(i==1&rownames(summary$coefficients)[i]=="(Intercept)") {clip <- cat(clip,"# (Intercept): Beta = 0.00","\n",sep="")}
        if(i>1|rownames(summary$coefficients)[i]!="(Intercept)") {
          clip <- cat(clip,
                      "# ",gsub(df_name,"",gsub(":"," x ",rownames(summary$coefficients)[i])),": Beta = ",wrap.rd0(summary$coefficients[i,2],2),", t(",summary$df[2],") = ",wrap.rd0(summary$coefficients[i,4],2),", p",if (as.numeric(summary$coefficients[i,5]) < .001) {" < .001"},if (as.numeric(summary$coefficients[i,5]) >= .001) {" = "},if (as.numeric(summary$coefficients[i,5]) >= .001) {wrap.rd(summary$coefficients[i,5],3)},"\n",
                      sep="")
        }
      }
    )
  }
}
