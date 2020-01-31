#' Linear regression
#'
#' @description Performs linear regression analyses. The function delegates
#' the primary computations to \code{\link[stats]{lm}} and
#' \code{\link[sjstats]{std_beta}}. In the output, R^2 is not adjusted.
#'
#' @param formula The linear model
#' @param standardized A logical argument: if \code{FALSE}, the function
#' returns unstandardized coefficients; if \code{TRUE}, the function returns
#' standardized coefficients
#'
#' @seealso \code{\link[stats]{lm}}, \code{\link[sjstats]{std_beta}}
#'
#' @examples
#' ## Linear regression with unstandardized coefficients
#' wrap.lm(formula = bdata$DV7 ~ bdata$DV5 * bdata$DV6, standardized = FALSE)
#'
#' # Linear regression with standardized coefficients
#' wrap.lm(formula = bdata$DV7 ~ bdata$DV5 * bdata$DV6, standardized = TRUE)
#' @import stringr stats
#' @importFrom clipr write_clip
#' @importFrom sjstats std_beta
#' @export
wrap.lm <- function(formula,standardized=FALSE) {

  options(scipen=999)
  
  # Error checks
  if(standardized!=FALSE&standardized!=TRUE) {return("Argument standardized must be equal to FALSE or TRUE.")}
  print(paste("Note: Your contrast options are currently set to unordered = ",options('contrasts')$contrasts[[1]],", ordered = ",options('contrasts')$contrasts[[2]],".",sep=""))
  data <- lm(formula)$model
  if(nrow(data)!=rownames(data)[nrow(data)]) {print("Note: Your inputs include one or more NA entries. The function will ignore the rows containing these entries.")}
  summary <- summary(lm(formula))
  confint_unstandard <- confint(lm(formula))
  summary_standard <- std_beta(lm(formula))
  if(nrow(summary$coefficients)==0) {return("Error: You did not enter a model.")}
  if(length(summary_standard$term)==0) {return("Error: You did not enter a model.")}

  df_name <- ""
  if(nrow(summary$coefficients)>1) {
    if(regexpr("\\$",rownames(summary$coefficients)[2])>0) {
      df_name <- substr(rownames(summary$coefficients)[2],1,regexpr("\\$",rownames(summary$coefficients)[2])[1]-1)
      df_name <- paste(df_name,"\\$",sep="")
    }
  }

  if(nrow(summary$coefficients)==1) {
    if(regexpr("\\$",rownames(summary$coefficients)[1])>0) {
      df_name <- substr(rownames(summary$coefficients)[1],1,regexpr("\\$",rownames(summary$coefficients)[1])[1]-1)
      df_name <- paste(df_name,"\\$",sep="")
    }
  }
  
  temp_clip <- ""
  if(is.null(summary$fstatistic)==F) {
    p <- pf(summary$fstatistic[1], summary$fstatistic[2], summary$fstatistic[3],lower.tail = FALSE)
    if(p < .001) {
      clip <- paste("# Model: R^2 = ",wrap.rd(summary$r.squared,2),", F(",summary$fstatistic[2],", ",summary$fstatistic[3],") = ",wrap.rd0(summary$fstatistic[1],2),", p < .001","\n","\n",sep="")
    }
    if(p >= .001) {
      clip <- paste("# Model: R^2 = ",wrap.rd(summary$r.squared,2),", F(",summary$fstatistic[2],", ",summary$fstatistic[3],") = ",wrap.rd0(summary$fstatistic[1],2),", p = ",wrap.rd(p,3),"\n","\n",sep="")
    }
    temp_clip <- paste("\n",clip,sep="")
  }

  # Unstandardized regression coefficients
  if(standardized==F) {
    for (i in 1:(nrow(summary$coefficients))) {
      clip <- paste(clip,
                    "# ",gsub(df_name,"",gsub(":"," x ",rownames(summary$coefficients)[i])),": b = ",wrap.rd0(summary$coefficients[i,1],2),", SE = ",wrap.rd0(summary$coefficients[i,2],2),", t(",summary$df[2],") = ",wrap.rd0(summary$coefficients[i,3],2),", p",if (as.numeric(summary$coefficients[i,4]) < .001) {" < .001"},if (as.numeric(summary$coefficients[i,4]) >= .001) {" = "},if (as.numeric(summary$coefficients[i,4]) >= .001) {wrap.rd(summary$coefficients[i,4],3)},", 95% CI = [",wrap.rd0(confint_unstandard[i,1],2),", ",wrap.rd0(confint_unstandard[i,2]),"]","\n",
                    sep="")
    }
    clip <- paste(substr(clip,1,nchar(clip)-1))
    write_clip(allow_non_interactive = TRUE, content = clip)

    return(
      for (i in 1:(nrow(summary$coefficients))) {
        cat(if(i==1){temp_clip},
            "# ",gsub(df_name,"",gsub(":"," x ",rownames(summary$coefficients)[i])),": b = ",wrap.rd0(summary$coefficients[i,1],2),", SE = ",wrap.rd0(summary$coefficients[i,2],2),", t(",summary$df[2],") = ",wrap.rd0(summary$coefficients[i,3],2),", p",if (as.numeric(summary$coefficients[i,4]) < .001) {" < .001"},if (as.numeric(summary$coefficients[i,4]) >= .001) {" = "},if (as.numeric(summary$coefficients[i,4]) >= .001) {wrap.rd(summary$coefficients[i,4],3)},", 95% CI = [",wrap.rd0(confint_unstandard[i,1],2),", ",wrap.rd0(confint_unstandard[i,2]),"]","\n",
            sep="")
      }
    )
  }
  
  # Standardized regression coefficients
  if(standardized==T) {
    if(rownames(summary(lm(formula))$coefficients)[1]=="(Intercept)") {
      clip <- paste(clip,"# (Intercept): Beta = 0.00","\n",sep="")
    }
    for (i in 1:(length(summary_standard$term))) {
      row <- which(rownames(summary$coefficients)==summary_standard$term[i])
      clip <- paste(clip,
                    "# ",gsub(df_name,"",gsub(":"," x ",summary_standard$term[i])),": Beta = ",wrap.rd0(summary_standard$std.estimate[i],2),", SE = ",wrap.rd0(summary_standard$std.error[i],2),", t(",summary$df[2],") = ",wrap.rd0(summary$coefficients[row,3],2),", p",if (as.numeric(summary$coefficients[row,4]) < .001) {" < .001"},if (as.numeric(summary$coefficients[row,4]) >= .001) {" = "},if (as.numeric(summary$coefficients[row,4]) >= .001) {wrap.rd(summary$coefficients[row,4],3)},", 95% CI = [",wrap.rd0(summary_standard$conf.low[i],2),", ",wrap.rd0(summary_standard$conf.high[i],2),"]","\n",
                    sep="")
    }
    clip <- paste(substr(clip,1,nchar(clip)-1))
    write_clip(allow_non_interactive = TRUE, content = clip)
    if(rownames(summary(lm(formula))$coefficients)[1]=="(Intercept)") {
      temp_clip <- paste(temp_clip,"# (Intercept): Beta = 0.00","\n",sep="")
    }
    return(
      for (i in 1:(length(summary_standard$term))) {
        row <- which(rownames(summary$coefficients)==summary_standard$term[i])
        cat(if(i==1) {temp_clip},
            "# ",gsub(df_name,"",gsub(":"," x ",summary_standard$term[i])),": Beta = ",wrap.rd0(summary_standard$std.estimate[i],2),", SE = ",wrap.rd0(summary_standard$std.error[i],2),", t(",summary$df[2],") = ",wrap.rd0(summary$coefficients[row,3],2),", p",if (as.numeric(summary$coefficients[row,4]) < .001) {" < .001"},if (as.numeric(summary$coefficients[row,4]) >= .001) {" = "},if (as.numeric(summary$coefficients[row,4]) >= .001) {wrap.rd(summary$coefficients[row,4],3)},", 95% CI = [",wrap.rd0(summary_standard$conf.low[i],2),", ",wrap.rd0(summary_standard$conf.high[i],2),"]","\n",
            sep="")
        
      }
    )
  }
}
