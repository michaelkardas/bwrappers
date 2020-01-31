#' Merge columns
#'
#' @description Searches the data frame for columns with identical names and then
#' merges them into one column, preserving only the single entry within each row
#' that is not blank or \code{NA}. If any row contains entries in two or more
#' identically named columns, the function returns an error and does not overwrite
#' data. After merging columns, the function assigns the revised data frame
#' directly to the Global Environment.
#'
#' @param df The data frame that the function should search for identical column
#' names
#'
#' @examples
#' wrap.merge(df = bdata)
#'
#' @import Hmisc
#' @importFrom dplyr pull
#' @export
wrap.merge <- function(df) {

  options(scipen=999)
  
  if(toString(substitute(df)) %in% ls(.GlobalEnv)==F) {return(paste("Error: Cannot find ",substitute(df)," in the Global Environment.",sep=""))}
  df_name <- toString(as.list(match.call(expand.dots = TRUE))[2][[1]])
  identical <- matrix(0,nrow=ncol(df),ncol=ncol(df))
  columnnames <- NULL

  # locate sets of identical column names
  for (i in 1:ncol(df)) {
    for (j in i:ncol(df)) {
      if ((colnames(df)[i]==colnames(df)[j])&(i!=j)) {
        identical[i,j] <- 1
        columnnames <- c(columnnames,colnames(df)[i])
      }
    }
  }

  if(sum(identical)>0) {
    # in the "identical" matrix, assign "0" to all but the first of those identical column names
    for (i in 1:(ncol(df)-1)) {
      for (j in (i+1):ncol(df)) {
        if (identical[i,j]==1) {
          identical[j,] <- 0
        }
      }
    }

    # error check: ensure that each row contains only one value that is not blank or NA per set of identical column names
    for (i in 1:nrow(identical)) {
      for (j in i:ncol(identical)) {
        if (identical[i,j]==1) {
          for (k in 1:nrow(df)) {
            if((is.na(df[k,i])==F&is.null(df[k,i])==F&df[k,i]!="")&(is.na(df[k,j])==F&is.null(df[k,j])==F&df[k,j]!="")) {return(paste("Error: Row ",k," contains values in multiple ",colnames(df)[i]," columns.",sep=""))}
          }
        }
      }
    }

    # look for the value that is not blank or NA, then assign this value to only the first of the repeated columns
    for (i in 1:nrow(identical)) {
      for (j in i:ncol(identical)) {
        if (identical[i,j]==1) {
          for (k in 1:nrow(df)) {
            if(is.na(df[k,i])==T|is.null(df[k,i])==T|df[k,i]=="") {df[k,i] <- df[k,j]}
          }

          # Convert to numeric any merged columns that only contain numbers
          if(all.is.numeric(pull(df[,i]))) {
            df[,i] <- as.numeric(pull(df[,i]))
          }
        }
      }
    }

    # delete the redundant columns
    delete <- NULL
    for (i in 1:ncol(df)) {
      for (j in i:ncol(df)) {
        if (identical[i,j]==1) {
          delete <- c(delete,j)
        }
      }
    }
    df <- df[,-delete]
  }

  # assign the updated data frame to the Global environment
  assign(df_name,df,.GlobalEnv)
  columnnames <- unique(columnnames)
  if(length(columnnames)>0) {
    print(paste("Merged ",length(columnnames)," group/s of columns: ",paste(columnnames,collapse=", "),".",sep=""))
  }
  if(length(columnnames)==0) {
    print("Note: Did not find multiple columns with identical names.")
  }
}
