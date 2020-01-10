#' Generate columns
#'
#' @description Searches the data frame for pairs of columns whose names are identical
#' except for specific strings that you've embedded within them--such as "T1" versus "T2"
#' or "Participant1" versus "Participant2--and then generates new columns by computing
#' sums, differences, or means between pairs of corresponding columns.
#'
#' @param df The data frame
#' @param string1,string2 Character strings embedded in the names of corresponding columns
#' @param operation Character string to specify the operation (\code{"sum"}, \code{"difference"}, or
#' \code{"mean"})
#' @param newString Character string to replace \code{string1} and \code{string2} in the
#' outputted column/s. By default, \code{newString} equals the operation (\code{"sum"},
#' \code{"difference"}, or \code{"mean"}).
#'
#' @examples
#' ## Computing differences between T1 responses and T2 responses
#' wrap.generate(df = bdata, string1 = "T1", string2 = "T2", operation = "difference")
#'
#' @import stringr
#' @export
wrap.generate <- function(df,string1,string2,operation, newString = operation) {

  # Error checks
  if(toString(substitute(df)) %in% ls(.GlobalEnv)==F) {return("Error: Cannot find argument df in the Global Environment.")}
  if(is.null(string1)==T|is.null(string2)==T) {return("Error: Must enter character strings for parameters string1 and string2")}
  if(is.character(string1)==F|is.character(string2)==F) {return("Error: Must enter character strings for parameters string1 and string2")}
  if(operation!="difference"&operation!="sum"&operation!="mean") {return("Error: operation must be set equal to difference, sum, or mean.")}
  if(length(string1)>1|length(string2)>1) {return("Error: Parameters string1 and string2 must have one element each.")}
  if(string1==string2) {return("Error: You entered the same character strings for arguments string1 and string2.")}
  if(string1==newString|string2==newString) {return("Error: newString must be distinct from both string1 and string2.")}

  fargs <- as.list(match.call(expand.dots = TRUE)); for (i in 1:length(fargs)) {fargs[i] <- gsub("*","",fargs[i],fixed=T); fargs[i] <- gsub("-","",fargs[i],fixed=T); fargs[i] <- gsub(" ", "", fargs[i],fixed=T)}
  originalcolumns <- ncol(df); originalcolnames <- colnames(df); newcolumns = 0; overwritten = 0

  # Add double back-slash when the first character is a period, to ensure it searches correctly.
  for (i in 1:length(string1)) {
    if(substr(string1[[i]],1,1)==".") {
      string1[[i]] <- paste("\\",string1[[i]],sep="")
    }
  }

  for (i in 1:length(string2)) {
    if(substr(string2[[i]],1,1)==".") {
      string2[[i]] <- paste("\\",string2[[i]],sep="")
    }
  }

  # Find matching columns and compute sums, differences, or means
  colnames <- colnames(df)
  colnames1 <- grep(string1,colnames)
  colnames2 <- grep(string2,colnames)
  duplicates <- NULL
  non_numeric <- NULL
  if(length(colnames1)>0&length(colnames2)>0) {
    for (i in 1:length(colnames1)) {
      for (j in 1:length(colnames2)) {
        if(gsub(string1,string2,colnames[colnames1[i]])==colnames[colnames2[j]]) {

          if(sum(colnames==colnames[colnames1[i]])>1) {duplicates <- c(duplicates,colnames[colnames1[i]])}
          if(sum(colnames==colnames[colnames2[j]])>1) {duplicates <- c(duplicates,colnames[colnames2[j]])}
          if(is.numeric(df[[colnames1[i]]])==F) {non_numeric <- c(non_numeric,colnames[colnames1[i]])}
          if(is.numeric(df[[colnames2[j]]])==F) {non_numeric <- c(non_numeric,colnames[colnames2[j]])}

          if(sum(colnames==colnames[colnames1[i]])==1&sum(colnames==colnames[colnames2[j]])==1&is.numeric(df[[colnames1[i]]])==T&is.numeric(df[[colnames2[j]]])==T) {
            newcolumns = newcolumns+1
            if(operation=="mean") {
              if(gsub(string1,newString,colnames[colnames1[i]]) %in% colnames) {overwritten <- overwritten + 1}
              df[gsub(string1,newString,colnames[colnames1[i]])] <- (df[,colnames1[i]]+df[,colnames2[j]])/2
            }
            if(operation=="difference") {
              if(gsub(string1,newString,colnames[colnames1[i]]) %in% colnames) {overwritten <- overwritten + 1}
              df[gsub(string1,newString,colnames[colnames1[i]])] <- (df[,colnames1[i]]-df[,colnames2[j]])
            }
            if(operation=="sum") {
              if(gsub(string1,newString,colnames[colnames1[i]]) %in% colnames) {overwritten <- overwritten + 1}
              df[gsub(string1,newString,colnames[colnames1[i]])] <- (df[,colnames1[i]]+df[,colnames2[j]])
            }
          }
        }
      }
    }
  }

  duplicates <- unique(duplicates); non_numeric <- unique(non_numeric)

  if(is.null(duplicates)==F) {print(paste("Note: Your data frame has multiple columns with the name/s ",paste(duplicates,collapse=", "),", and so these column/s will not be included in this operation.",sep=""))}
  if(is.null(non_numeric)==F) {print(paste("Note: Column/s ",paste(non_numeric,collapse=", ")," are not numeric and so will not be included in this operation.",sep=""))}

  if(operation=="mean"|operation=="sum") {
    if(overwritten>0) {print(paste("Note: The function is overwriting ",overwritten," column/s that already existed in the data frame.",sep=""))}
    print(paste("Note: Generated ",newcolumns," columns.",sep=""))
  }
  if(operation=="difference") {
    if(overwritten>0) {print(paste("Note: The function is overwriting",overwritten," column/s that already existed in the data frame.",sep=""))}
    if(newcolumns>0) {print(paste("Note: Generated ",newcolumns," column/s. Computed differences as ",string1," minus ",string2,".",sep=""))}
    if(newcolumns==0) {print(paste("Note: Generated ",newcolumns," column/s.",sep=""))}
  }
  names(df)[1:originalcolumns] <- originalcolnames
  assign(fargs[[2]],df,.GlobalEnv)
}
