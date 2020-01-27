#' Split a data frame
#'
#' @description Splits a data frame across all combinations of 1-3 independent
#' variables and then assigns the new data frames to the Global Environment.
#' The function delegates the primary computations to \code{\link[base]{split}}.
#'
#' @param df The data frame that you intend to split
#' @param iv1,iv2,iv3 Names of the columns containing the independent variables
#'
#' @seealso \code{\link[base]{split}}
#'
#' @examples
#' ## Splitting a data frame by 1 independent variable
#' wrap.split(df = bdata, iv1 = IV1)
#'
#' ## Splitting a data frame by 2 independent variables
#' wrap.split(df = bdata, iv1 = IV1, iv2 = IV2)
#' @import stringr
#' @export
wrap.split <- function(df, iv1, iv2 = NULL, iv3 = NULL) {

  options(scipen=999)
  
  if(is.null(substitute(iv2))==T) {
    colnames <- c((substitute(iv1)))
  }
  
  if(is.null(substitute(iv2))==F&is.null(substitute(iv3))==T) {
    colnames <- c((substitute(iv1)),(substitute(iv2)))
  }
  
  if(is.null(substitute(iv2))==F&is.null(substitute(iv3))==F) {
    colnames <- c((substitute(iv1)),(substitute(iv2)),(substitute(iv3)))
  }
  
  if(toString(substitute(df)) %in% ls(.GlobalEnv)==F) {return(paste("Error: Cannot find ",substitute(df)," in the Global Environment.",sep=""))}
  if(is.null(substitute(iv1))==F) {if(is.character(substitute(iv1))) {iv1 <- noquote(iv1)}}
  if(is.null(substitute(iv2))==F) {if(is.character(substitute(iv2))) {iv2 <- noquote(iv2)}}
  if(is.null(substitute(iv3))==F) {if(is.character(substitute(iv3))) {iv3 <- noquote(iv3)}}

  # Error checks
  if(is.null(substitute(iv1))==F&is.null(substitute(iv2))==F) {
    if(substitute(iv1)==substitute(iv2)) {return("Error: You entered one or more duplicate arguments.")}
  }
  if(is.null(substitute(iv1))==F&is.null(substitute(iv2))==F&is.null(substitute(iv3))==F) {
    if(substitute(iv1)==substitute(iv2)) {return("Error: You entered one or more duplicate arguments.")}
    if(substitute(iv1)==substitute(iv3)) {return("Error: You entered one or more duplicate arguments.")}
    if(substitute(iv2)==substitute(iv3)) {return("Error: You entered one or more duplicate arguments.")}
  }
  if(is.null(substitute(iv1))==T&(is.null(substitute(iv2))==F|is.null(substitute(iv3))==F)) {return("Must enter iv1 before iv2 or iv3.")}
  if(is.null(substitute(iv2))==T&is.null(substitute(iv3))==F) {return("Must enter iv2 before iv3.")}
  if(as.character(substitute(iv1)) %in% names(df) == F) {return(paste("Cannot find column ",substitute(iv1)," in the data frame.",sep=""))}
  if(is.null(substitute(iv2))==F) {if(as.character(substitute(iv2)) %in% names(df) == F) {return(paste("Cannot find column ",substitute(iv2)," in the data frame.",sep=""))}}
  if(is.null(substitute(iv3))==F) {if(as.character(substitute(iv3)) %in% names(df) == F) {return(paste("Cannot find column ",substitute(iv3)," in the data frame.",sep=""))}}
  levels <- NULL
  if(is.null(substitute(iv1))==F&is.null(substitute(iv2))==F&is.null(substitute(iv3))==T) {
    levels1 <- levels(df[[substitute(iv1)]])
    levels2 <- levels(df[[substitute(iv2)]])
    levels <- c(levels1,levels2)
    if(length(unique(levels))<length(levels)) {return("Error: The factors you inputted have overlapping levels. Please modify the factor levels so that they are nonoverlapping.")}
  }
  if(is.null(substitute(iv1))==F&is.null(substitute(iv2))==F&is.null(substitute(iv3))==F) {
    levels1 <- levels(df[[substitute(iv1)]])
    levels2 <- levels(df[[substitute(iv2)]])
    levels3 <- levels(df[[substitute(iv3)]])
    levels <- c(levels1,levels2,levels3)
    if(length(unique(levels))<length(levels)) {return("Error: The factors you inputted have overlapping levels. Please modify the factor levels so that they are nonoverlapping.")}
  }
  if(is.null(substitute(iv1))==F) {
    if(any(is.na(df[[substitute(iv1)]]))) {print(paste("Note: Column ",substitute(iv1)," is missing values in one or more rows. These rows will not be included in data frames that split by ",substitute(iv1),".",sep=""))}
  }
  if(is.null(substitute(iv2))==F) {
    if(any(is.na(df[[substitute(iv2)]]))) {print(paste("Note: Column ",substitute(iv2)," is missing values in one or more rows. These rows will not be included in data frames that split by ",substitute(iv2),".",sep=""))}
  }
  if(is.null(substitute(iv3))==F) {
    if(any(is.na(df[[substitute(iv3)]]))) {print(paste("Note: Column ",substitute(iv3)," is missing values in one or more rows. These rows will not be included in data frames that split by ",substitute(iv3),".",sep=""))}
  }
  
  # split by iv1
  try(A <- split(df, factor(eval(parse(text=paste(substitute(df),"$",substitute(iv1),sep=""))))), silent=T)
  names(A) <- paste0(substitute(df),".", names(A))
  list2env(A, envir=.GlobalEnv)

  if (is.null(substitute(iv2))==F) {

    # split by iv2
    B <- split(df, factor(eval(parse(text=paste(substitute(df),"$",substitute(iv2),sep="")))))
    names(B) <- paste0(substitute(df),".", names(B))
    list2env(B, envir=.GlobalEnv)

    for (i in 1:length(A)) {

      # split by iv1 and iv2
      C <- split(data.frame(A[i]),factor(eval(parse(text=paste(names(A)[i],"$",substitute(iv2),sep="")))))
      names(C) <- paste0((names(A)[i]),".", names(C))

      for (j in 1:length(C)) {
        colnames(C[[j]]) <- substring(colnames(C[[j]]),nchar(names(A)[i])+2)
        rownames(C[[j]]) <- 1:nrow(C[[j]])
      }
      list2env(C, envir=.GlobalEnv)
    }
  }

  # split by iv3
  if (is.null(substitute(iv2))==F & is.null(substitute(iv3))==F) {

    D <- split(df, factor(eval(parse(text=paste(substitute(df),"$",substitute(iv3),sep="")))))
    names(D) <- paste0(substitute(df),".", names(D))
    list2env(D, envir=.GlobalEnv)

    for (i in 1:length(A)) {

      # split by iv1 and iv2
      C <- split(data.frame(A[i]),factor(eval(parse(text=paste(names(A)[i],"$",substitute(iv2),sep="")))))
      names(C) <- paste0(names(A)[i],".", names(C),sep="")

      for (j in 1:length(C)) {
        colnames(C[[j]]) <- substring(colnames(C[[j]]),nchar(names(A)[i])+2)
        rownames(C[[j]]) <- 1:nrow(C[[j]])
      }

      # split by iv1 and iv3
      X <- split(data.frame(A[i]),factor(eval(parse(text=paste(names(A)[i],"$",substitute(iv3),sep="")))))
      names(X) <- paste0(names(A)[i],".", names(X),sep="")

      for (j in 1:length(X)) {
        colnames(X[[j]]) <- substring(colnames(X[[j]]),nchar(names(A)[i])+2)
        rownames(X[[j]]) <- 1:nrow(X[[j]])
      }

      for (j in 1:length(names(C))) {

        # split by iv1, iv2, and iv3
        E <- split(data.frame(C[j]),factor(eval(parse(text=paste(names(C)[j],"$",substitute(iv3),sep="")))))
        names(E) <- paste0(names(C)[j],".", names(E),sep="")

        for (k in 1:length(names(E))) {
          colnames(E[[k]]) <- substring(colnames(E[[k]]),nchar(names(C)[j])+2)
          rownames(E[[k]]) <- 1:nrow(E[[k]])
        }
        list2env(E, envir=.GlobalEnv)
        list2env(X, envir=.GlobalEnv)
      }
    }

    for (i in 1:length(B)) {
      # split by iv2 and iv3
      G <- split(data.frame(B[i]),factor(eval(parse(text=paste(names(B)[i],"$",substitute(iv3),sep="")))))
      names(G) <- paste0(names(B)[i],".", names(G),sep="")

      for (j in 1:length(G)) {
        colnames(G[[j]]) <- substring(colnames(G[[j]]),nchar(names(B)[i])+2)
        rownames(G[[j]]) <- 1:nrow(G[[j]])
      }
      list2env(G, envir=.GlobalEnv)
    }
  }
  print(paste("Note: Split ",substitute(df)," across all combinations of ",paste(colnames,collapse=", "),".",sep=""))
}
