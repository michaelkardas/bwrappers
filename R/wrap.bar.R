#' Bar plots
#'
#' @description Creates bar plots for numerical dependent variables, adds error bars,
#' and prints descriptive statistics in a summary table. The function creates plots
#' for up to 3 factors total, including 0-1 within-subjects factors and 0-3 between-
#' subjects factors. The function delegates the primary computations to \code{\link[ggplot2]{ggplot}}.
#' Error bars are +/-1 SE.
#'
#' @param dv1 Column vector containing the between-subjects dependent variable OR
#' multiple column vectors containing the within-subjects dependent variables
#' @param iv1,iv2,iv3 Column vectors containing the independent variables
#' @param reposition Numeric vector to rearrange columns in the summary table and
#' thus reposition factors within the plot itself. For example, \code{reposition = c(1, 3, 2)}
#' reverses the order of the second and third columns in the summary table and
#' repositions the corresponding factors within the plot. The length of the reposition
#' vector must equal the total number of within-subjects and between-subjects factors.
#' @param rename1,rename2,rename3 String vectors to rename the factor levels in the summary
#' table and thus rename factor levels within the plot itself. For example,
#' \code{rename1 = c("Close Friend", "Distant Stranger")} renames the levels in the first
#' column of the summary table and renames the corresponding factor levels within the plot.
#' (Note that the function applies the \code{reposition} argument to the summary table before applying
#' the \code{rename} arguments.)
#' @param reorder1,reorder2,reorder3 String vectors to reorder the factor levels in the
#' summary table and thus reorder factor levels within the plot itself. For example,
#' \code{reorder1 = c("Stranger", "Friend")} reorders the levels in the first column of the
#' summary table and reorders the corresponding factor levels within the plot. (Note that
#' the function applies the \code{reposition} and \code{rename} arguments to the summary table
#' before applying the \code{reorder} arguments.)
#' @param ylim Numeric vector containing lower and upper y-axis limits
#' @param ymajor Numeric argument representing spacing of y-axis tick marks
#' @param ylab Character string containing the y-axis label
#' @param xlab Character string containing the x-axis label
#' @param title Character string containing the plot title
#' @param size.axis.text.y,size.axis.text.x,size.title,size.panel.title,size.legend.text Numeric
#' arguments containing font sizes
#'
#' @seealso \code{\link[ggplot2]{ggplot}}
#'
#' @examples
#' ## Bar plot with 1 within-subjects factor
#' wrap.bar(dv1 = bdata[c(6, 8)])
#'
#' ## Bar plot with 2 between-subjects factors
#' wrap.bar(dv1 = bdata$DV5, iv1 = bdata$IV1, iv2 = bdata$IV2)
#'
#' ## Bar plot with 1 within-subjects factor & 2 between-subjects factors
#' wrap.bar(dv1 = bdata[c(6, 8)], iv1 = bdata$IV1, iv2 = bdata$IV3)
#'
#' @import ggplot2 stringr ggsignif
#' @export
wrap.bar <- function(dv1,iv1=NULL,iv2=NULL,iv3=NULL,reposition=NULL,
                     rename1=NULL,rename2=NULL,rename3=NULL,reorder1=NULL,
                     reorder2=NULL,reorder3=NULL,ylim=NULL,ymajor=NULL,
                     ylab=NULL,xlab=NULL,title=NULL,size.axis.text.y = 12,
                     size.axis.text.x=16,size.title=24,size.panel.title = 12,
                     size.legend.text=14) {

  if(is.data.frame(dv1)==T) {if(ncol(dv1)==1) {dv1 <- as.numeric(unlist(dv1))}}

  # Error checks
  if(is.null(iv1)==T&is.null(iv2)==F) {return("Error: Must input iv1 before entering iv2")}
  if(is.null(iv2)==T&is.null(iv3)==F) {return("Error: Must input iv2 before entering iv3")}
  if(is.null(iv1)==T&is.null(iv3)==F) {return("Error: Must input iv1 before entering iv3")}
  if(is.null(iv1)==F) {if(is.data.frame(iv1)) {if(ncol(iv1)>1) {return("Error: Must input one column maximum for iv1.")}}}
  if(is.null(iv2)==F) {if(is.data.frame(iv2)) {if(ncol(iv2)>1) {return("Error: Must input one column maximum for iv2.")}}}
  if(is.null(iv3)==F) {if(is.data.frame(iv3)) {if(ncol(iv3)>1) {return("Error: Must input one column maximum for iv3.")}}}
  if(is.null(iv1)==F) {if(is.factor(iv1)==F) {return("Error: Must input a factor variable for iv1.")}}
  if(is.null(iv2)==F) {if(is.factor(iv2)==F) {return("Error: Must input a factor variable for iv2.")}}
  if(is.null(iv3)==F) {if(is.factor(iv3)==F) {return("Error: Must input a factor variable for iv3.")}}
  if(is.data.frame(dv1)==T&is.null(iv1)==F&is.null(iv2)==F&is.null(iv3)==F) {return("Error: You inputted 1 within-subjects factor and 3 between-subjects factors. Must input 3 factors maximum.")}
  if(is.null(ylim)==F) {if(length(ylim)!=2) {return("Error: ylim must have two elements (e.g., ylim = c(0,10)).")}}
  if(is.null(ylim)==T&is.null(ymajor)==F) {return("Error: Must input argument ylim (y-axis limits) to specify ymajor (spacing of y-axis tick marks).")}
  
  # Formatting
  if(is.null(title)==T & grepl("\\$",toString(substitute(dv1)))==T& is.data.frame(dv1)==F) {title <- toString(substring(deparse(substitute(as.numeric(dv1))),str_locate_all(pattern=coll('$'),deparse(substitute(as.numeric(dv1))))[[1]][1]+1,nchar(deparse(substitute(as.numeric(dv1))))-1))}
  if(is.null(title)==T & grepl("\\[",toString(substitute(dv1)))==T& is.data.frame(dv1)==F) {df_temp <- get(substr(deparse(substitute(dv1)),1,which(strsplit(deparse(substitute(dv1)), "")[[1]]=="[")-1),envir = .GlobalEnv); title <- names(df_temp)[substitute(dv1)[[3]]]}
  if(is.null(title)==T & is.data.frame(dv1)==T) {title <- NULL}
  if(is.null(title)==F) {title <- gsub("\\_"," ",title)}
  color_theme1="black"
  color_theme2="white"
  color_font = color_theme1; color_border = color_theme1; color_grid = color_theme1; color_legend_text = color_theme1; color_facet = color_theme1
  dv1_ <- NULL; iv_1 <- NULL; iv_2 <- NULL; iv_3 <- NULL

  # Between-subjects data
  if(is.data.frame(dv1)==F) {
    if(is.numeric(dv1)==F) {return("Error: Must enter numeric variable for dv1.")}

    if (is.null(iv1)==T) {
      df <- as.numeric(dv1)
      dv1_ <- substring(deparse(substitute(as.numeric(dv1))),str_locate_all(pattern=coll('$'),deparse(substitute(as.numeric(dv1))))[[1]][1]+1)
    }

    if (is.null(iv1)==F&is.null(iv2)==T) {
      iv_1 <- substring(deparse(substitute(iv1)),str_locate_all(pattern=coll('$'),deparse(substitute(iv1)))[[1]][1]+1)
      dv1_ <- substring(deparse(substitute(as.numeric(dv1))),str_locate_all(pattern=coll('$'),deparse(substitute(as.numeric(dv1))))[[1]][1]+1)
      df <- data.frame(as.numeric(dv1),iv1)

      # Prevent the function from counting missing data points when later computing SE
      for (i in 1:nrow(df)) {
        if(is.na(df[i,1])) {df[i,2] <- NA}
      }
    }

    if (is.null(iv2)==F) {

      if(is.null(iv3)==T) {
        df <- data.frame(as.numeric(dv1),iv1,iv2)
        iv_1 <- substring(deparse(substitute(iv1)),str_locate_all(pattern=coll('$'),deparse(substitute(iv1)))[[1]][1]+1)
        iv_2 <- substring(deparse(substitute(iv2)),str_locate_all(pattern=coll('$'),deparse(substitute(iv2)))[[1]][1]+1)
        dv1_ <- substring(deparse(substitute(dv1)),str_locate_all(pattern=coll('$'),deparse(substitute(dv1)))[[1]][1]+1)

        # Prevent the function from counting missing data points when later computing SE
        for (i in 1:nrow(df)) {
          if(is.na(df[i,1])) {df[i,2] <- NA; df[i,3] <- NA}
        }
      }

      if(is.null(iv3)==F) {
        iv_1 <- substring(deparse(substitute(iv1)),str_locate_all(pattern=coll('$'),deparse(substitute(iv1)))[[1]][1]+1)
        iv_2 <- substring(deparse(substitute(iv2)),str_locate_all(pattern=coll('$'),deparse(substitute(iv2)))[[1]][1]+1)
        iv_3 <- substring(deparse(substitute(iv3)),str_locate_all(pattern=coll('$'),deparse(substitute(iv2)))[[1]][1]+1)
        dv1_ <- substring(deparse(substitute(dv1)),str_locate_all(pattern=coll('$'),deparse(substitute(dv1)))[[1]][1]+1)
        df <- data.frame(as.numeric(dv1),iv1,iv2,iv3)

        # Prevent the function from counting missing data points when later computing SE
        for (i in 1:nrow(df)) {
          if(is.na(df[i,1])) {df[i,2] <- NA; df[i,3] <- NA; df[i,4] <- NA}
        }
      }
    }

    if(is.null(iv1)==F) {iv1 <- factor(iv1)}
    if(is.null(iv2)==F) {iv2 <- factor(iv2)}
    if(is.null(iv3)==F) {iv3 <- factor(iv3)}

    # One DV, no between-subjects IVs
    if (is.null(iv1)==T) {
      summary <- data.frame(matrix(0,nrow=1,ncol=3))
      colnames(summary) <- c("dv1","M","SE")
      summary[1,1] <- substr(dv1_,1,nchar(dv1_)-1)
      summary[1,2] <- mean(df,na.rm=T)
      summary[1,3] <- sd(df,na.rm=T)/sqrt(sum(is.na(df)==F))
      fill <- scale_fill_grey(start=0.3,end=0.9,labels=paste(levels(factor(summary[[1]]))))
      if(is.null(ymajor)==F) {scale_y_continuous <- scale_y_continuous(breaks=seq(ylim[1],ylim[2],ymajor),expand = c(0,0))}
      if(is.null(ymajor)==T) {scale_y_continuous <- scale_y_continuous(expand = c(0,0))}
      axis.text.x = element_blank()
    }

    # One DV, one between-subjects IV
    if (is.null(iv1)==F&is.null(iv2)==T) {
      summary <- data.frame(matrix(0,nrow=nlevels(factor(iv1)),ncol=3))
      colnames(summary) <- c(iv_1,"M","SE")
      counter <- 1
      for (j in 1:nlevels(factor(iv1))) {
        summary[counter,1] <- levels(factor(iv1))[j]
        counter <- counter+1
      }
      counter <- 1

      for (i in 1:(nlevels(factor(iv1)))) {
        summary[i,2] <- mean(df[df[[2]]==summary[i,1],colnames(df)[1]],na.rm=T)
        summary[i,3] <- sd(df[df[[2]]==summary[i,1],colnames(df)[1]],na.rm=T)/sqrt(sum(df[[2]]==summary[i,1],na.rm=T))
      }

      fill <- scale_fill_grey(start=0.3,end=0.9,labels=paste(levels(factor(summary[[1]]))))

      if(is.null(ymajor)==F) {scale_y_continuous <- scale_y_continuous(breaks=seq(ylim[1],ylim[2],ymajor),expand = c(0,0))}
      if(is.null(ymajor)==T) {scale_y_continuous <- scale_y_continuous(expand = c(0,0))}

      axis.text.x = element_text(color= "black",angle=0,hjust=0.5,size=size.axis.text.x)
    }

    # One DV, two between-subjects IVs
    if (is.null(iv2)==F&is.null(iv3)==T) {
      summary <- data.frame(matrix(0,nrow=nlevels(factor(iv1))*nlevels(factor(iv2)),ncol=4))
      colnames(summary) <- c(iv_1,iv_2,"M","SE")
      counter <- 1
      for (j in 1:nlevels(factor(iv2))) {
        for (k in 1:nlevels(factor(iv1))) {
          summary[counter,1] <- levels(factor(iv1))[k]
          counter <- counter+1
        }
      }

      counter <- 1
      for (j in 1:nlevels(factor(iv2))) {
        for (k in 1:nlevels(factor(iv1))) {
          summary[counter,2] <- levels(factor(iv2))[j]
          counter <- counter+1
        }
      }
      for (i in 1:(nlevels(factor(iv1))*nlevels(factor(iv2)))) {
        summary[i,3] <- mean(df[df[[2]]==summary[i,1]&df[[3]]==summary[i,2],colnames(df)[1]],na.rm=T)
        summary[i,4] <- sd(df[df[[2]]==summary[i,1]&df[[3]]==summary[i,2],colnames(df)[1]],na.rm=T)/sqrt(sum(df[[2]]==summary[i,1]&df[[3]]==summary[i,2],na.rm=T))
      }

      fill <- scale_fill_grey(start=0.3,end=0.9,labels=paste(levels(factor(summary[[1]]))))

      if(is.null(ymajor)==F) {scale_y_continuous <- scale_y_continuous(breaks=seq(ylim[1],ylim[2],ymajor),expand = c(0,0))}
      if(is.null(ymajor)==T) {scale_y_continuous <- scale_y_continuous(expand = c(0,0))}

      axis.text.x = element_text(color= "black",angle=0,hjust=0.5,size=size.axis.text.x)
    }

    # One DV, three between-subjects IVs
    if (is.null(iv3)==F) {
      summary <- data.frame(matrix(0,nrow=nlevels(factor(iv1))*nlevels(factor(iv2))*nlevels(factor(iv3)),ncol=5))
      colnames(summary) <- c(iv_1,iv_2,iv_3,"M","SE")
      counter <- 1
      for (h in 1:nlevels(factor(iv3))) {
        for (j in 1:nlevels(factor(iv2))) {
          for (k in 1:nlevels(factor(iv1))) {
            summary[counter,1] <- levels(factor(iv1))[k]
            counter <- counter+1
          }
        }
      }

      counter <- 1
      for (h in 1:nlevels(factor(iv3))) {
        for (j in 1:nlevels(factor(iv2))) {
          for (k in 1:nlevels(factor(iv1))) {
            summary[counter,2] <- levels(factor(iv2))[j]
            counter <- counter+1
          }
        }
      }

      counter <- 1
      for (h in 1:nlevels(factor(iv3))) {
        for (j in 1:nlevels(factor(iv2))) {
          for (k in 1:nlevels(factor(iv1))) {
            summary[counter,3] <- levels(factor(iv3))[h]
            counter <- counter+1
          }
        }
      }

      for (i in 1:(nlevels(factor(iv1))*nlevels(factor(iv2))*nlevels(factor(iv3)))) {
        summary[i,4] <- mean(df[df[[2]]==summary[i,1]&df[[3]]==summary[i,2]&df[[4]]==summary[i,3],colnames(df)[1]],na.rm=T)
        summary[i,5] <- sd(df[df[[2]]==summary[i,1]&df[[3]]==summary[i,2]&df[[4]]==summary[i,3],colnames(df)[1]],na.rm=T)/sqrt(sum(df[[2]]==summary[i,1]&df[[3]]==summary[i,2]&df[[4]]==summary[i,3],na.rm=T))
      }

      fill <- scale_fill_grey(start=0.3,end=0.9,labels=paste(levels(factor(summary[[1]]))))

      if(is.null(ymajor)==F) {scale_y_continuous <- scale_y_continuous(breaks=seq(ylim[1],ylim[2],ymajor),expand = c(0,0))}
      if(is.null(ymajor)==T) {scale_y_continuous <- scale_y_continuous(expand = c(0,0))}

      axis.text.x = element_text(color= "black",angle=0,hjust=0.5,size=size.axis.text.x)

    }
  }

  # Within-subjects data
  if(is.data.frame(dv1)==T) {
    if(is.null(iv1)==F) {iv1 <- factor(iv1)}
    if(is.null(iv2)==F) {iv2 <- factor(iv2)}
    if(is.null(iv3)==F) {return("Error: You entered four factors (1 within-subjects, 3 between-subjects). This function can plot up to 3 factors total.")}
    names <- names(dv1)
    dv1 <- data.frame(dv1)
    names(dv1) <- names

    for (i in 1:ncol(dv1)) {
      if(class(dv1[[i]])!="numeric") {
        return("Error: Dependent variables must be numeric.")
      }
    }

    # One within-subjects DV, no between-subjects DVs
    if(is.null(iv1)==T) {
      dv1_num <- ncol(dv1)
      summary <- data.frame(matrix(rep(0,3*dv1_num),ncol=3))
      colnames(summary) <- c("dv1","M","SE")

      for (i in 1:nrow(summary)) {summary[i,1] <- colnames(dv1)[i]}
      for (i in 1:nrow(summary)) {summary[i,2] <- mean(dv1[[i]],na.rm=T)}
      for (i in 1:nrow(summary)) {summary[i,3] <- sd(dv1[[i]],na.rm=T)/sqrt(sum(is.na(dv1[[i]])==F))}

      if(is.null(ymajor)==F) {scale_y_continuous <- scale_y_continuous(breaks=seq(ylim[1],ylim[2],ymajor),expand = c(0,0))}
      if(is.null(ymajor)==T) {scale_y_continuous <- scale_y_continuous(expand = c(0,0))}
      summary$dv1 <- factor(summary$dv1,levels=unique(as.character(summary$dv1)))

      fill <- scale_fill_grey(levels(summary[[1]]),start=0.3,end=0.9)

      axis.text.x = element_text(color= "black",angle=0,hjust=0.5,size=size.axis.text.x)
    }

    # One within-subjects DV, one between-subjects DV
    if(is.null(iv1)==F&is.null(iv2)==T) {
      n_dv1 <- ncol(dv1)
      iv1 <- factor(iv1)
      nlevels_iv1 <- nlevels(iv1)
      summary <- data.frame(matrix(c(rep(0,4*n_dv1*nlevels_iv1)),ncol=4))
      colnames(summary) <- c("iv1","dv1","M","SE")

      # fill in condition
      for (i in 1:nrow(summary)) {
        summary[i,1] <- levels(iv1)[ceiling(i/n_dv1)]
      }

      # fill in time variable
      for (i in 1:nrow(summary)) {
        summary[i,2] <- colnames(dv1)[(i %% n_dv1) + 1]
      }

      # fill in Ms
      for (i in 1:nrow(summary)) {
        summary[i,3] <- mean(dv1[which(iv1==summary[i,1]),summary[i,2]],na.rm=T)
      }

      # fill in SEs
      for (i in 1:nrow(summary)) {
        summary[i,4] <- sd(dv1[which(iv1==summary[i,1]),summary[i,2]],na.rm=T)/sqrt(sum(iv1==summary[i,1]&!is.na(dv1[[summary[i,2]]]),na.rm=T))
      }

      summary$dv1 <- factor(summary$dv1,levels=unique(as.character(summary$dv1)))

      if(is.null(ymajor)==F) {scale_y_continuous <- scale_y_continuous(breaks=seq(ylim[1],ylim[2],ymajor),expand = c(0,0))}
      if(is.null(ymajor)==T) {scale_y_continuous <- scale_y_continuous(expand = c(0,0))}

      axis.text.x = element_text(color= "black",angle=0,hjust=0.5,size=size.axis.text.x)
      fill <- scale_fill_grey(start=0.3,end=0.9,labels=paste(levels(factor(summary[[2]]))))
    }

    # One within-subjects DV, two between-subjects DVs
    if(is.null(iv1)==F&is.null(iv2)==F) {

      iv1_num <- nlevels(iv1)
      iv1_levels <- levels(iv1)
      iv2_num <- nlevels(iv2)
      iv2_levels <- levels(iv2)
      dv1_num <- ncol(dv1)

      # create summary data frame
      summary <- data.frame(matrix(rep(0,5*ncol(dv1)*iv1_num*iv2_num),ncol=5)); colnames(summary) <- c("iv1","iv2","dv1","M","SE")

      # fill in iv1
      for (i in 1:nrow(summary)) {summary[i,1] <- iv1_levels[ceiling(i/(nrow(summary)/iv1_num))]}

      # fill in iv2
      for (i in 1:nrow(summary)) {summary[i,2] <- iv2_levels[((ceiling(i/(nrow(summary)/(iv1_num*iv2_num)))) %% iv2_num)+1]}

      # fill in dv1
      for (i in 1:nrow(summary)) {summary[i,3] <- colnames(dv1)[(i %% dv1_num) + 1]}

      # fill in Ms
      for (i in 1:nrow(summary)) {
        summary[i,4] <- mean(dv1[which(iv1==summary[i,1]&iv2==summary[i,2]),summary[i,3]],na.rm=T)
      }

      # fill in SEs
      for (i in 1:nrow(summary)) {
        summary[i,5] <- sd(dv1[which(iv1==summary[i,1]&iv2==summary[i,2]),summary[i,3]],na.rm=T)/sqrt(sum(iv1==summary[i,1]&iv2==summary[i,2]&!is.na(dv1[[summary[i,3]]]),na.rm=T))
      }

      fill <- scale_fill_grey(labels=paste(levels(factor(summary[[3]]))))

      summary$dv1 <- factor(summary$dv1,levels=unique(as.character(summary$dv1)))

      if(is.null(ymajor)==F) {scale_y_continuous <- scale_y_continuous(breaks=seq(ylim[1],ylim[2],ymajor),expand = c(0,0))}
      if(is.null(ymajor)==T) {scale_y_continuous <- scale_y_continuous(expand = c(0,0))}

      axis.text.x = element_text(color= "black",angle=0,hjust=0.5,size=size.axis.text.x)
    }
  }

  ### Rename entries in the summary table and create the legend
  # Replace underscores and periods with spaces.
  if(ncol(summary)>2) {
    for (j in 1:(ncol(summary)-2)) {
      if(ncol(summary)==3) {summary <- summary[order(as.character(summary[,1])),]}
      if(ncol(summary)==4) {summary <- summary[order(as.character(summary[,1]),as.character(summary[,2])),]}
      if(ncol(summary)==5) {summary <- summary[order(as.character(summary[,1]),as.character(summary[,2]),as.character(summary[,3])),]}
      if(ncol(summary)==6) {summary <- summary[order(as.character(summary[,1]),as.character(summary[,2]),as.character(summary[,3]),as.character(summary[,4])),]}
      if(ncol(summary)==7) {summary <- summary[order(as.character(summary[,1]),as.character(summary[,2]),as.character(summary[,3]),as.character(summary[,4]),as.character(summary[,5])),]}
      summary[[j]] <- as.factor(summary[[j]])
      summary[[j]] <- factor(summary[[j]],levels=unique(summary[[j]]))
      for (i in 1:nlevels(summary[[j]])) {
        levels(summary[[j]])[i] <- gsub("_"," ",levels(summary[[j]])[i])
        levels(summary[[j]])[i] <- gsub("\\."," ",levels(summary[[j]])[i])
      }
    }
  }

  # For plots including within-subjects data, ensure that the dv1 column comes first in the summary table
  if(ncol(data.frame(dv1))>1) {
    dv_col <- which(colnames(summary)=="dv1")
    if(dv_col>1) {
      summary <- summary[,c(dv_col,c(1:(dv_col-1)),c((dv_col+1):ncol(summary)))]
    }
  }

  # Reposition columns in the summary table based on the "reposition" argument
  total_levels <- 0
  if(ncol(as.data.frame(dv1))>1) {total_levels <- total_levels+1}
  if(is.null(iv1)==F) {total_levels <- total_levels+1}
  if(is.null(iv2)==F) {total_levels <- total_levels+1}
  if(is.null(iv3)==F) {total_levels <- total_levels+1}
  if(is.null(reposition)==F) {
    if(total_levels<2) {return("Error: Cannot reposition factors in graphs that display fewer than 2 factors.")}
    if(length(reposition)!=total_levels) {return(paste("Error: The reposition vector should have length ",total_levels,".",sep=""))}
    if(any(sort(reposition)!=c(1:total_levels))) {return(paste("Error: The reposition vector should contain the integers 1 through ",total_levels,", representing the within-subjects and/or between-subjects factors in the summary table.",sep=""))}
    summary[,1:total_levels] <- summary[,reposition]
  }

  # Rename levels within the columns of the summary data frame
  if(is.null(rename1)==F) {
    if(any(duplicated(rename1))) {return("Error: Your rename1 argument contains duplicate elements.")}
    if(length(rename1)!=length(levels(as.factor(summary[[1]])))) {temp <- toString(paste(levels(summary[[1]]),sep=", ")); return(paste("Error: You entered the wrong number of levels in the rename1 parameter. The current levels are ",temp,". (Note that this function applies reposition, then rename, then reorder.)",sep=""))}
    summary[[1]] <- as.factor(summary[[1]])
    original <- NULL; original <- paste(levels(summary[[1]]),sep=", ")
    levels(summary[[1]]) <- rename1
    revised <- NULL; revised <- paste(levels(summary[[1]]),sep=", ")
    for (i in 1:length(original)) {
      cat("# Column #1, Revision #",i,": ",original[i]," --> ",revised[i],"\n",sep="")
    }
  }

  if(is.null(rename2)==F) {
    if(any(duplicated(rename2))) {return("Error: Your rename2 argument contains duplicate elements.")}
    if(length(rename2)!=length(levels(as.factor(summary[[2]])))) {temp <- toString(paste(levels(summary[[2]]),sep=", ")); return(paste("Error: You entered the wrong number of levels in the rename2 parameter. The current levels are ",temp,". (Note that this function applies reposition, then rename, then reorder.)",sep=""))}
    summary[[2]] <- as.factor(summary[[2]])
    original <- NULL; original <- paste(levels(summary[[2]]),sep=", ")
    levels(summary[[2]]) <- rename2
    revised <- NULL; revised <- paste(levels(summary[[2]]),sep=", ")
    for (i in 1:length(original)) {
      cat("# Column #2, Revision #",i,": ",original[i]," --> ",revised[i],"\n",sep="")
    }
  }

  if(is.null(rename3)==F) {
    if(any(duplicated(rename3))) {return("Error: Your rename3 argument contains duplicate elements.")}
    if(length(rename3)!=length(levels(as.factor(summary[[3]])))) {temp <- toString(paste(levels(summary[[3]]),sep=", ")); return(paste("Error: You entered the wrong number of levels in the rename3 parameter. The current levels are ",temp,". (Note that this function applies reposition, then rename, then reorder.)",sep=""))}
    summary[[3]] <- as.factor(summary[[3]])
    original <- NULL; original <- paste(levels(summary[[3]]),sep=", ")
    levels(summary[[3]]) <- rename3
    revised <- NULL; revised <- paste(levels(summary[[3]]),sep=", ")
    for (i in 1:length(original)) {
      cat("# Column #3, Revision #",i,": ",original[i]," --> ",revised[i],"\n",sep="")
    }
  }

  if(is.null(reorder1)==F) {
    if(any(duplicated(reorder1))) {return("Error: Your reorder1 argument contains duplicate elements.")}
    if(length(reorder1)!=length(levels(as.factor(summary[[1]])))) {temp <- toString(paste(levels(summary[[1]]),sep=", ")); return(paste("Error: You entered the wrong number of levels in the reorder1 parameter. The current levels are ",temp,". (Note that this function applies reposition, then rename, then reorder.)",sep=""))}
    for (i in 1:length(reorder1)) {if(is.element(reorder1[i],levels(as.factor(summary[[1]])))==F) {temp <- toString(paste(levels(summary[[1]]),sep=", ")); return(paste("Error: One or more levels in your reorder1 parameter is not a level in this factor. The current levels are ",temp,". (Note that this function applies reposition, then rename, then reorder.)",sep=""))}}
  }

  if(is.null(reorder2)==F) {
    if(any(duplicated(reorder2))) {return("Error: Your reorder2 argument contains duplicate elements.")}
    if(length(reorder2)!=length(levels(as.factor(summary[[2]])))) {temp <- toString(paste(levels(summary[[2]]),sep=", ")); return(paste("Error: You entered the wrong number of levels in the reorder2 parameter. The current levels are ",temp,". (Note that this function applies reposition, then rename, then reorder.)",sep=""))}
    for (i in 1:length(reorder2)) {if(is.element(reorder2[i],levels(as.factor(summary[[2]])))==F) {temp <- toString(paste(levels(summary[[2]]),sep=", ")); return(paste("Error: One or more levels in your reorder2 parameter is not a level in this factor. The current levels are ",temp,". (Note that this function applies reposition, then rename, then reorder.)",sep=""))}}
  }

  if(is.null(reorder3)==F) {
    if(any(duplicated(reorder3))) {return("Error: Your reorder3 argument contains duplicate elements.")}
    if(length(reorder3)!=length(levels(as.factor(summary[[3]])))) {temp <- toString(paste(levels(summary[[3]]),sep=", ")); return(paste("Error: You entered the wrong number of levels in the reorder3 parameter. The current levels are ",temp,". (Note that this function applies reposition, then rename, then reorder.)",sep=""))}
    for (i in 1:length(reorder3)) {if(is.element(reorder3[i],levels(as.factor(summary[[3]])))==F) {temp <- toString(paste(levels(summary[[3]]),sep=", ")); return(paste("Error: One or more levels in your reorder3 parameter is not a level in this factor. The current levels are ",temp,". (Note that this function applies reposition, then rename, then reorder.)",sep=""))}}
  }

  ### Reorder the levels in each column of the summary data frame ###
  if(is.null(reorder1)==T&ncol(summary)>2) {reorder1 <- unique(summary[[1]]); null1 <- F}
  if(is.null(reorder2)==T&ncol(summary)>3) {reorder2 <- unique(summary[[2]]); null2 <- F}
  if(is.null(reorder3)==T&ncol(summary)>4) {reorder3 <- unique(summary[[3]]); null3 <- F}

  if(is.null(reorder1)==F&is.null(reorder2)==T) {
    summary <- summary[order(match(summary[[1]],reorder1)),]
  }

  if(is.null(reorder1)==F&is.null(reorder2)==F&is.null(reorder3)==T) {
    summary <- summary[order(match(summary[[1]],reorder1),match(summary[[2]],reorder2)),]
  }

  if(is.null(reorder1)==F&is.null(reorder2)==F&is.null(reorder3)==F) {
    summary <- summary[order(match(summary[[1]],reorder1),match(summary[[2]],reorder2),match(summary[[3]],reorder3)),]
  }

  if(ncol(summary)>2) {summary[[1]] <- factor(summary[[1]],levels=unique(summary[[1]]))}
  if(ncol(summary)>3) {summary[[2]] <- factor(summary[[2]],levels=unique(summary[[2]]))}
  if(ncol(summary)>4) {summary[[3]] <- factor(summary[[3]],levels=unique(summary[[3]]))}
  if(ncol(summary)>5) {summary[[4]] <- factor(summary[[4]],levels=unique(summary[[4]]))}

  ### Create legends & fill parameters
  legend_column <- NULL
  if (is.data.frame(dv1)==T&is.null(iv1)==T) {
    legend_column <- 2
    fill <- scale_fill_grey(labels=paste(levels(factor(summary[[2]]))))
  }

  if (is.data.frame(dv1)==T&is.null(iv1)==F&is.null(iv2)==T) {
    legend_column <- 2
    fill <- scale_fill_grey(labels=paste(levels(factor(summary[[2]]))))
  }

  if ((is.data.frame(dv1)==T&is.null(iv1)==F&is.null(iv2)==F)|is.data.frame(dv1)==F&is.null(iv1)==F&is.null(iv2)==F&is.null(iv3)==F) {
    legend_column <- 2
    fill <- scale_fill_grey(labels=paste(levels(factor(summary[[2]]))))
  }

  if (is.null(legend_column)==T) {
    legend_column <- 1
    fill <- scale_fill_grey(labels=paste(levels(factor(summary[[1]]))))
  }

  legend <- theme(legend.position="bottom",legend.direction="horizontal",legend.title=element_blank(),legend.text = element_text(color="black",size=size.legend.text))

  SummaryColumns <- ncol(summary)

  ### Plot summary table
  if(is.data.frame(dv1)==F&(any(class(dv1)=="numeric")|any(class(dv1)=="integer"))&is.null(iv1)==T) {
    assign(as.character(summary[1,1]),summary[1,1])
    plot <- eval(parse(text=paste("ggplot(summary,aes(x=",paste("`",as.character(summary[1,1]),"`",sep=""),",y=summary[[2]])) + coord_cartesian(ylim=ylim) + labs(title=title) + theme(plot.title=element_text(face=",shQuote("bold"),",color= ",shQuote("black"),",size=size.title))+theme(plot.title = element_text(color= ",shQuote("black"),",hjust = 0.5))+theme(axis.text.x = axis.text.x)+labs(x=element_blank())+labs(y=ylab)+labs(x=xlab)+geom_bar(size=1,stat=",shQuote("identity"),",color=",shQuote("black"),",position=position_dodge(width=0.75),width=0.75)  +fill+legend+theme(plot.background = element_rect(fill = ",shQuote("white"),", colour = ",shQuote("white"),"))+ theme(panel.grid.major.y = element_line(colour=",shQuote("black"),", size=1),panel.grid.major.x=element_blank(),panel.grid.minor.y=element_blank(),panel.grid.minor.x=element_blank())+theme(axis.text.y=element_text(size=size.axis.text.y,color= ",shQuote("black"),"))+scale_y_continuous+ theme(strip.background = element_rect(fill=",shQuote("white"),"))+theme(strip.text.x = element_text(size = size.panel.title,face=",shQuote("bold"),",color=",shQuote("black"),"))+theme(panel.background = element_rect(colour = ",shQuote("black"),", fill = ",shQuote("white"),", size = 1),panel.border = element_rect(colour = ",shQuote("black"),", fill=NA, size=1),axis.line = element_line(colour = ",shQuote("black"),",size=1))+theme(axis.ticks = element_line(colour = color_theme1,size=1))+geom_errorbar(aes(ymin=summary[[2]]-summary[[3]],ymax=summary[[2]]+summary[[3]]),width=0.15,size=0.8,colour=",shQuote("gray21"),",position=position_dodge(.75))")))
  }

  if(is.data.frame(dv1)==F&(any(class(dv1)=="numeric")|any(class(dv1)=="integer"))&is.null(iv1)==F&is.null(iv2)==T) {
    plot <- ggplot(summary,aes(x=summary[[1]],y=summary[[2]])) + coord_cartesian(ylim=ylim) + labs(title=title) + theme(plot.title=element_text(face="bold",color= "black",size=size.title))+theme(plot.title = element_text(color= "black",hjust = 0.5))+theme(axis.text.x = axis.text.x)+labs(x=xlab)+labs(y=ylab)+geom_bar(size=1,stat="identity",color="black",fill="gray80",position=position_dodge(width=0.75),width=0.75)  + guides(fill=guide_legend(title=iv_1)) + legend+theme(plot.background = element_rect(fill = "white", colour = "white"))+theme(panel.grid.major.y = element_blank(),panel.grid.major.x=element_blank(),panel.grid.minor.y=element_blank(),panel.grid.minor.x=element_blank())+theme(axis.text.y=element_text(size=size.axis.text.y,color= "black"))+scale_y_continuous+ theme(strip.background = element_rect(fill="white"))+theme(strip.text.x = element_text(size = size.panel.title,face="bold",color="black"))+theme(panel.background = element_rect(colour = "black", fill = "white", size = 1),panel.border = element_rect(colour = "black", fill=NA, size=1),axis.line = element_line(colour = "black",size=1))+theme(axis.ticks = element_line(colour = color_theme1,size=1))+
      geom_errorbar(aes(ymin=summary[[2]]-summary[[3]],ymax=summary[[2]]+summary[[3]]),width=0.15,size=0.8,colour="gray21",position=position_dodge(.75))
  }

  if(is.data.frame(dv1)==F&(any(class(dv1)=="numeric")|any(class(dv1)=="integer"))&is.null(iv1)==F&is.null(iv2)==F&is.null(iv3)==T) {
    plot <- ggplot(summary, aes(fill=summary[[1]], y=summary[[3]], x=summary[[2]]))+ coord_cartesian(ylim=ylim)+ labs(title=title) +theme(plot.title=element_text(face="bold",color= "black",size=size.title))+theme(plot.title = element_text(color= "black",hjust = 0.5))+theme(axis.text.x = axis.text.x)+labs(x=xlab)+labs(y=ylab)+geom_bar(size=1,stat="identity",color="black",position=position_dodge(width=0.75),width=0.75) + guides(fill=guide_legend(title=iv_1)) +fill+ legend+theme(plot.background = element_rect(fill = "white", colour = "white"))+theme(panel.grid.major.y = element_blank(),panel.grid.major.x=element_blank(),panel.grid.minor.y=element_blank(),panel.grid.minor.x=element_blank())+theme(axis.text.y=element_text(size=size.axis.text.y,color= "black"))+scale_y_continuous+ theme(strip.background = element_rect(fill="white"))+theme(strip.text.x = element_text(size = size.panel.title,face="bold",color="black"))+theme(panel.background = element_rect(colour = "black", fill = "white", size = 1),panel.border = element_rect(colour = "black", fill=NA, size=1),axis.line = element_line(colour = "black",size=1))+theme(axis.ticks = element_line(colour = color_theme1,size=1))+geom_errorbar(aes(ymin=summary[[3]]-summary[[4]],ymax=summary[[3]]+summary[[4]]),width=0.15,size=0.8,colour="gray21",position=position_dodge(.75))
  }

  if(is.data.frame(dv1)==F&(any(class(dv1)=="numeric")|any(class(dv1)=="integer"))&is.null(iv1)==F&is.null(iv2)==F&is.null(iv3)==F) {
    plot <- ggplot(summary, aes(fill=summary[[3]], x=summary[[2]], y=summary[[4]]))+ facet_wrap(~ summary[[1]],scales="free_x")+ coord_cartesian(ylim=ylim)+ labs(title=title) +theme(plot.title=element_text(face="bold",color= "black",size=size.title))+theme(plot.title = element_text(color= "black",hjust = 0.5))+theme(axis.text.x = axis.text.x)+labs(x=xlab)+labs(y=ylab)+geom_bar(size=1,stat="identity",color="black",position=position_dodge(width=0.75),width=0.75)+ guides(fill=guide_legend(title=iv_1)) +fill+ legend+theme(plot.background = element_rect(fill = "white", colour = "white"))+theme(panel.grid.major.y = element_blank(),panel.grid.major.x=element_blank(),panel.grid.minor.y=element_blank(),panel.grid.minor.x=element_blank())+theme(axis.text.y=element_text(size=size.axis.text.y,color= "black"))+scale_y_continuous+ theme(strip.background = element_rect(fill="white"))+theme(strip.text.x = element_text(size = size.panel.title,face="bold",color="black"))+theme(panel.background = element_rect(colour = "black", fill = "white", size = 1),panel.border = element_rect(colour = "black", fill=NA, size=1),axis.line = element_line(colour = "black",size=1))+theme(axis.ticks = element_line(colour = color_theme1,size=1))+geom_errorbar(aes(ymin=summary[[4]]-summary[[5]],ymax=summary[[4]]+summary[[5]]),width=0.15,size=0.8,colour="gray21",position=position_dodge(.75))
  }

  if(is.data.frame(dv1)==T&is.null(iv1)==T) {
    plot <- ggplot(summary, aes(y=summary[[2]], x=summary[[1]],fill=levels(summary[[1]])))+coord_cartesian(ylim=ylim)+ labs(title=title) +theme(plot.title=element_text(face="bold",color= "black",size=size.title))+theme(plot.title = element_text(color= "black",hjust = 0.5))+theme(axis.text.x = axis.text.x)+labs(x=xlab)+labs(y=ylab)+geom_bar(size=1,stat="identity",color="black",position=position_dodge(width=0.75),width=0.75)+ guides(fill=guide_legend(title=iv_1)) +fill+theme(legend.position="none",legend.title=element_blank(),legend.text = element_text(color="black",size=size.legend.text))+theme(plot.background = element_rect(fill = "white", colour = "white"))+theme(panel.grid.major.y = element_blank(),panel.grid.major.x=element_blank(),panel.grid.minor.y=element_blank(),panel.grid.minor.x=element_blank())+theme(axis.text.y=element_text(size=size.axis.text.y,color= "black"))+scale_y_continuous+ theme(strip.background = element_rect(fill="white"))+theme(strip.text.x = element_text(size = size.panel.title,face="bold",color="black"))+theme(panel.background = element_rect(colour = "black", fill = "white", size = 1),panel.border = element_rect(colour = "black", fill=NA, size=1),axis.line = element_line(colour = "black",size=1))+theme(axis.ticks = element_line(colour = color_theme1,size=1))+geom_errorbar(aes(ymin=summary[[2]]-summary[[3]],ymax=summary[[2]]+summary[[3]]),width=0.15,size=0.8,colour="gray21",position=position_dodge(.75))
  }

  if(is.data.frame(dv1)==T&is.null(iv1)==F&is.null(iv2)==T) {
    plot <- ggplot(summary, aes(fill=summary[[2]], x=summary[[1]], y=summary[[3]]))+ coord_cartesian(ylim=ylim)+ labs(title=title) +theme(plot.title=element_text(face="bold",color= "black",size=size.title))+theme(plot.title = element_text(color= "black",hjust = 0.5))+theme(axis.text.x = axis.text.x)+labs(x=xlab)+labs(y=ylab)+geom_bar(size=1,stat="identity",color="black",position=position_dodge(width=0.75),width=0.75)+ guides(fill=guide_legend(title=iv_1)) +fill+ legend+theme(plot.background = element_rect(fill = "white", colour = "white"))+theme(panel.grid.major.y = element_blank(),panel.grid.major.x=element_blank(),panel.grid.minor.y=element_blank(),panel.grid.minor.x=element_blank())+theme(axis.text.y=element_text(size=size.axis.text.y,color= "black"))+scale_y_continuous+ theme(strip.background = element_rect(fill="white"))+theme(strip.text.x = element_text(size = size.panel.title,face="bold",color="black"))+theme(panel.background = element_rect(colour = "black", fill = "white", size = 1),panel.border = element_rect(colour = "black", fill=NA, size=1),axis.line = element_line(colour = "black",size=1))+theme(axis.ticks = element_line(colour = color_theme1,size=1))+geom_errorbar(aes(ymin=summary[[3]]-summary[[4]],ymax=summary[[3]]+summary[[4]]),width=0.15,size=0.8,colour="gray21",position=position_dodge(.75))
  }

  if(is.data.frame(dv1)==T&is.null(iv1)==F&is.null(iv2)==F) {
    plot <- ggplot(summary, aes(fill=summary[[2]], x=summary[[1]], y=summary[[4]]))+ facet_wrap(~ summary[[3]],scales="free_x")+ coord_cartesian(ylim=ylim)+ labs(title=title) +theme(plot.title=element_text(face="bold",color= "black",size=size.title))+theme(plot.title = element_text(color= "black",hjust = 0.5))+theme(axis.text.x = axis.text.x)+labs(x=xlab)+labs(y=ylab)+geom_bar(size=1,stat="identity",color="black",position=position_dodge(width=0.75),width=0.75)+ guides(fill=guide_legend(title=iv_1)) +fill+ legend+theme(plot.background = element_rect(fill = "white", colour = "white"))+theme(panel.grid.major.y = element_blank(),panel.grid.major.x=element_blank(),panel.grid.minor.y=element_blank(),panel.grid.minor.x=element_blank())+theme(axis.text.y=element_text(size=size.axis.text.y,color= "black"))+scale_y_continuous+ theme(strip.background = element_rect(fill="white"))+theme(strip.text.x = element_text(size = size.panel.title,face="bold",color="black"))+theme(panel.background = element_rect(colour = "black", fill = "white", size = 1),panel.border = element_rect(colour = "black", fill=NA, size=1),axis.line = element_line(colour = "black",size=1))+theme(axis.ticks = element_line(colour = color_theme1,size=1))+geom_errorbar(aes(ymin=summary[[4]]-summary[[5]],ymax=summary[[4]]+summary[[5]]),width=0.15,size=0.8,colour="gray21",position=position_dodge(.75))
  }

  summary2 <- summary
  for (i in (ncol(summary2)-1):ncol(summary2)) {
    for (j in 1:nrow(summary2)) {
      summary2[j,i] <- wrap.rd0(as.numeric(paste(summary2[j,i])),2)
    }
  }
  rownames(summary2) <- NULL
  print(summary2)
  print("Note: Error bars are +/-1 SE.")
  return(plot)
}
