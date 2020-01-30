#' Line plots
#'
#' @description Creates line plots for numerical dependent variables, adds error bars,
#' and prints descriptive statistics in a summary table. The function creates plots for
#' up to 3 factors total, including 1 within-subjects factor (corresponding to the path
#' of each line) and 0-2 between-subjects factors. The function delegates the primary
#' computations to \code{\link[ggplot2]{ggplot}}. Error bars are ±1 SE.
#'
#' @param dv1 Multiple column vectors containing the within-subjects dependent variables
#' @param iv1,iv2 Column vectors containing the independent variables
#' @param errorbar Character string specifying the length of the error bars:
#' \code{"se"} displays +/-1 standard error; \code{"ci"} displays 95\% confidence intervals
#' without p-value adjustment
#' @param ylim Numeric vector containing lower and upper y-axis limits
#' @param ymajor Numeric argument representing spacing of y-axis tick marks
#' @param ylab Character string containing the y-axis label
#' @param xlab Character string containing the x-axis label
#' @param title Character string containing the plot title
#' @param size.axis.text.y,size.axis.text.x,size.title,size.panel.title,size.legend.text Numeric
#' arguments containing font sizes
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
#'
#' @seealso \code{\link[ggplot2]{ggplot}}
#'
#' @examples
#' ## Line plot with 1 within-subjects factor
#' wrap.line(dv1 = bdata[c(6, 8)], ylim=c(0, 10), ymajor=2)
#'
#' ## Line plot with 1 within-subjects factor & 2 between-subjects factors
#' wrap.line(dv1 = bdata[c(6, 8)], iv1 = bdata$IV1, iv2 = bdata$IV2, ylim=c(0, 10), ymajor=2)
#'
#' @import ggplot2 stringr ggsignif
#' @export
wrap.line <- function(dv1,iv1=NULL,iv2=NULL,errorbar="se",ylim=NULL,ymajor=NULL,ylab=NULL,
                      xlab=NULL,title=NULL,size.axis.text.y = 12,size.axis.text.x=16,
                      size.title=24,size.panel.title = 12,size.legend.text=14,
                      reposition=NULL,rename1=NULL,rename2=NULL,rename3=NULL,
                      reorder1=NULL,reorder2=NULL,reorder3=NULL) {

  options(scipen=999)
  
  # Error checks
  if(is.null(dv1)) {return(paste("Cannot find the column vector inputted to parameter dv1."))}
  if(is.null(substitute(iv1))==F) {if(is.null(iv1)) {return(paste("Cannot find the column vector inputted to parameter iv1."))}}
  if(is.null(substitute(iv2))==F) {if(is.null(iv2)) {return(paste("Cannot find the column vector inputted to parameter iv2."))}}
  if(is.null(iv1)==T&is.null(iv2)==F) {return("Enter iv1 before entering iv2")}
  if(is.null(iv1)==F) {if(is.data.frame(iv1)) {if(ncol(iv1)>1) {return("Error: Must input one column maximum for iv1.")}}}
  if(is.null(iv2)==F) {if(is.data.frame(iv2)) {if(ncol(iv2)>1) {return("Error: Must input one column maximum for iv2.")}}}
  if(is.data.frame(dv1)==F) {return("To plot a line graph, you must enter multiple columns for parameter dv1.")}
  if(is.data.frame(dv1)==T) {if(ncol(dv1)==1) {return("To plot a line graph, you must enter multiple columns for parameter dv1.")}}
  if(is.null(ylim)==F) {if(length(ylim)!=2) {return("Error: ylim must have two elements (e.g., ylim = c(0,10)).")}}
  if(is.null(ylim)==T&is.null(ymajor)==F) {return("Must input argument ylim (y-axis limits) to specify ymajor (spacing of y-axis tick marks).")}
  for (i in 1:ncol(dv1)) {
    if(class(dv1[[i]])!="numeric") {
      return("Error: Each column in the dv1 parameter must be numeric.")
    }
  }
  if(is.null(iv1)==F) {if(is.factor(iv1)==F) {return("Error: Must input a factor variable for iv1.")}}
  if(is.null(iv2)==F) {if(is.factor(iv2)==F) {return("Error: Must input a factor variable for iv2.")}}
  if(is.null(errorbar)) {return("Error: Parameter errorbar must equal se or ci.")}
  if(errorbar!="se"&errorbar!="ci") {return("Error: Parameter errorbar must equal se or ci. (If you tried entering three between-subjects independent variables, note that this function can plot up to 2 between-subjects independent variables maximum.)")}
  
  # Formatting
  title <- gsub("\\_"," ",title)
  color_theme1="black"
  color_theme2="white"
  color_font = color_theme1; color_border = color_theme1; color_grid = color_theme1; color_legend_text = color_theme1; color_facet = color_theme1
  iv_1 <- ""
  if(is.null(iv1)==F) {iv1 <- factor(iv1)}
  if(is.null(iv2)==F) {iv2 <- factor(iv2)}
  names <- names(dv1)
  dv1 <- data.frame(dv1)
  names(dv1) <- names

  if(is.null(iv1)==F) {iv1 <- factor(iv1)}
  if(is.null(iv1)==F) {df <- data.frame(dv1,iv1)}
  if(is.null(iv1)==T) {df <- data.frame(dv1)}
  nrepeat <- ncol(dv1)
  if(is.null(iv1)==F) {nlevels_iv <- nlevels(iv1)}
  if(is.null(iv1)==F) {iv_name <- "iv1"}
  repeat_name <- " dv1"
  iv_levels <- levels(iv1)

  # One within-subjects factor, no between-subjects factors
  if(is.null(iv1)==T) {
    summary <- data.frame(matrix(rep(0,nrepeat*4),ncol=4))
    colnames(summary) <- c(repeat_name,"M","SE","N")
    summary[,1] <- rep(colnames(dv1))
    df <- reshape(data=df,direction="long",varying=c(1:nrepeat),v.names="dv1",idvar="counter",timevar=repeat_name,times=colnames(dv1))

    for (i in 1:(nrow(summary))) {
      summary[i,2] <- mean(df[df[[1]]==summary[i,1],colnames(df)[2]],na.rm=T)
      summary[i,3] <- sd(df[df[[1]]==summary[i,1],colnames(df)[2]],na.rm=T)/sqrt(sum(df[[1]]==summary[i,1]&!is.na(dv1[[summary[i,1]]]),na.rm=T))
      summary[i,4] <- sum(df[[1]]==summary[i,1]&!is.na(dv1[[summary[i,1]]]),na.rm=T)
    }

    if(is.null(ymajor)==F) {scale_y_continuous <- scale_y_continuous(breaks=seq(ylim[1],ylim[2],ymajor),expand = c(0,0))}
    if(is.null(ymajor)==T) {scale_y_continuous <- scale_y_continuous(expand = c(0,0))}

    axis.text.x = element_text(color=color_font,angle=0,hjust=0.5,size=size.axis.text.x)
  }

  # One within-subjects factor, one between-subjects factor
  if(is.null(iv1)==F&is.null(iv2)==T) {
    summary <- data.frame(matrix(rep(0,nrepeat*nlevels_iv*5),ncol=5))
    colnames(summary) <- c(iv_name,repeat_name,"M","SE","N")
    for (i in 1:nlevels_iv) {for (j in (((i-1)*nrepeat)+1):(i*nrepeat)) {summary[j,1] <- iv_levels[i]}}
    summary[,2] <- rep(colnames(dv1))
    df <- reshape(data=df,direction="long",varying=c(1:nrepeat),v.names="dv1",idvar="counter",timevar=repeat_name,times=colnames(dv1))

    for (i in 1:(nrow(summary))) {
      summary[i,3] <- mean(df[df[[1]]==summary[i,1]&df[[2]]==summary[i,2],colnames(df)[3]],na.rm=T)
      summary[i,4] <- sd(df[df[[1]]==summary[i,1]&df[[2]]==summary[i,2],colnames(df)[3]],na.rm=T)/sqrt(sum(df[[1]]==summary[i,1]&df[[2]]==summary[i,2]&!is.na(dv1[[summary[i,2]]]),na.rm=T))
      summary[i,5] <- sum(df[[1]]==summary[i,1]&df[[2]]==summary[i,2]&!is.na(dv1[[summary[i,2]]]),na.rm=T)
    }

    if(is.null(ymajor)==F) {scale_y_continuous <- scale_y_continuous(breaks=seq(ylim[1],ylim[2],ymajor),expand = c(0,0))}
    if(is.null(ymajor)==T) {scale_y_continuous <- scale_y_continuous(expand = c(0,0))}

    axis.text.x = element_text(color=color_font,angle=0,hjust=0.5,size=size.axis.text.x)
  }

  # One within-subjects factor, two between-subjects factors
  if(is.null(iv1)==F&is.null(iv2)==F) {
    iv1_num <- nlevels(iv1)
    iv1_levels <- levels(iv1)
    iv2_num <- nlevels(iv2)
    iv2_levels <- levels(iv2)
    dv1_num <- ncol(dv1)

    # create summary data frame
    summary <- data.frame(matrix(rep(0,6*ncol(dv1)*iv1_num*iv2_num),ncol=6))
    colnames(summary) <- c("iv1","iv2","dv1","M","SE","N")

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

    # fill in Ns
    for (i in 1:nrow(summary)) {
      summary[i,6] <- sum(iv1==summary[i,1]&iv2==summary[i,2]&!is.na(dv1[[summary[i,3]]]),na.rm=T)
    }
    
    summary$dv1 <- factor(summary$dv1,levels=unique(as.character(summary$dv1)))

    if(is.null(ymajor)==F) {scale_y_continuous <- scale_y_continuous(breaks=seq(ylim[1],ylim[2],ymajor),expand = c(0,0))}
    if(is.null(ymajor)==T) {scale_y_continuous <- scale_y_continuous(expand = c(0,0))}

    axis.text.x = element_text(color=color_font,angle=0,hjust=0.5,size=size.axis.text.x)
  }

  ### Create the legend
  # First, replace underscores and periods with spaces.
  if(ncol(summary)>3) {
    for (j in 1:(ncol(summary)-3)) {
      if(ncol(summary)==4) {summary <- summary[order(as.character(summary[,1])),]}
      if(ncol(summary)==5) {summary <- summary[order(as.character(summary[,1]),as.character(summary[,2])),]}
      if(ncol(summary)==6) {summary <- summary[order(as.character(summary[,1]),as.character(summary[,2]),as.character(summary[,3])),]}
      if(ncol(summary)==7) {summary <- summary[order(as.character(summary[,1]),as.character(summary[,2]),as.character(summary[,3]),as.character(summary[,4])),]}
      if(ncol(summary)==8) {summary <- summary[order(as.character(summary[,1]),as.character(summary[,2]),as.character(summary[,3]),as.character(summary[,4]),as.character(summary[,5])),]}
      summary[[j]] <- as.factor(summary[[j]])
      summary[[j]] <- factor(summary[[j]],levels=unique(summary[[j]]))
      for (i in 1:nlevels(summary[[j]])) {
        levels(summary[[j]])[i] <- gsub("_"," ",levels(summary[[j]])[i])
        levels(summary[[j]])[i] <- gsub("\\."," ",levels(summary[[j]])[i])
      }
    }
  }

  # Ensure that the dv1 column comes first in the summary table
  dv_col <- which(colnames(summary)=="dv1"|colnames(summary)==" dv1")
  if(dv_col>1) {
    summary <- summary[,c(dv_col,c(1:(dv_col-1)),c((dv_col+1):ncol(summary)))]
  }

  # Reposition columns based on the reposition parameter
  total_levels <- 0
  if(ncol(as.data.frame(dv1))>1) {total_levels <- total_levels+1}
  if(is.null(iv1)==F) {total_levels <- total_levels+1}
  if(is.null(iv2)==F) {total_levels <- total_levels+1}
  if(is.null(reposition)==F) {
    if(total_levels<2) {return("Error: Cannot reposition factors in graphs that display fewer than 2 factors.")}
    if(any(sort(reposition)!=c(1:total_levels))) {return(paste("Error: The reposition parameter should contain the integers 1 through ",total_levels,", representing the column numbers of the summary table.",sep=""))}
    summary[,1:total_levels] <- summary[,reposition]
  }

  if(is.null(rename1)==F) {
    if(any(duplicated(rename1))) {return("Error: Your rename1 argument contains duplicate elements.")}
    if(length(rename1)!=length(levels(as.factor(summary[[1]])))) {temp <- toString(paste(levels(summary[[1]]),sep=", ")); return(paste("You entered the wrong number of levels in the rename1 parameter. The current levels are ",temp,". (Note that this function applies reposition, then rename, then reorder.)",sep=""))}
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
    if(length(rename2)!=length(levels(as.factor(summary[[2]])))) {temp <- toString(paste(levels(summary[[2]]),sep=", ")); return(paste("You entered the wrong number of levels in the rename2 parameter. The current levels are ",temp,". (Note that this function applies reposition, then rename, then reorder.)",sep=""))}
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
    if(length(rename3)!=length(levels(as.factor(summary[[3]])))) {temp <- toString(paste(levels(summary[[3]]),sep=", ")); return(paste("You entered the wrong number of levels in the rename3 parameter. The current levels are ",temp,". (Note that this function applies reposition, then rename, then reorder.)",sep=""))}
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
    if(length(reorder1)!=length(levels(as.factor(summary[[1]])))) {temp <- toString(paste(levels(summary[[1]]),sep=", ")); return(paste("You entered the wrong number of levels in the reorder1 parameter. The current levels are ",temp,". (Note that this function applies reposition, then rename, then reorder.)",sep=""))}
    for (i in 1:length(reorder1)) {if(is.element(reorder1[i],levels(as.factor(summary[[1]])))==F) {temp <- toString(paste(levels(summary[[1]]),sep=", ")); return(paste("Error: One or more levels in your reorder1 parameter is not a level in this factor. The current levels are ",temp,". (Note that this function applies reposition, then rename, then reorder.)",sep=""))}}
  }

  if(is.null(reorder2)==F) {
    if(any(duplicated(reorder2))) {return("Error: Your reorder2 argument contains duplicate elements.")}
    if(length(reorder2)!=length(levels(as.factor(summary[[2]])))) {temp <- toString(paste(levels(summary[[2]]),sep=", ")); return(paste("You entered the wrong number of levels in the reorder2 parameter. The current levels are ",temp,". (Note that this function applies reposition, then rename, then reorder.)",sep=""))}
    for (i in 1:length(reorder2)) {if(is.element(reorder2[i],levels(as.factor(summary[[2]])))==F) {temp <- toString(paste(levels(summary[[2]]),sep=", ")); return(paste("Error: One or more levels in your reorder2 parameter is not a level in this factor. The current levels are ",temp,". (Note that this function applies reposition, then rename, then reorder.)",sep=""))}}
  }

  if(is.null(reorder3)==F) {
    if(any(duplicated(reorder3))) {return("Error: Your reorder3 argument contains duplicate elements.")}
    if(length(reorder3)!=length(levels(as.factor(summary[[3]])))) {temp <- toString(paste(levels(summary[[3]]),sep=", ")); return(paste("You entered the wrong number of levels in the reorder3 parameter. The current levels are ",temp,". (Note that this function applies reposition, then rename, then reorder.)",sep=""))}
    for (i in 1:length(reorder3)) {if(is.element(reorder3[i],levels(as.factor(summary[[3]])))==F) {temp <- toString(paste(levels(summary[[3]]),sep=", ")); return(paste("Error: One or more levels in your reorder3 parameter is not a level in this factor. The current levels are ",temp,". (Note that this function applies reposition, then rename, then reorder.)",sep=""))}}
  }

  if(is.null(reorder1)==T&ncol(summary)>3) {reorder1 <- unique(summary[[1]]); null1 <- F}
  if(is.null(reorder2)==T&ncol(summary)>4) {reorder2 <- unique(summary[[2]]); null2 <- F}
  if(is.null(reorder3)==T&ncol(summary)>5) {reorder3 <- unique(summary[[3]]); null3 <- F}

  if(is.null(reorder1)==F&is.null(reorder2)==T) {
    summary <- summary[order(match(summary[[1]],reorder1)),]
  }

  if(is.null(reorder1)==F&is.null(reorder2)==F&is.null(reorder3)==T) {
    summary <- summary[order(match(summary[[1]],reorder1),match(summary[[2]],reorder2)),]
  }

  if(is.null(reorder1)==F&is.null(reorder2)==F&is.null(reorder3)==F) {
    summary <- summary[order(match(summary[[3]],reorder3),match(summary[[2]],reorder2),match(summary[[1]],reorder1)),]
  }

  if(ncol(summary)>3) {summary[[1]] <- factor(summary[[1]],levels=unique(summary[[1]]))}
  if(ncol(summary)>4) {summary[[2]] <- factor(summary[[2]],levels=unique(summary[[2]]))}
  if(ncol(summary)>5) {summary[[3]] <- factor(summary[[3]],levels=unique(summary[[3]]))}
  if(ncol(summary)>6) {summary[[4]] <- factor(summary[[4]],levels=unique(summary[[4]]))}

  ### create legend
  legend <- theme(legend.position="bottom",legend.direction="horizontal",legend.title=element_blank(),legend.text = element_text(color=color_legend_text,size=size.legend.text))

  ### Plot the summary tables
  
  # No between-subjects IVs
  if(is.null(iv1)==T) {
    if(errorbar=="se") {errorbar_multiplier = 1}
    if(errorbar=="ci") {errorbar_multiplier = qt(.975,summary[[4]]-1)}
    plot <- ggplot(summary, aes(x=summary[[1]], y=M,group=1),ylim=ylim) +  geom_point()+ coord_cartesian(ylim=ylim)+ labs(title=title) +theme(plot.title=element_text(face="bold",color=color_font,size=size.title))+theme(plot.title = element_text(color=color_font,hjust = 0.5))+theme(axis.text.x = axis.text.x)+labs(x=xlab)+labs(y=ylab) +theme(legend.position = "none")+theme(plot.background = element_rect(fill = "white", colour = "white"))+ theme(panel.grid.major.y = element_blank(),panel.grid.major.x=element_blank(),panel.grid.minor.y=element_blank(),panel.grid.minor.x=element_blank())+theme(axis.text.y=element_text(size=size.axis.text.y,color=color_font))+scale_y_continuous+ theme(strip.background = element_rect(fill="white"))+theme(strip.text.x = element_text(size = size.panel.title,face="bold",color="black"))+theme(panel.background = element_rect(colour = color_border, fill = "white", size = 1),panel.border = element_rect(colour = "black", fill=NA, size=1),axis.line = element_line(colour = "black",size=1))+theme(axis.ticks = element_line(colour = color_theme1,size=1))+
      geom_errorbar(aes(ymin=summary[[2]]-summary[[3]]*errorbar_multiplier,ymax=summary[[2]]+summary[[3]]*errorbar_multiplier),width=0.15,size=0.8,colour="gray21",linetype=1)+ geom_line(aes(color="black"),size=2,linetype=1,color="black")
  }

  # 1 between-subjects IV
  if((is.null(iv1)==F&is.null(iv2)==T)) {
    if(errorbar=="se") {errorbar_multiplier = 1}
    if(errorbar=="ci") {errorbar_multiplier = qt(.975,summary[[5]]-1)}
    plot <- ggplot(summary, aes(x=summary[[1]], y=M, group=summary[[2]],linetype=summary[[2]]),ylim=ylim) + geom_point(aes(color=summary[[2]]))+ coord_cartesian(ylim=ylim)+labs(title=title) +theme(plot.title=element_text(face="bold",color=color_font,size=size.title))+theme(plot.title = element_text(color=color_font,hjust = 0.5))+theme(axis.text.x = axis.text.x)+labs(x=xlab)+labs(y=ylab) + legend+theme(plot.background = element_rect(fill = "white", colour = "white"))+theme(panel.grid.major.y = element_blank(),panel.grid.major.x=element_blank(),panel.grid.minor.y=element_blank(),panel.grid.minor.x=element_blank())+theme(axis.text.y=element_text(size=size.axis.text.y,color=color_font))+scale_y_continuous+ theme(strip.background = element_rect(fill="white"))+theme(strip.text.x = element_text(size = size.panel.title,face="bold",color="black"))+theme(panel.background = element_rect(colour = color_border, fill = "white", size = 1),panel.border = element_rect(colour = "black", fill=NA, size=1),axis.line = element_line(colour = "black",size=1))+theme(axis.ticks = element_line(colour = color_theme1,size=1))+
      geom_errorbar(aes(ymin=summary[[3]]-summary[[4]]*errorbar_multiplier,ymax=summary[[3]]+summary[[4]]*errorbar_multiplier),width=0.15,size=0.8,colour="gray21",linetype=1)+theme(legend.key=element_blank())+ geom_line(aes(color=summary[[2]]),size=2,linetype=1)
  }

  # 2 between-subjects IVs
  if(is.null(iv1)==F&is.null(iv2)==F) {
    if(errorbar=="se") {errorbar_multiplier = 1}
    if(errorbar=="ci") {errorbar_multiplier = qt(.975,summary[[6]]-1)}
    plot <- ggplot(summary, aes(x=summary[[1]], y=M, group=summary[[2]],linetype=summary[[2]]),ylim=ylim)+ facet_wrap(~ summary[[3]],scales="free_x") + geom_point()+ coord_cartesian(ylim=ylim)+labs(title=title) +theme(plot.title=element_text(face="bold",color=color_font,size=size.title))+theme(plot.title = element_text(color=color_font,hjust = 0.5))+theme(axis.text.x = axis.text.x)+labs(x=xlab)+labs(y=ylab) +legend+theme(plot.background = element_rect(fill = "white", colour = "white"))+theme(panel.grid.major.y = element_blank(),panel.grid.major.x=element_blank(),panel.grid.minor.y=element_blank(),panel.grid.minor.x=element_blank())+theme(axis.text.y=element_text(size=size.axis.text.y,color=color_font))+scale_y_continuous+ theme(strip.background = element_rect(fill="white"))+theme(strip.text.x = element_text(size = size.panel.title,face="bold",color="black"))+theme(panel.background = element_rect(colour = color_border, fill = "white", size = 1),panel.border = element_rect(colour = "black", fill=NA, size=1),axis.line = element_line(colour = "black",size=1))+theme(axis.ticks = element_line(colour = color_theme1,size=1))+
      geom_errorbar(aes(ymin=summary[[4]]-summary[[5]]*errorbar_multiplier,ymax=summary[[4]]+summary[[5]]*errorbar_multiplier),width=0.15,size=0.8,colour="gray21",linetype=1)+theme(legend.key=element_blank())+ geom_line(aes(color=summary[[2]]),size=2,linetype=1)
  }

  summary2 <- summary
  for (i in (ncol(summary2)-2):(ncol(summary2)-1)) {
    for (j in 1:nrow(summary2)) {
      summary2[j,i] <- wrap.rd0(as.numeric(paste(summary2[j,i])),2)
    }
  }
  rownames(summary2) <- NULL
  summary2 <- summary2[,c(1:(ncol(summary2)-3),ncol(summary2),(ncol(summary2)-2):(ncol(summary2)-1))]
  print(summary2)
  if(errorbar=="se") {print("Note: Error bars are +/-1 standard error.")}
  if(errorbar=="ci") {print("Note: Error bars represent 95% confidence intervals without p-value adjustment.")}
  return(plot)
}
