#' Histogram plots
#'
#' @description Creates histogram plots for numerical dependent variables and prints
#' descriptive statistics in a summary table. The function delegates the primary
#' computations to \code{\link[ggplot2]{ggplot}}.
#'
#' @param dv1 Column vector containing the dependent variable
#' @param likert A logical argument: If \code{FALSE}, the function assumes a
#' continuous dependent variable; if \code{TRUE}, the function assumes a discrete
#' dependent variable that takes on only integer values
#' @param percent A logical argument: if \code{TRUE}, the plot displays percentages
#' along the y axis; if \code{FALSE}, the plot displays counts
#' @param binwidth Numeric argument representing bin width
#' @param ylim Numeric vector containing y-axis limits
#' @param ymajor Numeric argument representing spacing of y-axis tick marks
#' @param ylab Character string containing the y-axis label
#' @param xlim Numeric vector containing x-axis limits
#' @param xmajor Numeric argument representing spacing of x-axis labels
#' @param xlab Character string containing the x-axis label
#' @param title Character string containing plot title
#' @param size.axis.text.x,size.axis.text.y,size.title Numeric arguments
#' containing font sizes
#'
#' @seealso \code{\link[ggplot2]{ggplot}}
#'
#' @examples
#' ## Histogram for a continuous dependent variable
#' wrap.hist(dv1 = bdata$DV1, likert = FALSE)
#'
#' ## Histogram for a discrete dependent variable
#' wrap.hist(dv1 = bdata$DV5, likert = TRUE)
#'
#' @import stringr ggplot2
#' @importFrom scales percent_format
#' @export
wrap.hist <- function(dv1,likert=F,percent=T,binwidth=1,ylim=NULL,
                      ymajor=NULL,ylab=NULL,xlim=NULL,xmajor=NULL,
                      xlab=NULL,title=NULL,size.axis.text.y = 12,
                      size.axis.text.x=12,size.title=24) {

  options(scipen=999)

  # Error checks
  if(is.numeric(dv1)==F) {return("Error: Argument dv1 must be numeric.")}
  if(is.null(likert)==T) {return("Error: Likert parameter must equal TRUE or FALSE.")}
  if(length(likert)>1|(likert!=TRUE&likert!=FALSE)) {return("Error: Likert parameter must equal TRUE or FALSE.")}
  if(likert==T&binwidth!=1) {return("Error: Must input binwidth=1 for Likert scale DVs.")}
  if(is.null(xlim)==F) {if(length(xlim)!=2) {return("Error: xlim must have two elements (e.g., xlim = c(0,10)).")}}
  if(is.null(ylim)==F) {if(length(ylim)!=2) {return("Error: ylim must have two elements (e.g., ylim = c(0,10)).")}}
  if(is.null(xlim)==F&likert==T) {if(xlim[1]%%1!=0|xlim[2]%%1!=0) {return("Error: x axis limits must be integers for Likert scale DVs.")}}
  if(is.null(xlim)==F) {if((xlim[2]-xlim[1])%%binwidth>0) {return("Error: The x-axis range must be evenly divisible by the bin width.")}}
  if(is.null(binwidth)==F&is.null(xlim)==F) {if(binwidth>(xlim[2]-xlim[1])) {return("Error: Bin width must be less than or equal to the difference between the x-axis limits.")}}

  if(is.null(title)==T & grepl("\\$",toString(substitute(dv1)))==T) {title <- toString(substring(deparse(substitute(as.numeric(dv1))),str_locate_all(pattern=coll('$'),deparse(substitute(as.numeric(dv1))))[[1]][1]+1,nchar(deparse(substitute(as.numeric(dv1))))-1))}
  if(is.null(title)==T & grepl("\\[",toString(substitute(dv1)))==T) {df_temp <- get(substr(deparse(substitute(dv1)),1,which(strsplit(deparse(substitute(dv1)), "")[[1]]=="[")-1),envir = .GlobalEnv); title <- names(df_temp)[substitute(dv1)[[3]]]}
  if(any(is.na(dv1))) {print("Note: Your inputs include one or more NA entries. The function will ignore the rows containing these entries.")}
  dv1 <- dv1[!is.na(dv1)]
  if(all(dv1%%1==0)==F&likert==T) {warning("Note: You specified likert=T but one or more responses on the dependent measure are non-integers.")}
  df <- data.frame(dv1)
  if(is.null(binwidth)==F&is.null(xmajor)==T) {xmajor <- binwidth}

  if(is.null(xlim)==T&is.null(binwidth)==T) {xlim=c(min(dv1,na.rm=T),max(dv1,na.rm=T))}

  # Ensure that ggplot displays the uppermost bin in full
  if(is.null(xlim)==T&is.null(binwidth)==F) {
    xlim1 <- min(dv1,na.rm=T)
    xlim2 <- max(dv1,na.rm=T)
    xlim2 <- ceiling((xlim2-xlim1)/binwidth)*binwidth+xlim1
    xlim <- c(xlim1,xlim2)
  }

  max <- 0; for (i in 1:nlevels(as.factor(dv1))) {temp <- sum(as.factor(dv1)==levels(as.factor(dv1))[i],na.rm=T); if(temp>max) {max <- temp}}

  if(is.null(ylim)==T&percent==T) {ylim=c(0,1)}
  if(percent==T&is.null(ymajor)==T) {ymajor=.1}
  if(percent==F&is.null(ymajor)==T) {ymajor=ceiling(length(dv1)/((xlim[2]-xlim[1])/binwidth)/5)}
  if(likert==T) {offset=1/2}
  if(likert==F) {offset=0}

  # display percentages on y axis
  if(percent==T) {
    plot <- ggplot(data=df,aes(dv1))+labs(y=ylab,x=xlab,title=title)+geom_histogram(aes(y = (..count..)/sum(..count..)),binwidth=binwidth,breaks=seq(xlim[1]-offset,xlim[2]+offset,binwidth),col="black",fill="gray80")+scale_x_continuous(breaks=seq(xlim[1],xlim[2],xmajor),expand=c(0,0))+scale_y_continuous(expand=c(0,0),labels=percent_format(accuracy=1),breaks=seq(0,1,ymajor))+expand_limits(y=ylim)+theme(plot.title=element_text(face="bold",color="black",size=size.title))+theme(plot.title = element_text(color="black",hjust = 0.5))+theme(plot.background = element_rect(fill = "white", colour = "white"))+ theme(panel.grid.major.y = element_blank(),panel.grid.major.x=element_blank(),panel.grid.minor.y=element_blank(),panel.grid.minor.x=element_blank())+theme(axis.text.y=element_text(color="black",size=size.axis.text.y))+theme(axis.text.x=element_text(color="black",size=size.axis.text.x))+ theme(strip.background = element_rect(fill="white"))+theme(strip.text.x = element_text(size = 12,face="bold"))+theme(panel.background = element_rect(colour = "black", fill = "white", size = 0))
  }

  # display counts on y axis
  if(percent==F) {
    if(is.null(ylim)==T) {
      ylim <- c(0,max(ggplot_build(ggplot(data=df,aes(dv1))+labs(y=ylab,x=xlab,title=title)+geom_histogram(aes(y = (..count..)),binwidth=binwidth,breaks=seq(xlim[1]-offset,xlim[2]+offset,binwidth),col="black",fill="gray80")+scale_x_continuous(breaks=seq(xlim[1],xlim[2],xmajor),expand=c(0,0))+scale_y_continuous(expand=c(0,0),breaks=seq(0,0,ymajor))+expand_limits(y=ylim)+theme(plot.title=element_text(face="bold",color="black",size=size.title))+theme(plot.title = element_text(color="black",hjust = 0.5))+theme(plot.background = element_rect(fill = "white", colour = "white"))+ theme(panel.grid.major.y = element_blank(),panel.grid.major.x=element_blank(),panel.grid.minor.y=element_blank(),panel.grid.minor.x=element_blank())+theme(axis.text.y=element_text(color="black"))+theme(axis.text.x=element_text(color="black"))+ theme(strip.background = element_rect(fill="white"))+theme(strip.text.x = element_text(size = 12,face="bold"))+theme(panel.background = element_rect(colour = "black", fill = "white", size = 0)))$data[[1]]$ymax))
    }
    plot <- ggplot(data=df,aes(dv1))+labs(y=ylab,x=xlab,title=title)+geom_histogram(aes(y = (..count..)),binwidth=binwidth,breaks=seq(xlim[1]-offset,xlim[2]+offset,binwidth),col="black",fill="gray80")+scale_x_continuous(breaks=seq(xlim[1],xlim[2],xmajor),expand=c(0,0))+scale_y_continuous(expand=c(0,0),breaks=seq(ylim[1],ylim[2],ymajor))+expand_limits(y=ylim)+theme(plot.title=element_text(face="bold",color="black",size=size.title))+theme(plot.title = element_text(color="black",hjust = 0.5))+theme(plot.background = element_rect(fill = "white", colour = "white"))+ theme(panel.grid.major.y = element_blank(),panel.grid.major.x=element_blank(),panel.grid.minor.y=element_blank(),panel.grid.minor.x=element_blank())+theme(axis.text.y=element_text(color="black",size=size.axis.text.y))+theme(axis.text.x=element_text(color="black",size=size.axis.text.x))+ theme(strip.background = element_rect(fill="white"))+theme(strip.text.x = element_text(size = 12,face="bold"))+theme(panel.background = element_rect(colour = "black", fill = "white", size = 0))
  }

  summary <- ggplot_build(plot)$data[[1]][c(1,2,4,5)]
  colnames(summary) <- c("Percentage","Count","Lower Bound","Upper Bound")
  for (i in 1:nrow(summary)) {summary$Percentage[i] <- wrap.rd0(100*summary$Count[i]/sum(summary$Count),2)}
  summary$Percentage <- paste(summary$Percentage,"%",sep="")
  summary <- summary[c(3,4,1,2)]
  if(sum(summary$Count)<length(dv1)) {return("Error: One or more data points currently would not be displayed. Please expand x limits and/or reduce bin width.")}
  print(summary,row.names=F)
  return(plot)
}
