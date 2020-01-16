#' Descriptive statistics
#'
#' @description Computes descriptive statistics for one dependent variable, parsed
#' by between 0 and 2 independent variables.
#'
#' @param dv1 Column vector containing the dependent variable
#' @param iv1,iv2 Column vectors containing the independent variables
#'
#' @examples
#' ## Parsing the dependent variable by 0 independent variables
#' wrap.desc(dv1 = bdata$DV5)
#'
#' ## Parsing the dependent variable by 1 independent variable
#' wrap.desc(dv1 = bdata$DV5, iv1 = bdata$IV2)
#'
#' @import stringr
#' @importFrom clipr write_clip
#' @export
wrap.desc <- function(dv1,iv1=NULL,iv2=NULL) {
  options(scipen=999)

  # Error checks
  if(is.null(dv1)) {return(paste("Cannot find the column vector inputted to parameter dv1."))}
  if(is.null(substitute(iv1))==F&is.null(iv1)==T) {return(paste("Cannot find the column vector inputted to parameter iv1."))}
  if(is.null(substitute(iv2))==F&is.null(iv2)==T) {return(paste("Cannot find the column vector inputted to parameter iv2."))}
  if(is.null(iv1)==F) {if(is.factor(iv1)==F) {return("Error: Argument iv1 must be a factor variable.")}}
  if(is.null(iv2)==F) {if(is.factor(iv2)==F) {return("Error: Argument iv2 must be a factor variable.")}}
  if(is.null(iv1)==F) {if(is.data.frame(iv1)) {if(ncol(iv1)>1) {return("Error: Must input one column maximum for iv1.")}}}
  if(is.null(iv2)==F) {if(is.data.frame(iv2)) {if(ncol(iv2)>1) {return("Error: Must input one column maximum for iv2.")}}}
  if(is.data.frame(dv1)) {if(ncol(dv1)>1) {return("Error: Must input one column maximum for dv1.")}}
  if(is.null(iv1)==F&is.null(iv2)==F) {if(substitute(iv1)==substitute(iv2)) {return("Error: You inputted the same column for iv1 and iv2.")}}
  if(class(dv1)!="numeric"&class(dv1)!="factor") {return("Error: Argument dv1 must be either a factor or numeric variable.")}
  if(is.null(iv1)==T) {
    if(any(is.na(dv1))) {print("Note: Your inputs include one or more NA entries. The function will ignore the rows containing these entries.")}
    dv1 <- dv1[complete.cases(dv1)]
  }
  if(is.null(iv1)==F&is.null(iv2)==T) {
    complete.cases <- complete.cases(cbind(dv1,iv1))
    if(any(complete.cases==F)) {print("Note: Your inputs include one or more NA entries. The function will ignore the rows containing these entries.")}
    dv1 <- dv1[complete.cases]
    iv1 <- iv1[complete.cases]
  }
  if(is.null(iv1)==F&is.null(iv2)==F) {
    complete.cases <- complete.cases(cbind(dv1,iv1,iv2))
    if(any(complete.cases==F)) {print("Note: Your inputs include one or more NA entries. The function will ignore the rows containing these entries.")}
    dv1 <- dv1[complete.cases]
    iv1 <- iv1[complete.cases]
    iv2 <- iv2[complete.cases]
  }

  # Parsing the dependent variable by 0 IVs
  if(is.null(iv1)==T & is.null(iv2)==T) {

    # numerical dependent measures
    if(class(dv1)=="numeric"|class(dv1)=="integer") {
      a <- mean(dv1,na.rm=T)
      b <- sd(dv1,na.rm=T)
      c <- b/sqrt(sum(!is.na(dv1)))
      d <- a-qt(c(.025,.975),sum(!is.na(dv1))-1)[2]*c
      e <- a+qt(c(.025,.975),sum(!is.na(dv1))-1)[2]*c
      write_clip(allow_non_interactive = TRUE, content = paste("# N = ",sum(!is.na(dv1)),": M = ",wrap.rd0(a,2),", SD = ",wrap.rd0(b,2),", Var = ",wrap.rd0(b^2,2),", SE = ",wrap.rd0(c,2),", 95% CI = [",wrap.rd0(d),", ",wrap.rd0(e),"]",sep=""))
      return(cat("# N = ",sum(!is.na(dv1)),": M = ",wrap.rd0(a,2),", SD = ",wrap.rd0(b,2),", Var = ",wrap.rd0(b^2,2),", SE = ",wrap.rd0(c,2),", 95% CI = [",wrap.rd0(d),", ",wrap.rd0(e),"]",sep=""))
    }
    # categorical dependent measures
    else if(class(dv1)=="factor"|class(dv1)=="character") {
      dv1 <- factor(dv1)
      levels_dv1 <- nlevels(dv1)
      dv1.list <- as.list(rep(0,levels_dv1))

      # compute proportion of the dv1 vector that contains each of the factor levels
      for (j in 1:levels_dv1) {
        dv1.list[[j]] <- sum(dv1==levels(dv1)[j],na.rm=T)/sum(!is.na(dv1))*100
      }

      clip <- paste("# N = ",sum(!is.na(dv1)),": ",sep="")
      for (j in 1:levels_dv1) {
        if(j==1) {
          clip <- paste(clip,sum(dv1==levels(dv1)[j],na.rm=T)," ",levels(dv1)[j]," (",wrap.rd0(dv1.list[[j]],2),"%)",sep="")
        }

        if(j>1) {
          clip <- paste(clip,", ",sum(dv1==levels(dv1)[j],na.rm=T)," ",levels(dv1)[j]," (",wrap.rd0(dv1.list[[j]],2),"%)",sep="")
        }
      }
      if(substr(clip,1,1)=="\n") {clip <- substr(clip,2,nchar(clip))}
      write_clip(allow_non_interactive = TRUE, content = paste(clip))

      string <- cat("# N = ",sum(!is.na(dv1)),": ",sep="")
      return(for (j in 1:levels_dv1) {
        if(j==1) {
          string <- cat(string,sum(dv1==levels(dv1)[j],na.rm=T)," ",levels(dv1)[j]," (",wrap.rd0(dv1.list[[j]],2),"%)",sep="")
        }

        if(j>1) {
          string <- cat(string,", ",sum(dv1==levels(dv1)[j],na.rm=T)," ",levels(dv1)[j]," (",wrap.rd0(dv1.list[[j]],2),"%)",sep="")
        }
      })
    }
  }

  # Parsing the dependent variable by 1 IV
  else if (is.null(iv1)==F & is.null(iv2)==T) {

    # numerical dependent measures
    if(class(dv1)=="numeric"|class(dv1)=="integer") {
      a <- tapply(as.numeric(dv1[is.na(dv1)==F&is.na(iv1)==F&is.na(dv1)==F]),factor(iv1[is.na(dv1)==F&is.na(iv1)==F&is.na(dv1)==F]),length) # compute number of measurements per level of your iv1
      b <- tapply(as.numeric(dv1),factor(iv1),mean,na.rm=T) # compute mean dv1 at each level of your iv1
      c <- tapply(as.numeric(dv1),factor(iv1),sd,na.rm=T) # compute SD for your dv1 at each level of your iv1
      d <- c/sqrt(a) # compute SE for your dv1 at each level of your iv1
      e <- rep(0,length(a)); f <- rep(0,length(a))
      for (i in 1:length(a)) {
        e[i] <- b[i]-qt(c(.025,.975),a[i]-1)[2]*d[i]
        f[i] <- b[i]+qt(c(.025,.975),a[i]-1)[2]*d[i]
      }

      i = sum(!is.na(names(a)))
      string <- vector(mode="character",length=i)
      for (j in 1:i) {
        string[j] = paste("# ",names(a)[j]," (N = ",a[j],"): M = ",wrap.rd0(b[j],2), ", SD = ",wrap.rd0(c[j],2),", Var = ",wrap.rd0(c[j]^2,2),", SE = ",wrap.rd0(d[j],2),", 95% CI = [",wrap.rd0(e[j],2),", ",wrap.rd0(f[j],2),"]",sep="")
      }

      clip <- ""
      for (j in 1:i) {
        clip <- paste(clip,string[j],"\n",sep="")
      }

      clip <- paste(substr(clip,1,nchar(clip)-1),sep="")
      if(substr(clip,1,1)=="\n") {clip <- substr(clip,2,nchar(clip))}
      write_clip(allow_non_interactive = TRUE, content = paste(clip))

      e <- NULL
      return(for (j in 1:i) {
        e <- cat(e,string[j],"\n",sep="")
      })
    }

    # categorical dependent measures
    else if(class(dv1)=="factor"|class(dv1)=="character") {
      iv1 <- factor(iv1)
      dv1 <- factor(dv1)
      df <- data.frame(iv1,dv1)
      colnames(df) <- c("iv1","dv1")
      levels_iv1 <- nlevels(iv1)
      levels_dv1 = nlevels(dv1)

      # subset the dv1 for each level of the iv1, so that you can analyze each level separately
      iv1.list <- as.list(rep(0,levels_iv1))
      for (j in 1:levels_iv1) {
        iv1.list[[j]] <- subset(df,iv1==levels(iv1)[j])
      }

      # create a list to store descriptive statistics
      string <- as.list(rep(0,levels_iv1*levels_dv1))

      # create the list that you will return later
      list <- as.list(rep(0,levels_iv1*levels_dv1))

      # compute N at each level of the iv1
      for (x in 1:levels_iv1) {
        if (x==1) {
          string[[((x-1)*levels_dv1)+1]] <- paste("# ",levels(iv1)[x]," (N = ",sum(iv1==levels(iv1)[x]&is.na(dv1)==F,na.rm=T),"): ",sep="")

        }
        else {
          string[[((x-1)*levels_dv1)+1]] <- paste("\n","# ",levels(iv1)[x]," (N = ",sum(iv1==levels(iv1)[x]&is.na(dv1)==F,na.rm=T),"): ",sep="")
        }
      }

      for (x in 1:levels_iv1*levels_dv1) {
        if (string[[x]]=="0") {
          string[[x]] <- ""
        }
      }

      # return Ns and percentages
      clip <- ""
      for (x in 1:levels_iv1) {
        for (y in 1:levels_dv1) {
          if(y==1) {
            clip <- paste(clip,string[[(((x)-1)*levels_dv1)+y]],sum(iv1.list[[x]]$dv1==levels(dv1)[y],na.rm=T)," ",levels(dv1)[y]," (",wrap.rd0(100*sum(iv1.list[[x]]$dv1==levels(dv1)[y],na.rm=T)/sum(is.na(iv1.list[[x]]$iv1)==F&is.na(iv1.list[[x]]$dv1)==F),2),"%)",sep="")
          }

          if(y>1) {
            clip <- paste(clip,string[[(((x)-1)*levels_dv1)+y]],")%, ",sum(iv1.list[[x]]$dv1==levels(dv1)[y],na.rm=T)," ",levels(dv1)[y]," (",wrap.rd0(100*sum(iv1.list[[x]]$dv1==levels(dv1)[y],na.rm=T)/sum(is.na(iv1.list[[x]]$iv1)==F&is.na(iv1.list[[x]]$dv1)==F),2),"%)",sep="")
          }
        }
      }

      # Fix formatting errors in output
      clip <- gsub("%0","",x=clip,fixed=T);
      clip <- gsub("%%","%",x=clip,fixed=T)
      clip <- gsub(")%)%","%)",x=clip,fixed=T)
      clip <- gsub("))%","%)",x=clip,fixed=T)
      clip <- gsub("%%)","%)",x=clip,fixed=T)
      clip <- gsub("%)0)%","%)",x=clip,fixed=T)
      if(substr(clip,1,1)=="\n") {clip <- substr(clip,2,nchar(clip))}
      write_clip(allow_non_interactive = TRUE, content = paste(clip))
      return(cat(clip))
    }
  }

  # Parsing the dependent variable by 2 IVs
  else if(is.null(iv1)==F & is.null(iv2)==F) {
    iv1 <- factor(iv1)
    iv2 <- factor(iv2)

    # numerical dependent measures

    if (class(dv1)=="numeric"|class(dv1)=="integer") {

      df <- data.frame(dv1,iv2,iv1)
      nlevels_iv2 <- nlevels(iv2)
      nlevels_iv1 <- nlevels(iv1)

      # compute descriptives at each level of iv2
      a <- matrix(rep(0),nrow=nlevels_iv2,ncol=nlevels_iv1) # matrix for "N ="
      b <- matrix(rep(0),nrow=nlevels_iv2,ncol=nlevels_iv1) # matrix for "M ="
      c <- matrix(rep(0),nrow=nlevels_iv2,ncol=nlevels_iv1) # matrix for "SD ="
      d <- matrix(rep(0),nrow=nlevels_iv2,ncol=nlevels_iv1) # matrix for "SE ="
      e <- matrix(rep(0),nrow=nlevels_iv2,ncol=nlevels_iv1) # matrix for "CI_lower ="
      f <- matrix(rep(0),nrow=nlevels_iv2,ncol=nlevels_iv1) # matrix for "CI_upper ="
      g <- matrix(rep(0),nrow=nlevels_iv2,ncol=nlevels_iv1) # matrix for "Var ="

      for (i in 1:nlevels_iv2) {
        for (j in 1:nlevels_iv1) {

          # Compute N
          a[i,j] <- sum(iv2==levels(iv2)[i] & iv1==levels(iv1)[j] & is.na(dv1)==F,na.rm=T)

          # Compute M
          b[i,j] <- mean(subset(df,iv2==levels(iv2)[i]&iv1==levels(iv1)[j])$dv1,na.rm=T)

          # Compute SD
          c[i,j] <- sd(subset(df,iv2==levels(iv2)[i]&iv1==levels(iv1)[j])$dv1,na.rm=T)

          # Compute SE
          d[i,j] <- c[i,j]/sqrt(a[i,j])

          # Compute CI_lower
          e[i,j] <- b[i,j]-qt(c(.025,.975),a[i,j]-1)[2]*d[i,j]

          # Compute CI_upper
          f[i,j] <- b[i,j]+qt(c(.025,.975),a[i,j]-1)[2]*d[i,j]

          # Compute Var
          g[i,j] <- c[i,j]^2
        }
      }

      for (i in 1:nlevels_iv2) {
        for (j in 1:nlevels_iv1) {
          b[i,j] <- wrap.rd0(as.numeric(b[i,j]),2)
          c[i,j] <- wrap.rd0(as.numeric(c[i,j]),2)
          d[i,j] <- wrap.rd0(as.numeric(d[i,j]),2)
          e[i,j] <- wrap.rd0(as.numeric(e[i,j]),2)
          f[i,j] <- wrap.rd0(as.numeric(f[i,j]),2)
          g[i,j] <- wrap.rd0(as.numeric(g[i,j]),2)
        }
      }

      clip <- ""
      for (i in 1:nlevels_iv2) {
        clip <- paste(clip,"## ",levels(iv2)[i],"\n",sep="")
        for (j in 1:nlevels_iv1) {
          clip <- paste(clip,"# ",levels(iv1)[j]," (N = ",a[i,j],"): M = ",b[i,j],", SD = ",c[i,j],", Var = ",g[i,j],", SE = ",d[i,j],", 95% CI = [",e[i,j],", ",f[i,j],"]","\n",sep="")
        }
        if (i < (nlevels_iv2)) {
          clip <- paste(clip,"\n",sep="")
        }
      }
      clip <- paste(substr(clip,1,nchar(clip)-1))
      if(substr(clip,1,1)=="\n") {clip <- substr(clip,2,nchar(clip))}
      write_clip(allow_non_interactive = TRUE, content = paste(clip))

      string <- ""
      for (i in 1:nlevels_iv2) {
        string <- cat(string,"## ",levels(iv2)[i],"\n",sep="")
        for (j in 1:nlevels_iv1) {
          string <- cat(string,"# ",levels(iv1)[j]," (N = ",a[i,j],"): M = ",b[i,j],", SD = ",c[i,j],", Var = ",g[i,j],", SE = ",d[i,j],", 95% CI = [",e[i,j],", ",f[i,j],"]","\n",sep="")
        }
        if (i < (nlevels_iv2)) {
          string <- cat(string,"\n",sep="")
        }
      }
      return(cat(string))
    }

    # categorical dependent measures
    else if (class(dv1)=="factor"|class(dv1)=="character") {
      dv1 <- factor(dv1)
      df <- data.frame(dv1,iv1,iv2)
      nlevels_iv1 <- nlevels(iv1)
      nlevels_iv2 <- nlevels(iv2)
      nlevels_dv1 <- nlevels(dv1)

      # matrix for "N = "
      a <- matrix(rep(0),nrow=nlevels_iv1,ncol=nlevels_iv2)

      for (i in 1:nlevels_iv1) {
        for (j in 1:nlevels_iv2) {
          a[i,j] <- sum(iv1==levels(iv1)[i] & iv2==levels(iv2)[j] & is.na(dv1)==F,na.rm=T)
        }
      }

      b <- array(0,dim=c(nlevels_iv1,nlevels_iv2,nlevels_dv1))

      # array to store number of repetitions of each level of dv1, at each combination of iv1 & iv2
      for (i in 1:nlevels_iv1) {
        for (j in 1:nlevels_iv2) {
          for (k in 1:nlevels_dv1) {
            b[i,j,k] <- sum(iv1==levels(iv1)[i] & iv2==levels(iv2)[j] & dv1==levels(dv1)[k],na.rm=T)
          }
        }
      }
      clip <- ""
      for (i in 1:nlevels_iv1) {
        clip <- paste(clip,"## ",levels(iv1)[i],"\n",sep="")
        for (j in 1:nlevels_iv2) {
          clip <- paste(clip,"# ",levels(iv2)[j]," (N = ",a[i,j],"): ",sep="")
          for (k in 1:nlevels_dv1) {
            if(a[i,j]>0) {clip <- paste(clip,b[i,j,k]," ",levels(dv1)[k]," (",wrap.rd0(100*b[i,j,k]/a[i,j],2),"%)",sep="")}
            if(a[i,j]==0) {return("Cannot compute descriptive statistics: one or more combinations of iv1 and iv2 has cell size 0.")}
            if (k < nlevels_dv1) {
              clip <- paste(clip,", ",sep="")
            }
          }
          clip <- paste(clip,"\n",sep="")
        }
        if (i < nlevels_iv1) {
          clip <- paste(clip,"\n",sep="")
        }
      }
      clip <- paste(substr(clip,1,nchar(clip)-1))
      if(substr(clip,1,1)=="\n") {clip <- substr(clip,2,nchar(clip))}
      write_clip(allow_non_interactive = TRUE, content = paste(clip))

      string <- ""
      for (i in 1:nlevels_iv1) {
        string <- cat(string,"## ",levels(iv1)[i],"\n",sep="")
        for (j in 1:nlevels_iv2) {
          string <- cat(string,"# ",levels(iv2)[j]," (N = ",a[i,j],"): ",sep="")
          for (k in 1:nlevels_dv1) {
            string <- cat(string,b[i,j,k]," ",levels(dv1)[k]," (",wrap.rd0(100*b[i,j,k]/a[i,j],2),"%)",sep="")
            if (k < nlevels_dv1) {
              string <- cat(string,", ",sep="")
            }
          }
          string <- cat(string,"\n",sep="")
        }
        if (i < nlevels_iv1) {
          string <- cat(string,"\n",sep="")
        }
      }
      return(cat(string))
    }
  }
}
