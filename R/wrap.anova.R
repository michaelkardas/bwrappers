#' ANOVA (main effects & interaction effects)
#'
#' @description Computes main effects and interaction effects for ANOVA with
#' up to 1 within-subjects factor and up to 3 between-subjects factors. The function
#' delegates the primary computations to \code{\link[ez]{ezANOVA}}. Note that this
#' function assumes categorical (i.e., unordered) independent variables, fixed effects,
#' equality of variances for between-subjects factors, and sphericity for within-subjects
#' factors. If variance differs significantly by condition in a fully between-subjects
#' analysis, or if sphericity does not hold, the function additionally displays
#' assumption checks. In the output, hp2 denotes partial eta squared.
#'
#' @param dv1 Column vector containing the between-subjects dependent variable
#' OR multiple column vectors containing the within-subjects dependent variables
#' @param iv1,iv2,iv3 Column vectors containing the between-subjects independent variables
#' @param type Numeric argument representing sum-of-squares type (\code{1}, \code{2}, or \code{3})
#'
#' @seealso \code{\link[ez]{ezANOVA}}
#'
#' @examples
#' ## ANOVA with 1 within-subjects factor
#' wrap.anova(dv1 = bdata[c(6, 8)])
#'
#' ## ANOVA with 2 between-subjects factors
#' wrap.anova(dv1 = bdata$DV5, iv1 = bdata$IV1, iv2 = bdata$IV2)
#'
#' ## ANOVA with 1 within-subjects factor & 2 between-subjects factors
#' wrap.anova(dv1 = bdata[c(6, 8)], iv1 = bdata$IV1, iv2 = bdata$IV2)
#' @import stringr effsize ez tidyr
#' @importFrom dplyr one_of
#' @importFrom clipr write_clip
#' @export
wrap.anova <- function(dv1,iv1=NULL,iv2=NULL,iv3=NULL,type=3) {

  options(scipen=999)
  x <- options('contrasts') # store original contrasts
  options(contrasts = c('contr.sum','contr.poly'))
  dv1_name <- deparse(substitute(dv1))
  sub1 <- NULL; if(is.null(substitute(iv1))==F) {sub1 <- substitute(iv1); iv1_name <- deparse(substitute(iv1)); colname1 <- substring(iv1_name,str_locate_all(pattern=coll('$'),iv1_name)[[1]][1]+1)}
  sub2 <- NULL; if(is.null(substitute(iv2))==F) {sub2 <- substitute(iv2); iv2_name <- deparse(substitute(iv2)); colname2 <- substring(iv2_name,str_locate_all(pattern=coll('$'),iv2_name)[[1]][1]+1)}
  sub3 <- NULL; if(is.null(substitute(iv3))==F) {sub3 <- substitute(iv3); iv3_name <- deparse(substitute(iv3)); colname3 <- substring(iv3_name,str_locate_all(pattern=coll('$'),iv3_name)[[1]][1]+1)}

  # Error checks
  if(is.null(dv1)) {return(paste("Cannot find the column vector inputted to parameter dv1."))}
  if(is.null(sub1)==F&is.null(iv1)==T) {return(paste("Cannot find the column vector inputted to parameter iv1."))}
  if(is.null(sub2)==F&is.null(iv2)==T) {return(paste("Cannot find the column vector inputted to parameter iv2."))}
  if(is.null(sub3)==F&is.null(iv3)==T) {return(paste("Cannot find the column vector inputted to parameter iv3."))}
  for (i in 1:ncol(as.data.frame(dv1))) {
    if(is.numeric(as.data.frame(dv1)[,i])==F) {return("Error: Must enter numeric columns for the dv1 parameter.")}
  }
  if(is.null(iv1)==T&(is.null(iv2)==F|is.null(iv3)==F)) {return("Error: Must input iv1 before inputting either iv2 or iv3.")}
  if(is.null(iv2)==T&is.null(iv3)==F) {return("Error: Must input iv2 before inputting iv3.")}
  if(ncol(as.data.frame(dv1))==1&is.null(iv1)==T) {return("Error: Must enter between-subjects factors or multiple within-subjects DVs.")}
  if((is.data.frame(iv1)==T)|(is.data.frame(iv2)==T)|(is.data.frame(iv3)==T)) {
    return("Error: Must enter column vectors, rather than data frames, for the independent variables.")
  }
  if(type!=1&type!=2&type!=3) {return("Error: Argument type must be equal to 1, 2, or 3.")}
  if(is.null(iv1)==F) {if(is.factor(iv1)==F) {return("Error: Must input a factor variable for iv1.")}}
  if(is.null(iv2)==F) {if(is.factor(iv2)==F) {return("Error: Must input a factor variable for iv2.")}}
  if(is.null(iv3)==F) {if(is.factor(iv3)==F) {return("Error: Must input a factor variable for iv3.")}}

  # delete rows with missing values
  if(is.null(iv1)==T) {
    complete.cases <- complete.cases(dv1)
    if(any(complete.cases==F)) {
      print("Note: Your inputs include one or more NA entries. The function will ignore the rows containing these entries.")
      if(is.data.frame(dv1)==T) {dv1 <- dv1[complete.cases,]; dv1 <- as.data.frame(dv1)}
      if(is.data.frame(dv1)==F) {dv1 <- dv1[complete.cases]; dv1 <- as.numeric(dv1)}
    }
  }
  if(is.null(iv1)==F&is.null(iv2)==T) {
    complete.cases <- complete.cases(cbind(dv1,iv1))
    if(any(complete.cases==F)) {print("Note: Your inputs include one or more NA entries. The function will ignore the rows containing these entries.")}
    if(is.data.frame(dv1)==T) {dv1 <- dv1[complete.cases,]; dv1 <- as.data.frame(dv1)}
    if(is.data.frame(dv1)==F) {dv1 <- dv1[complete.cases]; dv1 <- as.numeric(dv1)}
    iv1 <- iv1[complete.cases]
  }
  if(is.null(iv1)==F&is.null(iv2)==F&is.null(iv3)==T) {
    complete.cases <- complete.cases(cbind(dv1,iv1,iv2))
    if(any(complete.cases==F)) {print("Note: Your inputs include one or more NA entries. The function will ignore the rows containing these entries.")}
    if(is.data.frame(dv1)==T) {dv1 <- dv1[complete.cases,]; dv1 <- as.data.frame(dv1)}
    if(is.data.frame(dv1)==F) {dv1 <- dv1[complete.cases]; dv1 <- as.numeric(dv1)}
    iv1 <- iv1[complete.cases]
    iv2 <- iv2[complete.cases]
  }
  if(is.null(iv1)==F&is.null(iv2)==F&is.null(iv3)==F) {
    complete.cases <- complete.cases(cbind(dv1,iv1,iv2,iv3))
    if(any(complete.cases==F)) {print("Note: Your inputs include one or more NA entries. The function will ignore the rows containing these entries.")}
    if(is.data.frame(dv1)==T) {dv1 <- dv1[complete.cases,]; dv1 <- as.data.frame(dv1)}
    if(is.data.frame(dv1)==F) {dv1 <- dv1[complete.cases]; dv1 <- as.numeric(dv1)}
    iv1 <- iv1[complete.cases]
    iv2 <- iv2[complete.cases]
    iv3 <- iv3[complete.cases]
  }

  if(is.null(iv1)==T) {df_temp <- as.data.frame(dv1)}
  if(is.null(iv1)==F&is.null(iv2)==T) {df_temp <- as.data.frame(cbind(dv1,iv1)); colnames(df_temp)[ncol(df_temp)] <- colname1}
  if(is.null(iv1)==F&is.null(iv2)==F&is.null(iv3)==T) {df_temp <- as.data.frame(cbind(dv1,iv1,iv2)); colnames(df_temp)[(ncol(df_temp)-1):ncol(df_temp)] <- c(colname1,colname2)}
  if(is.null(iv1)==F&is.null(iv2)==F&is.null(iv3)==F) {df_temp <- as.data.frame(cbind(dv1,iv1,iv2,iv3)); colnames(df_temp)[(ncol(df_temp)-2):ncol(df_temp)] <- c(colname1,colname2,colname3)}

  print(paste("ASSUMPTIONS: This function assumes categorical (i.e., unordered) independent variables, fixed effects, equality of variances for between-subjects factors, and sphericity for within-subjects factors. In the output, hp2 denotes partial eta squared.",sep=""))
  print(paste("Note: You are currently using type ",type," sums of squares.",sep=""))
  if(is.data.frame(dv1)==F) {

    # 3-way between-subjects ANOVA
    if(is.null(iv1)==F&is.null(iv2)==F&is.null(iv3)==F) {
      dv1 <- as.numeric(dv1); iv1_ <- factor(iv1); iv2_ <- factor(iv2); iv3_ <- factor(iv3)

      # assign variable names so that you can produce the IV names within the output as well
      iv1name <- substring(iv1_name,str_locate_all(pattern=coll('$'),iv1_name)[[1]][1]+1)
      iv2name <- substring(iv2_name,str_locate_all(pattern=coll('$'),iv2_name)[[1]][1]+1)
      iv3name <- substring(iv3_name,str_locate_all(pattern=coll('$'),iv3_name)[[1]][1]+1)
      df <- as.data.frame(cbind(dv1,iv1_,iv2_,iv3_,1:length(iv1_)))
      colnames(df) <- c("dv1","iv1","iv2","iv3","userAnovaIDNum0")
      df$userAnovaIDNum0 <- factor(df$userAnovaIDNum0); df$iv1 <- factor(df$iv1); df$iv2 <- factor(df$iv2); df$iv3 <- factor(df$iv3)

      # If the design is balanced, then convert type 1 ANOVA to type 2 ANOVA (they're identical for balanced designs)
      if(type==1) {
        cellsizes <- rep(0,nlevels(df[["iv1"]])*nlevels(df[["iv2"]])*nlevels(df[["iv3"]]))
        count <- 1
        for (i in 1:nlevels(df[["iv1"]])) {
          for (j in 1:nlevels(df[["iv2"]])) {
            for (k in 1:nlevels(df[["iv3"]])) {
              cellsizes[count] <- sum(df[["iv1"]]==levels(df[["iv1"]])[i]&df[["iv2"]]==levels(df[["iv2"]])[j]&df[["iv3"]]==levels(df[["iv3"]])[k])
              count <- count+1
            }
          }
        }
        if(length(unique(cellsizes))==1&cellsizes[1]>0) {type <- 2}
      }

      ez <- ezANOVA(data=df,dv=dv1,wid=userAnovaIDNum0,between=.(iv1,iv2,iv3),detailed=T,type=type)
      Anova <- ez$ANOVA
      levene <- ez$`Levene's Test for Homogeneity of Variance`
      if(levene$p <= .05 & levene$p >= .001) {levene_string <- paste("\n","\n","Note: Variance differs significantly by condition, F(",levene$DFn,", ",levene$DFd,") = ",wrap.rd0(levene$F,2),", p = ",wrap.rd(levene$p,3),", hp2 = ",wrap.rd(levene$SSn/(levene$SSn+levene$SSd),2),".",sep="")}
      if(levene$p < .001) {levene_string <- paste("\n","\n","Note: Variance differs significantly by condition, F(",levene$DFn,", ",levene$DFd,") = ",wrap.rd0(levene$F,2),", p < .001, hp2 = ",wrap.rd(levene$SSn/(levene$SSn+levene$SSd),2),".",sep="")}
      if(levene$p > .05) {levene_string <- ""}
      if(Anova$Effect[1]!="(Intercept)") {Anova <- rbind(NA,Anova); rownames(Anova) <- 1:nrow(Anova)}
      options(contrasts= c(x$contrasts[1],x$contrasts[2])) # reset contrasts to whatever they were before calling this function

      write_clip(allow_non_interactive = TRUE, content = 
        paste("# ",iv1name," (main effect): F(",Anova$DFn[2],", ",Anova$DFd[2],") = ",wrap.rd0(Anova$F[2],2),", p",if (as.numeric(Anova$p[2]) < .001) {" < .001"},if (as.numeric(Anova$p[2]) >= .001) {" = "},if (as.numeric(Anova$p[2]) >= .001) {wrap.rd(Anova$p[2],3)},", hp2 = ",wrap.rd(Anova$SSn[2]/(Anova$SSn[2]+Anova$SSd[2]),2),
              "\n","# ",iv2name," (main effect): F(",Anova$DFn[3],", ",Anova$DFd[3],") = ",wrap.rd0(Anova$F[3],2),", p",if (as.numeric(Anova$p[3]) < .001) {" < .001"},if (as.numeric(Anova$p[3]) >= .001) {" = "},if (as.numeric(Anova$p[3]) >= .001) {wrap.rd(Anova$p[3],3)},", hp2 = ",wrap.rd(Anova$SSn[3]/(Anova$SSn[3]+Anova$SSd[3]),2),
              "\n","# ",iv3name," (main effect): F(",Anova$DFn[4],", ",Anova$DFd[4],") = ",wrap.rd0(Anova$F[4],2),", p",if (as.numeric(Anova$p[4]) < .001) {" < .001"},if (as.numeric(Anova$p[4]) >= .001) {" = "},if (as.numeric(Anova$p[4]) >= .001) {wrap.rd(Anova$p[4],3)},", hp2 = ",wrap.rd(Anova$SSn[4]/(Anova$SSn[4]+Anova$SSd[4]),2),
              "\n","# ",iv1name," x ",iv2name," (2-way interaction): F(",Anova$DFn[5],", ",Anova$DFd[5],") = ",wrap.rd0(Anova$F[5],2),", p",if (as.numeric(Anova$p[5]) < .001) {" < .001"},if (as.numeric(Anova$p[5]) >= .001) {" = "},if (as.numeric(Anova$p[5]) >= .001) {wrap.rd(Anova$p[5],3)},", hp2 = ",wrap.rd(Anova$SSn[5]/(Anova$SSn[5]+Anova$SSd[5]),2),
              "\n","# ",iv1name," x ",iv3name," (2-way interaction): F(",Anova$DFn[6],", ",Anova$DFd[6],") = ",wrap.rd0(Anova$F[6],2),", p",if (as.numeric(Anova$p[6]) < .001) {" < .001"},if (as.numeric(Anova$p[6]) >= .001) {" = "},if (as.numeric(Anova$p[6]) >= .001) {wrap.rd(Anova$p[6],3)},", hp2 = ",wrap.rd(Anova$SSn[6]/(Anova$SSn[6]+Anova$SSd[6]),2),
              "\n","# ",iv2name," x ",iv3name," (2-way interaction): F(",Anova$DFn[7],", ",Anova$DFd[7],") = ",wrap.rd0(Anova$F[7],2),", p",if (as.numeric(Anova$p[7]) < .001) {" < .001"},if (as.numeric(Anova$p[7]) >= .001) {" = "},if (as.numeric(Anova$p[7]) >= .001) {wrap.rd(Anova$p[7],3)},", hp2 = ",wrap.rd(Anova$SSn[7]/(Anova$SSn[7]+Anova$SSd[7]),2),
              "\n","# ",iv1name," x ",iv2name," x ",iv3name," (3-way interaction): F(",Anova$DFn[8],", ",Anova$DFd[8],") = ",wrap.rd0(Anova$F[8],2),", p",if (as.numeric(Anova$p[8]) < .001) {" < .001"},if (as.numeric(Anova$p[8]) >= .001) {" = "},if (as.numeric(Anova$p[8]) >= .001) {wrap.rd(Anova$p[8],3)},", hp2 = ",wrap.rd(Anova$SSn[8]/(Anova$SSn[8]+Anova$SSd[8]),2),
              sep="")
      )
      return(
        cat("\n","# ",iv1name," (main effect): F(",Anova$DFn[2],", ",Anova$DFd[2],") = ",wrap.rd0(Anova$F[2],2),", p",if (as.numeric(Anova$p[2]) < .001) {" < .001"},if (as.numeric(Anova$p[2]) >= .001) {" = "},if (as.numeric(Anova$p[2]) >= .001) {wrap.rd(Anova$p[2],3)},", hp2 = ",wrap.rd(Anova$SSn[2]/(Anova$SSn[2]+Anova$SSd[2]),2),
            "\n","# ",iv2name," (main effect): F(",Anova$DFn[3],", ",Anova$DFd[3],") = ",wrap.rd0(Anova$F[3],2),", p",if (as.numeric(Anova$p[3]) < .001) {" < .001"},if (as.numeric(Anova$p[3]) >= .001) {" = "},if (as.numeric(Anova$p[3]) >= .001) {wrap.rd(Anova$p[3],3)},", hp2 = ",wrap.rd(Anova$SSn[3]/(Anova$SSn[3]+Anova$SSd[3]),2),
            "\n","# ",iv3name," (main effect): F(",Anova$DFn[4],", ",Anova$DFd[4],") = ",wrap.rd0(Anova$F[4],2),", p",if (as.numeric(Anova$p[4]) < .001) {" < .001"},if (as.numeric(Anova$p[4]) >= .001) {" = "},if (as.numeric(Anova$p[4]) >= .001) {wrap.rd(Anova$p[4],3)},", hp2 = ",wrap.rd(Anova$SSn[4]/(Anova$SSn[4]+Anova$SSd[4]),2),
            "\n","# ",iv1name," x ",iv2name," (2-way interaction): F(",Anova$DFn[5],", ",Anova$DFd[5],") = ",wrap.rd0(Anova$F[5],2),", p",if (as.numeric(Anova$p[5]) < .001) {" < .001"},if (as.numeric(Anova$p[5]) >= .001) {" = "},if (as.numeric(Anova$p[5]) >= .001) {wrap.rd(Anova$p[5],3)},", hp2 = ",wrap.rd(Anova$SSn[5]/(Anova$SSn[5]+Anova$SSd[5]),2),
            "\n","# ",iv1name," x ",iv3name," (2-way interaction): F(",Anova$DFn[6],", ",Anova$DFd[6],") = ",wrap.rd0(Anova$F[6],2),", p",if (as.numeric(Anova$p[6]) < .001) {" < .001"},if (as.numeric(Anova$p[6]) >= .001) {" = "},if (as.numeric(Anova$p[6]) >= .001) {wrap.rd(Anova$p[6],3)},", hp2 = ",wrap.rd(Anova$SSn[6]/(Anova$SSn[6]+Anova$SSd[6]),2),
            "\n","# ",iv2name," x ",iv3name," (2-way interaction): F(",Anova$DFn[7],", ",Anova$DFd[7],") = ",wrap.rd0(Anova$F[7],2),", p",if (as.numeric(Anova$p[7]) < .001) {" < .001"},if (as.numeric(Anova$p[7]) >= .001) {" = "},if (as.numeric(Anova$p[7]) >= .001) {wrap.rd(Anova$p[7],3)},", hp2 = ",wrap.rd(Anova$SSn[7]/(Anova$SSn[7]+Anova$SSd[7]),2),
            "\n","# ",iv1name," x ",iv2name," x ",iv3name," (3-way interaction): F(",Anova$DFn[8],", ",Anova$DFd[8],") = ",wrap.rd0(Anova$F[8],2),", p",if (as.numeric(Anova$p[8]) < .001) {" < .001"},if (as.numeric(Anova$p[8]) >= .001) {" = "},if (as.numeric(Anova$p[8]) >= .001) {wrap.rd(Anova$p[8],3)},", hp2 = ",wrap.rd(Anova$SSn[8]/(Anova$SSn[8]+Anova$SSd[8]),2),
            levene_string,
            sep="")
      )
    }

    # 2-way between-subjects ANOVA
    if(is.null(iv1)==F&is.null(iv2)==F) {
      dv1 <- as.numeric(dv1); iv1_ <- factor(iv1); iv2_ <- factor(iv2)

      # assign variable names so that you can produce the IV names within the output as well
      iv1name <- substring(iv1_name,str_locate_all(pattern=coll('$'),iv1_name)[[1]][1]+1)
      iv2name <- substring(iv2_name,str_locate_all(pattern=coll('$'),iv2_name)[[1]][1]+1)

      df <- data.frame(dv1,iv1_,iv2_,1:length(iv1_))
      colnames(df) <- c("dv1","iv1","iv2","userAnovaIDNum0")
      df$userAnovaIDNum0 <- factor(df$userAnovaIDNum0); df$iv1 <- factor(df$iv1); df$iv2 <- factor(df$iv2)

      # If the design is balanced, then convert type 1 ANOVA to type 2 ANOVA (they're identical for balanced designs)
      if(type==1) {
        cellsizes <- rep(0,nlevels(df[["iv1"]])*nlevels(df[["iv2"]]))
        count <- 1
        for (i in 1:nlevels(df[["iv1"]])) {
          for (j in 1:nlevels(df[["iv2"]])) {
            cellsizes[count] <- sum(df[["iv1"]]==levels(df[["iv1"]])[i]&df[["iv2"]]==levels(df[["iv2"]])[j])
            count <- count+1
          }
        }
        if(length(unique(cellsizes))==1&cellsizes[1]>0) {type <- 2}
      }

      ez <- ezANOVA(data=df,dv=dv1,wid=userAnovaIDNum0,between=.(iv1,iv2),detailed=T,type=type)
      Anova <- ez$ANOVA
      levene <- ez$`Levene's Test for Homogeneity of Variance`
      if(levene$p <= .05 & levene$p >= .001) {levene_string <- paste("\n","\n","Note: Variance differs significantly by condition, F(",levene$DFn,", ",levene$DFd,") = ",wrap.rd0(levene$F,2),", p = ",wrap.rd(levene$p,3),", hp2 = ",wrap.rd(levene$SSn/(levene$SSn+levene$SSd),2),".",sep="")}
      if(levene$p < .001) {levene_string <- paste("\n","\n","Note: Variance differs significantly by condition, F(",levene$DFn,", ",levene$DFd,") = ",wrap.rd0(levene$F,2),", p < .001, hp2 = ",wrap.rd(levene$SSn/(levene$SSn+levene$SSd),2),".",sep="")}
      if(levene$p > .05) {levene_string <- ""}
      if(Anova$Effect[1]!="(Intercept)") {Anova <- rbind(NA,Anova); rownames(Anova) <- 1:nrow(Anova)}
      options(contrasts= c(x$contrasts[1],x$contrasts[2])) # reset contrasts to whatever they were before calling this function

      write_clip(allow_non_interactive = TRUE, content = 
        paste("# ",iv1name," (main effect): F(",Anova$DFn[2],", ",Anova$DFd[2],") = ",wrap.rd0(Anova$F[2],2),", p",if (as.numeric(Anova$p[2]) < .001) {" < .001"},if (as.numeric(Anova$p[2]) >= .001) {" = "},if (as.numeric(Anova$p[2]) >= .001) {wrap.rd(Anova$p[2],3)},", hp2 = ",wrap.rd(Anova$SSn[2]/(Anova$SSn[2]+Anova$SSd[2]),2),
              "\n","# ",iv2name," (main effect): F(",Anova$DFn[3],", ",Anova$DFd[3],") = ",wrap.rd0(Anova$F[3],2),", p",if (as.numeric(Anova$p[3]) < .001) {" < .001"},if (as.numeric(Anova$p[3]) >= .001) {" = "},if (as.numeric(Anova$p[3]) >= .001) {wrap.rd(Anova$p[3],3)},", hp2 = ",wrap.rd(Anova$SSn[3]/(Anova$SSn[3]+Anova$SSd[3]),2),
              "\n","# ",iv1name," x ",iv2name," (2-way interaction): F(",Anova$DFn[4],", ",Anova$DFd[4],") = ",wrap.rd0(Anova$F[4],2),", p",if (as.numeric(Anova$p[4]) < .001) {" < .001"},if (as.numeric(Anova$p[4]) >= .001) {" = "},if (as.numeric(Anova$p[4]) >= .001) {wrap.rd(Anova$p[4],3)},", hp2 = ",wrap.rd(Anova$SSn[4]/(Anova$SSn[4]+Anova$SSd[4]),2),
              sep="")
      )
      return(
        cat("\n","# ",iv1name," (main effect): F(",Anova$DFn[2],", ",Anova$DFd[2],") = ",wrap.rd0(Anova$F[2],2),", p",if (as.numeric(Anova$p[2]) < .001) {" < .001"},if (as.numeric(Anova$p[2]) >= .001) {" = "},if (as.numeric(Anova$p[2]) >= .001) {wrap.rd(Anova$p[2],3)},", hp2 = ",wrap.rd(Anova$SSn[2]/(Anova$SSn[2]+Anova$SSd[2]),2),
            "\n","# ",iv2name," (main effect): F(",Anova$DFn[3],", ",Anova$DFd[3],") = ",wrap.rd0(Anova$F[3],2),", p",if (as.numeric(Anova$p[3]) < .001) {" < .001"},if (as.numeric(Anova$p[3]) >= .001) {" = "},if (as.numeric(Anova$p[3]) >= .001) {wrap.rd(Anova$p[3],3)},", hp2 = ",wrap.rd(Anova$SSn[3]/(Anova$SSn[3]+Anova$SSd[3]),2),
            "\n","# ",iv1name," x ",iv2name," (2-way interaction): F(",Anova$DFn[4],", ",Anova$DFd[4],") = ",wrap.rd0(Anova$F[4],2),", p",if (as.numeric(Anova$p[4]) < .001) {" < .001"},if (as.numeric(Anova$p[4]) >= .001) {" = "},if (as.numeric(Anova$p[4]) >= .001) {wrap.rd(Anova$p[4],3)},", hp2 = ",wrap.rd(Anova$SSn[4]/(Anova$SSn[4]+Anova$SSd[4]),2),
            levene_string,
            sep="")
      )
    }

    # One-way between-subjects ANOVA
    if(is.null(iv2)==T) {
      if(is.null(iv1)==F) {iv1 <- factor(iv1)}

      df <- data.frame(iv1,dv1)

      # assign variable names so that you can produce the IV names within the output as well
      df <- cbind(df,1:nrow(df))
      colnames(df) <- c("iv1","dv1","userAnovaIDNum0")
      df$userAnovaIDNum0 <- factor(df$userAnovaIDNum0); df$iv1 <- factor(df$iv1)

      # If the design is balanced, then convert type 1 ANOVA to type 2 ANOVA (they're identical for balanced designs)
      if(type==1) {
        cellsizes <- rep(0,nlevels(df[["iv1"]]))
        count <- 1
        for (i in 1:nlevels(df[["iv1"]])) {
          cellsizes[count] <- sum(df[["iv1"]]==levels(df[["iv1"]])[i])
          count <- count+1
        }
        if(length(unique(cellsizes))==1&cellsizes[1]>0) {type <- 2}
      }

      ez <- ezANOVA(data=df,dv=dv1,wid=userAnovaIDNum0,between=iv1,detailed=T,type=type)
      Anova <- ez$ANOVA
      levene <- ez$`Levene's Test for Homogeneity of Variance`
      if(levene$p <= .05 & levene$p >= .001) {levene_string <- paste("\n","\n","Note: Variance differs significantly by condition, F(",levene$DFn,", ",levene$DFd,") = ",wrap.rd0(levene$F,2),", p = ",wrap.rd(levene$p,3),", hp2 = ",wrap.rd(levene$SSn/(levene$SSn+levene$SSd),2),".",sep="")}
      if(levene$p < .001) {levene_string <- paste("\n","\n","Note: Variance differs significantly by condition, F(",levene$DFn,", ",levene$DFd,") = ",wrap.rd0(levene$F,2),", p < .001, hp2 = ",wrap.rd(levene$SSn/(levene$SSn+levene$SSd),2),".",sep="")}
      if(levene$p > .05) {levene_string <- ""}
      if(Anova$Effect[1]!="(Intercept)") {Anova <- rbind(NA,Anova); rownames(Anova) <- 1:nrow(Anova)}
      options(contrasts= c(x$contrasts[1],x$contrasts[2])) # reset contrasts to whatever they were before calling this function

      write_clip(allow_non_interactive = TRUE, content = 
        paste("# F(",Anova$DFn[2],", ",Anova$DFd[2],") = ",wrap.rd0(Anova$F[2],2),", p",if (as.numeric(Anova$p[2]) < .001) {" < .001"},if (as.numeric(Anova$p[2]) >= .001) {" = "},if (as.numeric(Anova$p[2]) >= .001) {wrap.rd(Anova$p[2],3)},", hp2 = ",wrap.rd(Anova$SSn[2]/(Anova$SSn[2]+Anova$SSd[2]),2),
              sep="")
      )
      return(
        cat("\n","# F(",Anova$DFn[2],", ",Anova$DFd[2],") = ",wrap.rd0(Anova$F[2],2),", p",if (as.numeric(Anova$p[2]) < .001) {" < .001"},if (as.numeric(Anova$p[2]) >= .001) {" = "},if (as.numeric(Anova$p[2]) >= .001) {wrap.rd(Anova$p[2],3)},", hp2 = ",wrap.rd(Anova$SSn[2]/(Anova$SSn[2]+Anova$SSd[2]),2),
            levene_string,
            sep="")
      )
    }
  }

  # One or more repeated measures.
  if(is.data.frame(dv1)==T) {
    data_frame <- df_temp
    data_frame$userAnovaIDNum <- 0
    for (i in 1:nrow(data_frame)) {
      data_frame$userAnovaIDNum[i] <- i
    }

    # One-way ANOVA, within-subjects
    if(is.null(iv1)==T) {
      temp0 <- colnames(df_temp)[1:ncol(dv1)]
      temp <- gather(data_frame,Ratings,Scale,one_of(temp0),factor_key=TRUE)
      temp$userAnovaIDNum0 <- factor(temp[[which(colnames(temp)==colnames(data_frame)[grep('^userAnovaIDNum', names(data_frame))[1]])]])

      # If the design is balanced, then convert type 1 ANOVA to type 2 ANOVA (they're identical for balanced designs)
      if(type==1) {
        cellsizes <- rep(0,nlevels(temp$Ratings))
        count <- 1
        for (l in 1:nlevels(temp$Ratings)) {
          cellsizes[count] <- sum(temp[["Ratings"]]==levels(temp$Ratings)[l])
          count <- count+1
        }
        if(length(unique(cellsizes))==1&cellsizes[1]>0) {type <- 2}
      }

      ez <- ezANOVA(data=data.frame(temp),dv=Scale,wid=userAnovaIDNum0,within=.(Ratings),detailed=T,type=type)
      Anova <- ez$ANOVA
      mauchly_string <- ""
      if(is.null(ez$`Mauchly's Test for Sphericity`)==F) {
        mauchly <- ez$`Mauchly's Test for Sphericity`
        if(mauchly$p[1]<=.05&mauchly$p[1]>=.001) {
          mauchly_string <- paste("\n","\n","Note: The sphericity assumption does not hold, W = ",wrap.rd(mauchly$W[1],2),", p = ",wrap.rd(mauchly$p[1],3),".",sep="")
        }
        if(mauchly$p[1]<.001) {
          mauchly_string <- paste("\n","\n","Note: The sphericity assumption does not hold, W = ",wrap.rd(mauchly$W[1],2),", p < .001.",sep="")
        }
        if(mauchly$p[1]>.05) {
          mauchly_string <- ""
        }
      }
      if(Anova$Effect[1]!="(Intercept)") {Anova <- rbind(NA,Anova); rownames(Anova) <- 1:nrow(Anova)}
      options(contrasts= c(x$contrasts[1],x$contrasts[2])) # reset contrasts to whatever they were before calling this function
      data_frame$userAnovaIDNum <- NULL

      if (Anova$p[2]<.001) {
        write_clip(allow_non_interactive = TRUE, content = paste("# F(",Anova$DFn[2],", ",Anova$DFd[2],") = ",wrap.rd0(Anova$"F"[2],2),", p < .001",", hp2 = ",wrap.rd(Anova$SSn[2]/(Anova$SSn[2]+Anova$SSd[2]),2),sep=""))
        return(cat("\n"," # F(",Anova$DFn[2],", ",Anova$DFd[2],") = ",wrap.rd0(Anova$"F"[2],2),", p < .001",", hp2 = ",wrap.rd(Anova$SSn[2]/(Anova$SSn[2]+Anova$SSd[2]),2),mauchly_string,sep=""))
      }

      else {
        write_clip(allow_non_interactive = TRUE, content = paste("# F(",Anova$DFn[2],", ",Anova$DFd[2],") = ",wrap.rd0(Anova$"F"[2],2),", p = ",wrap.rd(Anova$p[2],3),", hp2 = ",wrap.rd(Anova$SSn[2]/(Anova$SSn[2]+Anova$SSd[2]),2),sep=""))
        return(cat("\n"," # F(",Anova$DFn[2],", ",Anova$DFd[2],") = ",wrap.rd0(Anova$"F"[2],2),", p = ",wrap.rd(Anova$p[2],3),", hp2 = ",wrap.rd(Anova$SSn[2]/(Anova$SSn[2]+Anova$SSd[2]),2),mauchly_string,sep=""))
      }
    }

    ### Two-way ANOVA with one factor within-subjects
    if(is.null(iv1)==F&is.null(iv2)==T) {

      # assign variable names so that you can produce the IV names within the output as well
      iv1name <- substring(iv1_name,str_locate_all(pattern=coll('$'),iv1_name)[[1]][1]+1)
      temp0 <- colnames(df_temp)[1:ncol(dv1)]
      temp <- gather(data_frame,Ratings,Scale,one_of(temp0),factor_key=TRUE)
      temp$userAnovaIDNum0 <- factor(temp[[which(colnames(temp)==colnames(data_frame)[grep('^userAnovaIDNum', names(data_frame))[1]])]])

      # If the design is balanced, then convert type 1 ANOVA to type 2 ANOVA (they're identical for balanced designs)
      if(type==1) {
        cellsizes <- rep(0,nlevels(iv1)*nlevels(temp$Ratings))
        count <- 1
        for (i in 1:nlevels(iv1)) {
          for (l in 1:nlevels(temp$Ratings)) {
            cellsizes[count] <- sum(temp[[iv1name]]==levels(iv1)[i]&temp[["Ratings"]]==levels(temp$Ratings)[l])
            count <- count+1
          }
        }
        if(length(unique(cellsizes))==1&cellsizes[1]>0) {type <- 2}
      }

      ez <- eval(parse(text=paste0('ezANOVA(data=data.frame(temp),dv=Scale,wid=userAnovaIDNum0,within=.(Ratings),between=.(', sub('.*\\$', '', sub1)[3],'),detailed=T,type=type)')))
      Anova <- ez$ANOVA
      mauchly_string <- ""
      if(is.null(ez$`Mauchly's Test for Sphericity`)==F) {
        mauchly <- ez$`Mauchly's Test for Sphericity`
        if(mauchly$p[1]<=.05&mauchly$p[1]>=.001) {
          mauchly_string <- paste("\n","\n","Note: The sphericity assumption does not hold, W = ",wrap.rd(mauchly$W[1],2),", p = ",wrap.rd(mauchly$p[1],3),".",sep="")
        }
        if(mauchly$p[1]<.001) {
          mauchly_string <- paste("\n","\n","Note: The sphericity assumption does not hold, W = ",wrap.rd(mauchly$W[1],2),", p < .001.",sep="")
        }
        if(mauchly$p[1]>.05) {
          mauchly_string <- ""
        }
      }
      if(Anova$Effect[1]!="(Intercept)") {Anova <- rbind(NA,Anova); rownames(Anova) <- 1:nrow(Anova)}
      options(contrasts= c(x$contrasts[1],x$contrasts[2])) # reset contrasts to whatever they were before calling this function
      data_frame$userAnovaIDNum <- NULL

      write_clip(allow_non_interactive = TRUE, content = 
        paste("# ",iv1name," (main effect): F(",Anova$DFn[2],", ",Anova$DFd[2],") = ",wrap.rd0(Anova$F[2],2),", p",if (as.numeric(Anova$p[2]) < .001) {" < .001"},if (as.numeric(Anova$p[2]) >= .001) {" = "},if (as.numeric(Anova$p[2]) >= .001) {wrap.rd(Anova$p[2],3)},", hp2 = ",wrap.rd(Anova$SSn[2]/(Anova$SSn[2]+Anova$SSd[2]),2),
              "\n","# Within-Subjects (main effect): F(",Anova$DFn[3],", ",Anova$DFd[3],") = ",wrap.rd0(Anova$F[3],2),", p",if (as.numeric(Anova$p[3]) < .001) {" < .001"},if (as.numeric(Anova$p[3]) >= .001) {" = "},if (as.numeric(Anova$p[3]) >= .001) {wrap.rd(Anova$p[3],3)},", hp2 = ",wrap.rd(Anova$SSn[3]/(Anova$SSn[3]+Anova$SSd[3]),2),
              "\n","# ",iv1name," x Within-Subjects (2-way interaction): F(",Anova$DFn[4],", ",Anova$DFd[4],") = ",wrap.rd0(Anova$F[4],2),", p",if (as.numeric(Anova$p[4]) < .001) {" < .001"},if (as.numeric(Anova$p[4]) >= .001) {" = "},if (as.numeric(Anova$p[4]) >= .001) {wrap.rd(Anova$p[4],3)},", hp2 = ",wrap.rd(Anova$SSn[4]/(Anova$SSn[4]+Anova$SSd[4]),2),
              sep="")
      )
      return(
        cat("\n","# ",iv1name," (main effect): F(",Anova$DFn[2],", ",Anova$DFd[2],") = ",wrap.rd0(Anova$F[2],2),", p",if (as.numeric(Anova$p[2]) < .001) {" < .001"},if (as.numeric(Anova$p[2]) >= .001) {" = "},if (as.numeric(Anova$p[2]) >= .001) {wrap.rd(Anova$p[2],3)},", hp2 = ",wrap.rd(Anova$SSn[2]/(Anova$SSn[2]+Anova$SSd[2]),2),
            "\n","# Within-Subjects (main effect): F(",Anova$DFn[3],", ",Anova$DFd[3],") = ",wrap.rd0(Anova$F[3],2),", p",if (as.numeric(Anova$p[3]) < .001) {" < .001"},if (as.numeric(Anova$p[3]) >= .001) {" = "},if (as.numeric(Anova$p[3]) >= .001) {wrap.rd(Anova$p[3],3)},", hp2 = ",wrap.rd(Anova$SSn[3]/(Anova$SSn[3]+Anova$SSd[3]),2),
            "\n","# ",iv1name," x Within-Subjects (2-way interaction): F(",Anova$DFn[4],", ",Anova$DFd[4],") = ",wrap.rd0(Anova$F[4],2),", p",if (as.numeric(Anova$p[4]) < .001) {" < .001"},if (as.numeric(Anova$p[4]) >= .001) {" = "},if (as.numeric(Anova$p[4]) >= .001) {wrap.rd(Anova$p[4],3)},", hp2 = ",wrap.rd(Anova$SSn[4]/(Anova$SSn[4]+Anova$SSd[4]),2),
            mauchly_string,
            sep="")
      )
    }

    ### Three-way ANOVA with one factor within-subjects
    if(is.null(iv1)==F&is.null(iv2)==F&is.null(iv3)==T) {

      # assign variable names so that you can produce the IV names within the output as well
      iv1name <- substring(iv1_name,str_locate_all(pattern=coll('$'),iv1_name)[[1]][1]+1)
      iv2name <- substring(iv2_name,str_locate_all(pattern=coll('$'),iv2_name)[[1]][1]+1)
      temp0 <- colnames(df_temp)[1:ncol(dv1)]
      temp <- gather(data_frame,Ratings,Scale,one_of(temp0),factor_key=TRUE)
      temp$userAnovaIDNum0 <- factor(temp[[which(colnames(temp)==colnames(data_frame)[grep('^userAnovaIDNum', names(data_frame))[1]])]])

      # If the design is balanced, then convert type 1 ANOVA to type 2 ANOVA (they're identical for balanced designs)
      if(type==1) {
        cellsizes <- rep(0,nlevels(iv1)*nlevels(iv2)*nlevels(temp$Ratings))
        count <- 1
        for (i in 1:nlevels(iv1)) {
          for (j in 1:nlevels(iv2)) {
            for (l in 1:nlevels(temp$Ratings)) {
              cellsizes[count] <- sum(temp[[iv1name]]==levels(iv1)[i]&temp[[iv2name]]==levels(iv2)[j]&temp[["Ratings"]]==levels(temp$Ratings)[l])
              count <- count+1
            }
          }
        }
        if(length(unique(cellsizes))==1&cellsizes[1]>0) {type <- 2}
      }

      ez <- eval(parse(text=paste0('ezANOVA(data=data.frame(temp),dv=Scale,wid=userAnovaIDNum0,within=.(Ratings),between=.(', sub('.*\\$', '', sub1)[3],',', sub('.*\\$', '', sub2)[3],'),detailed=T,type=type)')))
      Anova <- ez$ANOVA
      mauchly_string <- ""
      if(is.null(ez$`Mauchly's Test for Sphericity`)==F) {
        mauchly <- ez$`Mauchly's Test for Sphericity`
        if(mauchly$p[1]<=.05&mauchly$p[1]>=.001) {
          mauchly_string <- paste("\n","\n","Note: The sphericity assumption does not hold, W = ",wrap.rd(mauchly$W[1],2),", p = ",wrap.rd(mauchly$p[1],3),".",sep="")
        }
        if(mauchly$p[1]<.001) {
          mauchly_string <- paste("\n","\n","Note: The sphericity assumption does not hold, W = ",wrap.rd(mauchly$W[1],2),", p < .001.",sep="")
        }
        if(mauchly$p[1]>.05) {
          mauchly_string <- ""
        }
      }
      if(Anova$Effect[1]!="(Intercept)") {Anova <- rbind(NA,Anova); rownames(Anova) <- 1:nrow(Anova)}
      options(contrasts= c(x$contrasts[1],x$contrasts[2])) # reset contrasts to whatever they were before calling this function
      data_frame$userAnovaIDNum <- NULL

      write_clip(allow_non_interactive = TRUE, content = 
        paste("# ",iv1name," (main effect): F(",Anova$DFn[2],", ",Anova$DFd[2],") = ",wrap.rd0(Anova$F[2],2),", p",if (as.numeric(Anova$p[2]) < .001) {" < .001"},if (as.numeric(Anova$p[2]) >= .001) {" = "},if (as.numeric(Anova$p[2]) >= .001) {wrap.rd(Anova$p[2],3)},", hp2 = ",wrap.rd(Anova$SSn[2]/(Anova$SSn[2]+Anova$SSd[2]),2),
              "\n","# ",iv2name," (main effect): F(",Anova$DFn[3],", ",Anova$DFd[3],") = ",wrap.rd0(Anova$F[3],2),", p",if (as.numeric(Anova$p[3]) < .001) {" < .001"},if (as.numeric(Anova$p[3]) >= .001) {" = "},if (as.numeric(Anova$p[3]) >= .001) {wrap.rd(Anova$p[3],3)},", hp2 = ",wrap.rd(Anova$SSn[3]/(Anova$SSn[3]+Anova$SSd[3]),2),
              "\n","# Within-Subjects (main effect): F(",Anova$DFn[4],", ",Anova$DFd[4],") = ",wrap.rd0(Anova$F[4],2),", p",if (as.numeric(Anova$p[4]) < .001) {" < .001"},if (as.numeric(Anova$p[4]) >= .001) {" = "},if (as.numeric(Anova$p[4]) >= .001) {wrap.rd(Anova$p[4],3)},", hp2 = ",wrap.rd(Anova$SSn[4]/(Anova$SSn[4]+Anova$SSd[4]),2),
              "\n","# ",iv1name," x ",iv2name," (2-way interaction): F(",Anova$DFn[5],", ",Anova$DFd[5],") = ",wrap.rd0(Anova$F[5],2),", p",if (as.numeric(Anova$p[5]) < .001) {" < .001"},if (as.numeric(Anova$p[5]) >= .001) {" = "},if (as.numeric(Anova$p[5]) >= .001) {wrap.rd(Anova$p[5],3)},", hp2 = ",wrap.rd(Anova$SSn[5]/(Anova$SSn[5]+Anova$SSd[5]),2),
              "\n","# ",iv1name," x Within-Subjects (2-way interaction): F(",Anova$DFn[6],", ",Anova$DFd[6],") = ",wrap.rd0(Anova$F[6],2),", p",if (as.numeric(Anova$p[6]) < .001) {" < .001"},if (as.numeric(Anova$p[6]) >= .001) {" = "},if (as.numeric(Anova$p[6]) >= .001) {wrap.rd(Anova$p[6],3)},", hp2 = ",wrap.rd(Anova$SSn[6]/(Anova$SSn[6]+Anova$SSd[6]),2),
              "\n","# ",iv2name," x Within-Subjects (2-way interaction): F(",Anova$DFn[7],", ",Anova$DFd[7],") = ",wrap.rd0(Anova$F[7],2),", p",if (as.numeric(Anova$p[7]) < .001) {" < .001"},if (as.numeric(Anova$p[7]) >= .001) {" = "},if (as.numeric(Anova$p[7]) >= .001) {wrap.rd(Anova$p[7],3)},", hp2 = ",wrap.rd(Anova$SSn[7]/(Anova$SSn[7]+Anova$SSd[7]),2),
              "\n","# ",iv1name," x ",iv2name," x Within-Subjects (3-way interaction): F(",Anova$DFn[8],", ",Anova$DFd[8],") = ",wrap.rd0(Anova$F[8],2),", p",if (as.numeric(Anova$p[8]) < .001) {" < .001"},if (as.numeric(Anova$p[8]) >= .001) {" = "},if (as.numeric(Anova$p[8]) >= .001) {wrap.rd(Anova$p[8],3)},", hp2 = ",wrap.rd(Anova$SSn[8]/(Anova$SSn[8]+Anova$SSd[8]),2),
              sep="")
      )
      return(
        cat("\n","# ",iv1name," (main effect): F(",Anova$DFn[2],", ",Anova$DFd[2],") = ",wrap.rd0(Anova$F[2],2),", p",if (as.numeric(Anova$p[2]) < .001) {" < .001"},if (as.numeric(Anova$p[2]) >= .001) {" = "},if (as.numeric(Anova$p[2]) >= .001) {wrap.rd(Anova$p[2],3)},", hp2 = ",wrap.rd(Anova$SSn[2]/(Anova$SSn[2]+Anova$SSd[2]),2),
            "\n","# ",iv2name," (main effect): F(",Anova$DFn[3],", ",Anova$DFd[3],") = ",wrap.rd0(Anova$F[3],2),", p",if (as.numeric(Anova$p[3]) < .001) {" < .001"},if (as.numeric(Anova$p[3]) >= .001) {" = "},if (as.numeric(Anova$p[3]) >= .001) {wrap.rd(Anova$p[3],3)},", hp2 = ",wrap.rd(Anova$SSn[3]/(Anova$SSn[3]+Anova$SSd[3]),2),
            "\n","# Within-Subjects (main effect): F(",Anova$DFn[4],", ",Anova$DFd[4],") = ",wrap.rd0(Anova$F[4],2),", p",if (as.numeric(Anova$p[4]) < .001) {" < .001"},if (as.numeric(Anova$p[4]) >= .001) {" = "},if (as.numeric(Anova$p[4]) >= .001) {wrap.rd(Anova$p[4],3)},", hp2 = ",wrap.rd(Anova$SSn[4]/(Anova$SSn[4]+Anova$SSd[4]),2),
            "\n","# ",iv1name," x ",iv2name," (2-way interaction): F(",Anova$DFn[5],", ",Anova$DFd[5],") = ",wrap.rd0(Anova$F[5],2),", p",if (as.numeric(Anova$p[5]) < .001) {" < .001"},if (as.numeric(Anova$p[5]) >= .001) {" = "},if (as.numeric(Anova$p[5]) >= .001) {wrap.rd(Anova$p[5],3)},", hp2 = ",wrap.rd(Anova$SSn[5]/(Anova$SSn[5]+Anova$SSd[5]),2),
            "\n","# ",iv1name," x Within-Subjects (2-way interaction): F(",Anova$DFn[6],", ",Anova$DFd[6],") = ",wrap.rd0(Anova$F[6],2),", p",if (as.numeric(Anova$p[6]) < .001) {" < .001"},if (as.numeric(Anova$p[6]) >= .001) {" = "},if (as.numeric(Anova$p[6]) >= .001) {wrap.rd(Anova$p[6],3)},", hp2 = ",wrap.rd(Anova$SSn[6]/(Anova$SSn[6]+Anova$SSd[6]),2),
            "\n","# ",iv2name," x Within-Subjects (2-way interaction): F(",Anova$DFn[7],", ",Anova$DFd[7],") = ",wrap.rd0(Anova$F[7],2),", p",if (as.numeric(Anova$p[7]) < .001) {" < .001"},if (as.numeric(Anova$p[7]) >= .001) {" = "},if (as.numeric(Anova$p[7]) >= .001) {wrap.rd(Anova$p[7],3)},", hp2 = ",wrap.rd(Anova$SSn[7]/(Anova$SSn[7]+Anova$SSd[7]),2),
            "\n","# ",iv1name," x ",iv2name," x Within-Subjects (3-way interaction): F(",Anova$DFn[8],", ",Anova$DFd[8],") = ",wrap.rd0(Anova$F[8],2),", p",if (as.numeric(Anova$p[8]) < .001) {" < .001"},if (as.numeric(Anova$p[8]) >= .001) {" = "},if (as.numeric(Anova$p[8]) >= .001) {wrap.rd(Anova$p[8],3)},", hp2 = ",wrap.rd(Anova$SSn[8]/(Anova$SSn[8]+Anova$SSd[8]),2),
            mauchly_string,
            sep="")
      )
    }

    ### Four-way ANOVA with one factor within-subjects
    if(is.data.frame(dv1)==T&is.null(iv1)==F&is.null(iv2)==F&is.null(iv3)==F) {

      # assign variable names so that you can produce the IV names within the output as well
      iv1name <- substring(iv1_name,str_locate_all(pattern=coll('$'),iv1_name)[[1]][1]+1)
      iv2name <- substring(iv2_name,str_locate_all(pattern=coll('$'),iv2_name)[[1]][1]+1)
      iv3name <- substring(iv3_name,str_locate_all(pattern=coll('$'),iv3_name)[[1]][1]+1)
      temp0 <- colnames(df_temp)[1:ncol(dv1)]
      temp <- gather(data_frame,Ratings,Scale,one_of(temp0),factor_key=TRUE)
      temp$userAnovaIDNum0 <- factor(temp[[which(colnames(temp)==colnames(data_frame)[grep('^userAnovaIDNum', names(data_frame))[1]])]])

      # If the design is balanced, then convert type 1 ANOVA to type 2 ANOVA (they're identical for balanced designs)
      if(type==1) {
        cellsizes <- rep(0,nlevels(iv1)*nlevels(iv2)*nlevels(iv3)*nlevels(temp$Ratings))
        count <- 1
        for (i in 1:nlevels(iv1)) {
          for (j in 1:nlevels(iv2)) {
            for (k in 1:nlevels(iv3)) {
              for (l in 1:nlevels(temp$Ratings)) {
                cellsizes[count] <- sum(temp[[iv1name]]==levels(iv1)[i]&temp[[iv2name]]==levels(iv2)[j]&temp[[iv3name]]==levels(iv3)[k]&temp[["Ratings"]]==levels(temp$Ratings)[l])
                count <- count+1
              }
            }
          }
        }
        if(length(unique(cellsizes))==1&cellsizes[1]>0) {type <- 2}
      }

      ez <- eval(parse(text=paste0('ezANOVA(data=data.frame(temp),dv=Scale,wid=userAnovaIDNum0,within=.(Ratings),between=.(', sub('.*\\$', '', sub1)[3],',',sub('.*\\$', '', sub2)[3],',',sub('.*\\$', '', sub3)[3],'),detailed=T,type=type)')))
      Anova <- ez$ANOVA
      mauchly_string <- ""
      if(is.null(ez$`Mauchly's Test for Sphericity`)==F) {
        mauchly <- ez$`Mauchly's Test for Sphericity`
        if(mauchly$p[1]<=.05&mauchly$p[1]>=.001) {
          mauchly_string <- paste("\n","\n","Note: The sphericity assumption does not hold, W = ",wrap.rd(mauchly$W[1],2),", p = ",wrap.rd(mauchly$p[1],3),".",sep="")
        }
        if(mauchly$p[1]<.001) {
          mauchly_string <- paste("\n","\n","Note: The sphericity assumption does not hold, W = ",wrap.rd(mauchly$W[1],2),", p < .001.",sep="")
        }
        if(mauchly$p[1]>.05) {
          mauchly_string <- ""
        }
      }
      if(Anova$Effect[1]!="(Intercept)") {Anova <- rbind(NA,Anova); rownames(Anova) <- 1:nrow(Anova)}
      options(contrasts= c(x$contrasts[1],x$contrasts[2])) # reset contrasts to whatever they were before calling this function
      data_frame$userAnovaIDNum <- NULL

      write_clip(allow_non_interactive = TRUE, content = 
        paste("# ", iv1name," (main effect): F(",Anova$DFn[2],", ",Anova$DFd[2],") = ",wrap.rd0(Anova$F[2],2),", p",if (as.numeric(Anova$p[2]) < .001) {" < .001"},if (as.numeric(Anova$p[2]) >= .001) {" = "},if (as.numeric(Anova$p[2]) >= .001) {wrap.rd(Anova$p[2],3)},", hp2 = ",wrap.rd(Anova$SSn[2]/(Anova$SSn[2]+Anova$SSd[2]),2),
              "\n","# ",iv2name, " (main effect): F(",Anova$DFn[3],", ",Anova$DFd[3],") = ",wrap.rd0(Anova$F[3],2),", p",if (as.numeric(Anova$p[3]) < .001) {" < .001"},if (as.numeric(Anova$p[3]) >= .001) {" = "},if (as.numeric(Anova$p[3]) >= .001) {wrap.rd(Anova$p[3],3)},", hp2 = ",wrap.rd(Anova$SSn[3]/(Anova$SSn[3]+Anova$SSd[3]),2),
              "\n","# ",iv3name, " (main effect): F(",Anova$DFn[4],", ",Anova$DFd[4],") = ",wrap.rd0(Anova$F[4],2),", p",if (as.numeric(Anova$p[4]) < .001) {" < .001"},if (as.numeric(Anova$p[4]) >= .001) {" = "},if (as.numeric(Anova$p[4]) >= .001) {wrap.rd(Anova$p[4],3)},", hp2 = ",wrap.rd(Anova$SSn[4]/(Anova$SSn[4]+Anova$SSd[4]),2),
              "\n","# Within-Subjects (main effect): F(",Anova$DFn[5],", ",Anova$DFd[5],") = ",wrap.rd0(Anova$F[5],2),", p",if (as.numeric(Anova$p[5]) < .001) {" < .001"},if (as.numeric(Anova$p[5]) >= .001) {" = "},if (as.numeric(Anova$p[5]) >= .001) {wrap.rd(Anova$p[5],3)},", hp2 = ",wrap.rd(Anova$SSn[5]/(Anova$SSn[5]+Anova$SSd[5]),2),
              "\n","# ",iv1name," x ",iv2name," (2-way interaction): F(",Anova$DFn[6],", ",Anova$DFd[6],") = ",wrap.rd0(Anova$F[6],2),", p",if (as.numeric(Anova$p[6]) < .001) {" < .001"},if (as.numeric(Anova$p[6]) >= .001) {" = "},if (as.numeric(Anova$p[6]) >= .001) {wrap.rd(Anova$p[6],3)},", hp2 = ",wrap.rd(Anova$SSn[6]/(Anova$SSn[6]+Anova$SSd[6]),2),
              "\n","# ",iv1name," x ",iv3name," (2-way interaction): F(",Anova$DFn[7],", ",Anova$DFd[7],") = ",wrap.rd0(Anova$F[7],2),", p",if (as.numeric(Anova$p[7]) < .001) {" < .001"},if (as.numeric(Anova$p[7]) >= .001) {" = "},if (as.numeric(Anova$p[7]) >= .001) {wrap.rd(Anova$p[7],3)},", hp2 = ",wrap.rd(Anova$SSn[7]/(Anova$SSn[7]+Anova$SSd[7]),2),
              "\n","# ",iv2name," x ",iv3name," (2-way interaction): F(",Anova$DFn[8],", ",Anova$DFd[8],") = ",wrap.rd0(Anova$F[8],2),", p",if (as.numeric(Anova$p[8]) < .001) {" < .001"},if (as.numeric(Anova$p[8]) >= .001) {" = "},if (as.numeric(Anova$p[8]) >= .001) {wrap.rd(Anova$p[8],3)},", hp2 = ",wrap.rd(Anova$SSn[8]/(Anova$SSn[8]+Anova$SSd[8]),2),
              "\n","# ",iv1name," x Within-Subjects (2-way interaction): F(",Anova$DFn[9],", ",Anova$DFd[9],") = ",wrap.rd0(Anova$F[9],2),", p",if (as.numeric(Anova$p[9]) < .001) {" < .001"},if (as.numeric(Anova$p[9]) >= .001) {" = "},if (as.numeric(Anova$p[9]) >= .001) {wrap.rd(Anova$p[9],3)},", hp2 = ",wrap.rd(Anova$SSn[9]/(Anova$SSn[9]+Anova$SSd[9]),2),
              "\n","# ",iv2name," x Within-Subjects (2-way interaction): F(",Anova$DFn[10],", ",Anova$DFd[10],") = ",wrap.rd0(Anova$F[10],2),", p",if (as.numeric(Anova$p[10]) < .001) {" < .001"},if (as.numeric(Anova$p[10]) >= .001) {" = "},if (as.numeric(Anova$p[10]) >= .001) {wrap.rd(Anova$p[10],3)},", hp2 = ",wrap.rd(Anova$SSn[10]/(Anova$SSn[10]+Anova$SSd[10]),2),
              "\n","# ",iv3name," x Within-Subjects (2-way interaction): F(",Anova$DFn[11],", ",Anova$DFd[11],") = ",wrap.rd0(Anova$F[11],2),", p",if (as.numeric(Anova$p[11]) < .001) {" < .001"},if (as.numeric(Anova$p[11]) >= .001) {" = "},if (as.numeric(Anova$p[11]) >= .001) {wrap.rd(Anova$p[11],3)},", hp2 = ",wrap.rd(Anova$SSn[11]/(Anova$SSn[11]+Anova$SSd[11]),2),
              "\n","# ",iv1name," x ",iv2name," x ",iv3name," (3-way interaction): F(",Anova$DFn[12],", ",Anova$DFd[12],") = ",wrap.rd0(Anova$F[12],2),", p",if (as.numeric(Anova$p[12]) < .001) {" < .001"},if (as.numeric(Anova$p[12]) >= .001) {" = "},if (as.numeric(Anova$p[12]) >= .001) {wrap.rd(Anova$p[12],3)},", hp2 = ",wrap.rd(Anova$SSn[12]/(Anova$SSn[12]+Anova$SSd[12]),2),
              "\n","# ",iv1name," x ",iv2name," x Within-Subjects (3-way interaction): F(",Anova$DFn[13],", ",Anova$DFd[13],") = ",wrap.rd0(Anova$F[13],2),", p",if (as.numeric(Anova$p[13]) < .001) {" < .001"},if (as.numeric(Anova$p[13]) >= .001) {" = "},if (as.numeric(Anova$p[13]) >= .001) {wrap.rd(Anova$p[13],3)},", hp2 = ",wrap.rd(Anova$SSn[13]/(Anova$SSn[13]+Anova$SSd[13]),2),
              "\n","# ",iv1name," x ",iv3name," x Within-Subjects (3-way interaction): F(",Anova$DFn[14],", ",Anova$DFd[14],") = ",wrap.rd0(Anova$F[14],2),", p",if (as.numeric(Anova$p[14]) < .001) {" < .001"},if (as.numeric(Anova$p[14]) >= .001) {" = "},if (as.numeric(Anova$p[14]) >= .001) {wrap.rd(Anova$p[14],3)},", hp2 = ",wrap.rd(Anova$SSn[14]/(Anova$SSn[14]+Anova$SSd[14]),2),
              "\n","# ",iv2name," x ",iv3name," x Within-Subjects (3-way interaction): F(",Anova$DFn[15],", ",Anova$DFd[15],") = ",wrap.rd0(Anova$F[15],2),", p",if (as.numeric(Anova$p[15]) < .001) {" < .001"},if (as.numeric(Anova$p[15]) >= .001) {" = "},if (as.numeric(Anova$p[15]) >= .001) {wrap.rd(Anova$p[15],3)},", hp2 = ",wrap.rd(Anova$SSn[15]/(Anova$SSn[15]+Anova$SSd[15]),2),
              "\n","# ",iv1name," x ",iv2name," x ",iv3name," x Within-Subjects (4-way interaction): F(",Anova$DFn[16],", ",Anova$DFd[16],") = ",wrap.rd0(Anova$F[16],2),", p",if (as.numeric(Anova$p[16]) < .001) {" < .001"},if (as.numeric(Anova$p[16]) >= .001) {" = "},if (as.numeric(Anova$p[16]) >= .001) {wrap.rd(Anova$p[16],3)},", hp2 = ",wrap.rd(Anova$SSn[16]/(Anova$SSn[16]+Anova$SSd[16]),2),
              sep="")
      )
      return(
        cat("\n","# ", iv1name," (main effect): F(",Anova$DFn[2],", ",Anova$DFd[2],") = ",wrap.rd0(Anova$F[2],2),", p",if (as.numeric(Anova$p[2]) < .001) {" < .001"},if (as.numeric(Anova$p[2]) >= .001) {" = "},if (as.numeric(Anova$p[2]) >= .001) {wrap.rd(Anova$p[2],3)},", hp2 = ",wrap.rd(Anova$SSn[2]/(Anova$SSn[2]+Anova$SSd[2]),2),
            "\n","# ",iv2name, " (main effect): F(",Anova$DFn[3],", ",Anova$DFd[3],") = ",wrap.rd0(Anova$F[3],2),", p",if (as.numeric(Anova$p[3]) < .001) {" < .001"},if (as.numeric(Anova$p[3]) >= .001) {" = "},if (as.numeric(Anova$p[3]) >= .001) {wrap.rd(Anova$p[3],3)},", hp2 = ",wrap.rd(Anova$SSn[3]/(Anova$SSn[3]+Anova$SSd[3]),2),
            "\n","# ",iv3name, " (main effect): F(",Anova$DFn[4],", ",Anova$DFd[4],") = ",wrap.rd0(Anova$F[4],2),", p",if (as.numeric(Anova$p[4]) < .001) {" < .001"},if (as.numeric(Anova$p[4]) >= .001) {" = "},if (as.numeric(Anova$p[4]) >= .001) {wrap.rd(Anova$p[4],3)},", hp2 = ",wrap.rd(Anova$SSn[4]/(Anova$SSn[4]+Anova$SSd[4]),2),
            "\n","# Within-Subjects (main effect): F(",Anova$DFn[5],", ",Anova$DFd[5],") = ",wrap.rd0(Anova$F[5],2),", p",if (as.numeric(Anova$p[5]) < .001) {" < .001"},if (as.numeric(Anova$p[5]) >= .001) {" = "},if (as.numeric(Anova$p[5]) >= .001) {wrap.rd(Anova$p[5],3)},", hp2 = ",wrap.rd(Anova$SSn[5]/(Anova$SSn[5]+Anova$SSd[5]),2),
            "\n","# ",iv1name," x ",iv2name," (2-way interaction): F(",Anova$DFn[6],", ",Anova$DFd[6],") = ",wrap.rd0(Anova$F[6],2),", p",if (as.numeric(Anova$p[6]) < .001) {" < .001"},if (as.numeric(Anova$p[6]) >= .001) {" = "},if (as.numeric(Anova$p[6]) >= .001) {wrap.rd(Anova$p[6],3)},", hp2 = ",wrap.rd(Anova$SSn[6]/(Anova$SSn[6]+Anova$SSd[6]),2),
            "\n","# ",iv1name," x ",iv3name," (2-way interaction): F(",Anova$DFn[7],", ",Anova$DFd[7],") = ",wrap.rd0(Anova$F[7],2),", p",if (as.numeric(Anova$p[7]) < .001) {" < .001"},if (as.numeric(Anova$p[7]) >= .001) {" = "},if (as.numeric(Anova$p[7]) >= .001) {wrap.rd(Anova$p[7],3)},", hp2 = ",wrap.rd(Anova$SSn[7]/(Anova$SSn[7]+Anova$SSd[7]),2),
            "\n","# ",iv2name," x ",iv3name," (2-way interaction): F(",Anova$DFn[8],", ",Anova$DFd[8],") = ",wrap.rd0(Anova$F[8],2),", p",if (as.numeric(Anova$p[8]) < .001) {" < .001"},if (as.numeric(Anova$p[8]) >= .001) {" = "},if (as.numeric(Anova$p[8]) >= .001) {wrap.rd(Anova$p[8],3)},", hp2 = ",wrap.rd(Anova$SSn[8]/(Anova$SSn[8]+Anova$SSd[8]),2),
            "\n","# ",iv1name," x Within-Subjects (2-way interaction): F(",Anova$DFn[9],", ",Anova$DFd[9],") = ",wrap.rd0(Anova$F[9],2),", p",if (as.numeric(Anova$p[9]) < .001) {" < .001"},if (as.numeric(Anova$p[9]) >= .001) {" = "},if (as.numeric(Anova$p[9]) >= .001) {wrap.rd(Anova$p[9],3)},", hp2 = ",wrap.rd(Anova$SSn[9]/(Anova$SSn[9]+Anova$SSd[9]),2),
            "\n","# ",iv2name," x Within-Subjects (2-way interaction): F(",Anova$DFn[10],", ",Anova$DFd[10],") = ",wrap.rd0(Anova$F[10],2),", p",if (as.numeric(Anova$p[10]) < .001) {" < .001"},if (as.numeric(Anova$p[10]) >= .001) {" = "},if (as.numeric(Anova$p[10]) >= .001) {wrap.rd(Anova$p[10],3)},", hp2 = ",wrap.rd(Anova$SSn[10]/(Anova$SSn[10]+Anova$SSd[10]),2),
            "\n","# ",iv3name," x Within-Subjects (2-way interaction): F(",Anova$DFn[11],", ",Anova$DFd[11],") = ",wrap.rd0(Anova$F[11],2),", p",if (as.numeric(Anova$p[11]) < .001) {" < .001"},if (as.numeric(Anova$p[11]) >= .001) {" = "},if (as.numeric(Anova$p[11]) >= .001) {wrap.rd(Anova$p[11],3)},", hp2 = ",wrap.rd(Anova$SSn[11]/(Anova$SSn[11]+Anova$SSd[11]),2),
            "\n","# ",iv1name," x ",iv2name," x ",iv3name," (3-way interaction): F(",Anova$DFn[12],", ",Anova$DFd[12],") = ",wrap.rd0(Anova$F[12],2),", p",if (as.numeric(Anova$p[12]) < .001) {" < .001"},if (as.numeric(Anova$p[12]) >= .001) {" = "},if (as.numeric(Anova$p[12]) >= .001) {wrap.rd(Anova$p[12],3)},", hp2 = ",wrap.rd(Anova$SSn[12]/(Anova$SSn[12]+Anova$SSd[12]),2),
            "\n","# ",iv1name," x ",iv2name," x Within-Subjects (3-way interaction): F(",Anova$DFn[13],", ",Anova$DFd[13],") = ",wrap.rd0(Anova$F[13],2),", p",if (as.numeric(Anova$p[13]) < .001) {" < .001"},if (as.numeric(Anova$p[13]) >= .001) {" = "},if (as.numeric(Anova$p[13]) >= .001) {wrap.rd(Anova$p[13],3)},", hp2 = ",wrap.rd(Anova$SSn[13]/(Anova$SSn[13]+Anova$SSd[13]),2),
            "\n","# ",iv1name," x ",iv3name," x Within-Subjects (3-way interaction): F(",Anova$DFn[14],", ",Anova$DFd[14],") = ",wrap.rd0(Anova$F[14],2),", p",if (as.numeric(Anova$p[14]) < .001) {" < .001"},if (as.numeric(Anova$p[14]) >= .001) {" = "},if (as.numeric(Anova$p[14]) >= .001) {wrap.rd(Anova$p[14],3)},", hp2 = ",wrap.rd(Anova$SSn[14]/(Anova$SSn[14]+Anova$SSd[14]),2),
            "\n","# ",iv2name," x ",iv3name," x Within-Subjects (3-way interaction): F(",Anova$DFn[15],", ",Anova$DFd[15],") = ",wrap.rd0(Anova$F[15],2),", p",if (as.numeric(Anova$p[15]) < .001) {" < .001"},if (as.numeric(Anova$p[15]) >= .001) {" = "},if (as.numeric(Anova$p[15]) >= .001) {wrap.rd(Anova$p[15],3)},", hp2 = ",wrap.rd(Anova$SSn[15]/(Anova$SSn[15]+Anova$SSd[15]),2),
            "\n","# ",iv1name," x ",iv2name," x ",iv3name," x Within-Subjects (4-way interaction): F(",Anova$DFn[16],", ",Anova$DFd[16],") = ",wrap.rd0(Anova$F[16],2),", p",if (as.numeric(Anova$p[16]) < .001) {" < .001"},if (as.numeric(Anova$p[16]) >= .001) {" = "},if (as.numeric(Anova$p[16]) >= .001) {wrap.rd(Anova$p[16],3)},", hp2 = ",wrap.rd(Anova$SSn[16]/(Anova$SSn[16]+Anova$SSd[16]),2),
            mauchly_string,
            sep="")
      )
    }
  }
}
