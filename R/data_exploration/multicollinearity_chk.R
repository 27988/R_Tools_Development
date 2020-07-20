#' Check for Multicollinearity among candidate predictors.
#'
#' @param data A dataframe.
#' @param excludeVars A vector of variables to be excluded form multicollinearity check.
#' @param numVars A vector of numeric variables (optional).
#' @param categoricalVars A vector of character (categorical) variables (optional).
#' @param cutoff Cutoff of correlation coefficient or Cramer's V coefficient to be used to output correlated variables.
#' @param corrmethod Correlation coefficient method to be used, Default is 'pearson'. Options are 'kendall', 'spearman'.
#' @param outtype Type of output file.
#' @param outpath Path of output file if xls or xlsx outtype selected.
#' @return A list of 2 dataframes; correlation analysis of numeric variables are stored as list1 and categorical analysis is stored as list2.
#' @examples
#' multi_out <- multicoll_chk(data=data,numVars=c("yrs.service","yrs.since.phd"),categoricalVars = c("rank","discipline"))
#' multi_out <- multicoll_chk(data=data)
#' multi_out <- multicoll_chk(data=data,excludeVars="patient_id")
#' multi_out <- multicoll_chk(data=data,excludeVars=NULL,outtype="xls",outpath="/stats/projects/all/R_Tools_Development/data/multicoll_chk.xls")


multicoll_chk <- function(data,excludeVars=NULL,numVars=NULL,categoricalVars=NULL,cutoff=0,corrmethod="pearson",outtype="list",outpath) {
  
  #Identify numeric variables if is.null(numVars)
  if (is.null(numVars)) {
    numVars <- NA
}
  if (is.na(numVars[1])) {
    for (i in 1:dim(data)[2]) {
      yesnum <-  is.numeric(data[,i])
      numVars <- append(numVars,ifelse(yesnum == "TRUE", colnames(data)[i],numVars),after = length(numVars))
    }
    
  }
  numVars <- na.omit(numVars)
  numVars <- numVars[!(numVars %in% excludeVars)]
  
  #Identify character/categorical variables if is.null(categoricalVars)
  if (is.null(categoricalVars)) {
    categoricalVars <- NA
  }
  if (is.na(categoricalVars[1])) {
    categoricalVars <- NA
    for (i in 1:dim(data)[2]) {
      yescat <-  is.character(data[,i]) | is.factor(data[,i])
      categoricalVars <- append(categoricalVars,ifelse(yescat == "TRUE", colnames(data)[i],categoricalVars),after = length(categoricalVars))
    }
  }
  categoricalVars <- na.omit(categoricalVars)
  categoricalVars <- categoricalVars[!(categoricalVars %in% excludeVars)]
  
  #Correlation for categorical variables
  library(pedometrics)
  
  if (is.null(categoricalVars)) {
    message("No numeric variables in dataset")
  }  else {    cramer_d <- data[,which(colnames(data) %in% categoricalVars)]}
  
  if (length(cramer_d) > 0 ) {
 
  cramer_vars <- matrix(0,((dim(cramer_d)[2]*(dim(cramer_d)[2]-1))/2),3)
 
  cramer_v <- cramer(cramer_d)
  
    c=0
    for (j in 1:dim(cramer_v)[2]) {
      if ((j+1) <= dim(cramer_v)[2]) {for (k in (j+1):dim(cramer_v)[2]) {
        c=c+1
       cramer_vars[c,1] <- rownames(cramer_v)[j]
       cramer_vars[c,2] <- colnames(cramer_v)[k]
       cramer_vars[c,3] <- cramer_v[j,k]
     }
    }
   }
    cramer_vars <- data.frame(cramer_vars)
    colnames(cramer_vars) <- c("Variable1", "Variable2", "Coeff")
    cramer_vars$Coeff <- as.numeric(as.character(cramer_vars$Coeff))
  
  #Track the variables that are highly correlated
  corr_categorical <- cramer_vars[cramer_vars[["Coeff"]] >= cutoff,]
  
}

  #Correlation for numeric variables
  if (is.null(numVars)) {
    message("No numeric variables in dataset")
  } else {corr_d <- data[,which(colnames(data) %in% numVars)]}
  
  if (length(corr_d) > 0 ) {
    
    corr_vars <- matrix(0,((dim(corr_d)[2]*(dim(corr_d)[2]-1))/2),3)
    
    corr_v <- cor(corr_d, use="complete.obs", method=corrmethod)
    
    c=0
    for (j in 1:dim(corr_v)[2]) {
      if ((j+1) <= dim(corr_v)[2]) {for (k in (j+1):dim(corr_v)[2]) {
        c=c+1
        corr_vars[c,1] <- rownames(corr_v)[j]
        corr_vars[c,2] <- colnames(corr_v)[k]
        corr_vars[c,3] <- corr_v[j,k]
      }
      }
    }
    
   
    corr_vars <- data.frame(corr_vars)
    colnames(corr_vars) <- c("Variable1", "Variable2", "Coeff")
    corr_vars$Coeff <- as.numeric(as.character(corr_vars$Coeff))
    
    #Track the variables that are highly correlated
    corr_num <- corr_vars[corr_vars[["Coeff"]] >= cutoff,]
    
    
  }
  
  
  
  if (outtype == "list"){
    corr_list <- list()
    corr_list$numeric_list <- corr_num
    corr_list$categorical_list <- corr_categorical
    return(corr_list)
  } else if (tolower(outtype) == "xls" | tolower(outtype) == "xlsx") {
    
    write.xlsx(corr_num, file=outpath, sheetName="Numeric List", row.names=FALSE)
    write.xlsx(corr_categorical, file=outpath, sheetName="Categorical List", append=TRUE, row.names=FALSE)
    
  } else stop("ERROR: Type of output file should either be list, xls or xlsx")
}


#Test 
library(xlsx)
data = readRDS("/stats/projects/all/R_Tools_Development/data/salaries_data.Rds")

a = multicoll_chk(data=data,numVars=c("yrs.service","yrs.since.phd"),categoricalVars = c("rank","discipline"))
a = multicoll_chk(data=data)
a = multicoll_chk(data=data,excludeVars="patient_id")
a = multicoll_chk(data=data,excludeVars=NULL,outtype="xls",outpath="/stats/projects/all/R_Tools_Development/data/multicoll_chk.xls")



