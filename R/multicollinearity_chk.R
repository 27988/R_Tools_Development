#' Check for Multicollinearity among candidate predictors.
#'
#' @param df A dataframe.
#' @param exclude_vars A vector of variables to be excluded form multicollinearity check.
#' @param num_vars A vector of numeric variables (optional).
#' @param factor_vars A vector of character (categorical) variables (optional).
#' @param cutoff Cutoff of correlation coefficient or Cramer's V coefficient to be used to output correlated variables.
#' @param  corr_method Correlation coefficient method to be used, Default is 'pearson'. Options are 'kendall', 'spearman'.
#' @param filetype_out Type of output file.
#' @param output_path Path of output file if xls or xlsx filetype_out selected.
#' @return A list of 2 dataframes; correlation analysis of numeric variables are stored as list1 and categorical analysis is stored as list2.
#' @examples
#' multi_out <- multicoll_chk(df=data,num_vars=c("yrs.service","yrs.since.phd"),factor_vars = c("rank","discipline"))
#' multi_out <- multicoll_chk(df=data)
#' multi_out <- multicoll_chk(df=data,exclude_vars="patient_id")
#' multi_out <- multicoll_chk(df=data,exclude_vars=NULL,filetype_out="xls",output_path="/stats/projects/all/R_Tools_Development/data/multicoll_chk.xls")


multicoll_chk <- function(df,exclude_vars=NULL,num_vars=NULL,factor_vars=NULL,cutoff=0, corr_method="pearson",filetype_out="list",output_path) {
  
  #Input dataset must have class as data.frame only
  data = data.frame(df)
  
  var_type <- var_type_identify(df=data,exclude_vars=exclude_vars)
  #Identify numeric variables if is.null(num_vars)
  if (is.null(num_vars)) {
    num_vars <- var_type[[1]]
}
  
  #Identify character/categorical variables if is.null(factor_vars)
  if (is.null(factor_vars)) {
    factor_vars <- var_type[[2]]
  }
  
  
  #Correlation for categorical variables
  library(pedometrics)
  
  if (is.null(factor_vars)) {
    message("No numeric variables in dataset")
  }  else {    cramer_d <- data[,which(colnames(data) %in% factor_vars)]}
  
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
  if (is.null(num_vars)) {
    message("No numeric variables in dataset")
  } else {corr_d <- data[,which(colnames(data) %in% num_vars)]}
  
  if (length(corr_d) > 0 ) {
    
    corr_vars <- matrix(0,((dim(corr_d)[2]*(dim(corr_d)[2]-1))/2),3)
    
    corr_v <- cor(corr_d, use="complete.obs", method= corr_method)
    
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
  
  
  
  if (filetype_out == "list"){
    corr_list <- list()
    corr_list$numeric_list <- corr_num
    corr_list$categorical_list <- corr_categorical
    return(corr_list)
  } else if (tolower(filetype_out) == "xls" | tolower(filetype_out) == "xlsx") {
    
    write.xlsx(corr_num, file=output_path, sheetName="Numeric List", row.names=FALSE)
    write.xlsx(corr_categorical, file=output_path, sheetName="Categorical List", append=TRUE, row.names=FALSE)
    
  } else stop("ERROR: Type of output file should either be list, xls or xlsx")
}


#Test 
library(xlsx)
data = readRDS("/stats/projects/all/R_Tools_Development/data/salaries_data.Rds")

a = multicoll_chk(df=data,num_vars=c("yrs.service","yrs.since.phd"),factor_vars = c("rank","discipline"))
a = multicoll_chk(df=data)
a = multicoll_chk(df=data,exclude_vars="patient_id")
a = multicoll_chk(df=data,exclude_vars=NULL,filetype_out="xls",output_path="/stats/projects/all/R_Tools_Development/data/multicoll_chk.xls")



