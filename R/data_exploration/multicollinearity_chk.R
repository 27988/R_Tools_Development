#' Check for Multicollinearity among candidate predictors.
#'
#' @param data A dataframe.
#' @param varlist A vector of numeric variables to be imputed.
#' @param type_num Type of imputation for numeric variables to be used. Mean is Default method.
#' @param round Upto which decimal should imputed numeric values be rounded. Default rounds to whole number.
#' @param factvarlist A vector of factor variables to be imputed.
#' @param type_fact Type of imputation for factor variables to be used. Add 'missing' level is Default method.
#' @return input dataset with imputed values.
#' @examples
#' data <- impute(data=data,numvarlist=c("salary","yrs.service"),type_num = "mean",round=0,factvarlist=c("rank"),type_fact = "mode")
#' data <- impute(data=data,numvarlist=c("salary","yrs.service"),type_num = "median",round=2,factvarlist=c("rank"),type_fact="missing")
#' 


multicoll_chk <- function(data,numVars=NULL,categoricalVars=NULL,remove.corr='N',corrmethod="pearson") {
  
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
  
  #Correlation for categorical variables
  library(pedometrics)
  
  if (is.null(categoricalVars)) {
    message("No numeric variables in dataset")
  }  else {    cramer_d <- data[,which(colnames(data) %in% categoricalVars)]}
  
  if (length(cramer_d) > 0 ) {
  cramer_v <- as.vector(0)
  cramer_vars <- matrix(0,dim(cramer_d)[2]-1,2)
  
  for (i in 1:(dim(cramer_d)[2]-1)){
    cramer_v[i] <- cramer(cramer_d[,c(i,i+1)])[1,2]
    cramer_vars[i,] <- colnames(cramer(cramer_d[,c(i,i+1)]))
    }
  
  #Track the variables that are highly correlated
  corr_categorical <- cramer_vars[which(cramer_v > 0.5),]
  excl_cat_list <- matrix(cramer_vars[which(cramer_v > 0.5),]); excl_cat_list <- excl_cat_list[((dim(excl_cat_list)[1]/2)+1):dim(excl_cat_list)[1]]
  if (remove.corr == "Y") {data <- data[,which(!colnames(data) %in% excl_cat_list)] }
}

  #Correlation for numeric variables
  if (is.null(numVars)) {
    message("No numeric variables in dataset")
  } else {corr_d <- data[,which(colnames(data) %in% numVars)]}
  
  if (length(corr_d) > 0 ) {
    corr_v <- as.vector(0)
    corr_vars <- matrix(0,dim(corr_d)[2]-1,2)
    
    for (i in 1:(dim(corr_d)[2]-1)){
      corr_v[i] <- cor(corr_d[,c(i,i+1)], use="complete.obs", method=corrmethod)[1,2]
      corr_vars[i,] <- colnames(cor(corr_d[,c(i,i+1)], use="complete.obs", method=corrmethod))
    }
    
    #Track the variables that are highly correlated
    corr_num <- corr_vars[which(corr_v > 0.5),]
    excl_num_list <- matrix(corr_vars[which(corr_v > 0.5),]); excl_num_list <- excl_num_list[((dim(excl_num_list)[1]/2)+1):dim(excl_num_list)[1]]
    if (remove.corr == "Y") {data <- data[,which(!colnames(data) %in% excl_num_list)] }
    
  }
  
    corr_list <- list()
    corr_list$num_list <- ifelse(length(excl_num_list) >0, excl_num_list, NA)
    corr_list$cat_list <- ifelse(length(excl_cat_list) >0, excl_cat_list, NA)
    
    return(corr_list)
}


#Test 
library(xlsx)
data = readRDS("/stats/projects/all/R_Tools_Development/data/salaries_data.Rds")

a = multicoll_chk(data=data,numVars=c("yrs.service","yrs.since.phd"),categoricalVars = c("rank","discipline"),remove.corr="Y")
a = multicoll_chk(data=data,remove.corr="N")

