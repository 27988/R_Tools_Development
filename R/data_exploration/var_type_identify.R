
#' Identify numeric/integer and categorical/character variables.
#'
#' @param df A dataframe.
#' @param exclude_vars A vector of variables to be excluded.
#' @return List of numeric and categorical variables.
#' @examples
#' var_type <- var_type_identify(data=data,exclude_vars=NULL)
#' 


 var_type_identify <- function (df,exclude_vars=NULL) {
   
   #Identify numeric variables 
        numVars <- NA
   
   if (is.na(numVars[1])) {
     for (i in 1:dim(df)[2]) {
       yesnum <-  is.numeric(df[,i]) | is.integer(df[,i])
       numVars <- append(numVars,ifelse(yesnum == "TRUE", colnames(df)[i],numVars),after = length(numVars))
     }
     
   }
   numVars <- na.omit(numVars)
   numVars <- numVars[!(numVars %in% exclude_vars)]
   
   #Identify character/categorical variables
  
     categoricalVars <- NA
   
   if (is.na(categoricalVars[1])) {
     categoricalVars <- NA
     for (i in 1:dim(df)[2]) {
       yescat <-  is.character(df[,i]) | is.factor(df[,i])
       categoricalVars <- append(categoricalVars,ifelse(yescat == "TRUE", colnames(df)[i],categoricalVars),after = length(categoricalVars))
     }
   }
   categoricalVars <- na.omit(categoricalVars)
   categoricalVars <- categoricalVars[!(categoricalVars %in% exclude_vars)]
   
   return(list(numVars,categoricalVars))
 }
 
 #Test 
 data = readRDS("/stats/projects/all/R_Tools_Development/data/salaries_data.Rds")
 
 var_type <- var_type_identify(df=data,exclude_vars=NULL)
 
 