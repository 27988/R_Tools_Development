
#' Identify numeric/integer and categorical/character variables.
#'
#' @param data A dataframe.
#' @param excludeVars A vector of variables to be excluded.
#' @return List of numeric and categorical variables.
#' @examples
#' var_type <- var_type_identify(data=data,excludeVars=NULL)
#' 


 var_type_identify <- function (data,excludeVars=NULL) {
   
   #Identify numeric variables 
        numVars <- NA
   
   if (is.na(numVars[1])) {
     for (i in 1:dim(data)[2]) {
       yesnum <-  is.numeric(data[,i]) | is.integer(data[,i])
       numVars <- append(numVars,ifelse(yesnum == "TRUE", colnames(data)[i],numVars),after = length(numVars))
     }
     
   }
   numVars <- na.omit(numVars)
   numVars <- numVars[!(numVars %in% excludeVars)]
   
   #Identify character/categorical variables
  
     categoricalVars <- NA
   
   if (is.na(categoricalVars[1])) {
     categoricalVars <- NA
     for (i in 1:dim(data)[2]) {
       yescat <-  is.character(data[,i]) | is.factor(data[,i])
       categoricalVars <- append(categoricalVars,ifelse(yescat == "TRUE", colnames(data)[i],categoricalVars),after = length(categoricalVars))
     }
   }
   categoricalVars <- na.omit(categoricalVars)
   categoricalVars <- categoricalVars[!(categoricalVars %in% excludeVars)]
   
   return(list(numVars,categoricalVars))
 }
 
 #Test 
 data = readRDS("/stats/projects/all/R_Tools_Development/data/salaries_data.Rds")
 
 var_type <- var_type_identify(data=data,excludeVars=NULL)
 
 