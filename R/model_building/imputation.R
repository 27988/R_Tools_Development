
#' Imputation of missing values.
#'
#' @param data A dataframe.
#' @param varlist A vector of numeric variables to be imputed.
#' @param type_num Type of imputation for numeric variables to be used. Mean is Default method.
#' @param factvarlist A vector of factor variables to be imputed.
#' @param type_fact Type of imputation for factor variables to be used. Add 'missing' level is Default method.
#' @return input dataset with imputed values.
#' @examples
#' data <- impute(data=data,numvarlist=c("salary","yrs.service"),type_num = "mean",factvarlist=c("rank"),type_fact = "mode")
#' data <- impute(data=data,numvarlist=c("salary","yrs.service"),type_num = "median",factvarlist=c("rank"),type_fact="missing")
#' 

impute <- function(data, numvarlist,type_num = "mean",factvarlist,type_fact = "missing") {
  

  if (!is.null(numvarlist)) {
  for(i in numvarlist ){
    data[[i]] <- as.numeric(data[[i]])
    summ <- list()
    if (tolower(type_num) == "mean") {
    summ[[i]] <- round(mean(data[[i]],na.rm=TRUE))
    data[[i]] <- ifelse(is.na(data[[i]]),summ[[i]],data[[i]])
    }
  
    else if (tolower(type_num) == "median") {
      summ[[i]] <- round(median(data[[i]],na.rm=TRUE))
      data[[i]] <- ifelse(is.na(data[[i]]),summ[[i]],data[[i]])
    }
    
    else if (tolower(type_num) == "mode") {
      summ[[i]] <- Mode(data[[i]],na.rm=TRUE)
      data[[i]] <- ifelse(is.na(data[[i]]),summ[[i]],data[[i]])
    }
    else if (tolower(type_num) == "max") {
      summ[[i]] <- max(data[[i]],na.rm=TRUE)
      data[[i]] <- ifelse(is.na(data[[i]]),summ[[i]],data[[i]])
    }
    else if (tolower(type_num) == "min") {
      summ[[i]] <- min(data[[i]],na.rm=TRUE)
      data[[i]] <- ifelse(is.na(data[[i]]),summ[[i]],data[[i]])
    }
    else stop("ERROR: Only minimum, maximum, mean, median and mode allowed for type of numeric variables imputation")
  }
  }
  
  if (!is.null(factvarlist)) {
    for(i in factvarlist ){
      
      summ <- list()
      if (tolower(type_fact) == "mode") {
        summ[[i]] <- Mode(levels(data[[i]])[data[[i]]],na.rm=TRUE)
        data[[i]] <- as.factor(ifelse(is.na(data[[i]]),summ[[i]],levels(data[[i]])[data[[i]]]))
      }
      else if (tolower(type_fact) == "missing") {
        data[[i]] <- as.factor(ifelse(is.na(data[[i]]),"Missing",levels(data[[i]])[data[[i]]]))
      }
      else stop("ERROR: Only 'mode' and 'missing' allowed for type of factor variables imputation")
    }
  }
  return(data)
}

#Test 
library(DescTools)
data = readRDS("/stats/projects/all/R_Tools_Development/data/salaries_data.Rds")
data[1,] <- c(NA,NA,NA,NA,NA,NA,0)
data$salary = as.integer(data$salary)
data_imputed <- impute(data=data,numvarlist=c("salary","yrs.service"),type_num = "mean",factvarlist=c("rank"),type_fact="missing")
str(data)



