
#' Imputation of missing values.
#'
#' @param data A dataframe.
#' @param excludeVars A vector of variables to be excluded form imputation.
#' @param numvarlist A vector of numeric variables to be imputed.
#' @param type_num Type of imputation for numeric variables to be used. Mean is Default method. Choices are min, max, mean, median and mode.
#' @param round Upto which decimal should imputed numeric values be rounded. Default rounds to whole number.
#' @param factvarlist A vector of factor variables to be imputed.
#' @param type_fact Type of imputation for factor variables to be used. Adding 'missing' level is Default method. Choices are mode and missing.
#' @return Input dataset with imputed values.
#' @examples
#' data <- impute(data=data,numvarlist=c("salary","yrs.service"),type_num = "mean",round=0,factvarlist=c("rank"),type_fact = "mode")
#' data <- impute(data=data,numvarlist=c("salary","yrs.service"),type_num = "median",round=2,factvarlist=c("rank"),type_fact="missing")
#' 

impute <- function(data, excludeVars=NULL,numvarlist=NULL,type_num = "mean",round=0,factvarlist=NULL,type_fact = "missing") {
  
  var_type <- var_type_identify(data=data,excludeVars=excludeVars)
  #Identify numeric variables if is.null(numvarlist)
  if (is.null(numvarlist)) {
    numvarlist <- var_type[[1]]
  }
  
  if (!is.null(numvarlist)) {
  for(i in numvarlist ){
    data[[i]] <- as.numeric(data[[i]])
    summ <- list()
    if (tolower(type_num) == "mean") {
    summ[[i]] <- round(mean(data[[i]],na.rm=TRUE),round)
    data[[paste0(i,"_imputed")]] <- ifelse(is.na(data[[i]]),summ[[i]],data[[i]])
    data[[paste0(i,"_impute_flag")]] <- ifelse(is.na(data[[i]]),"Yes","No")
    }
  
    else if (tolower(type_num) == "median") {
      summ[[i]] <- round(median(data[[i]],na.rm=TRUE),round)
      data[[paste0(i,"_imputed")]] <- ifelse(is.na(data[[i]]),summ[[i]],data[[i]])
      data[[paste0(i,"_impute_flag")]] <- ifelse(is.na(data[[i]]),"Yes","No")
    }
    
    else if (tolower(type_num) == "mode") {
      Mode <- function(x) {
        x <- x[!is.na(x)]
        ux <- unique(x)
        ux[which.max(tabulate(match(x, ux)))]
      }
      summ[[i]] <- Mode(data[[i]])
      data[[paste0(i,"_imputed")]] <- ifelse(is.na(data[[i]]),summ[[i]],data[[i]])
      data[[paste0(i,"_impute_flag")]] <- ifelse(is.na(data[[i]]),"Yes","No")
    }
    else if (tolower(type_num) == "max") {
      summ[[i]] <- max(data[[i]],na.rm=TRUE)
      data[[paste0(i,"_imputed")]] <- ifelse(is.na(data[[i]]),summ[[i]],data[[i]])
      data[[paste0(i,"_impute_flag")]] <- ifelse(is.na(data[[i]]),"Yes","No")
    }
    else if (tolower(type_num) == "min") {
      summ[[i]] <- min(data[[i]],na.rm=TRUE)
      data[[paste0(i,"_imputed")]] <- ifelse(is.na(data[[i]]),summ[[i]],data[[i]])
      data[[paste0(i,"_impute_flag")]] <- ifelse(is.na(data[[i]]),"Yes","No")
    }
    else stop("ERROR: Only min, max, mean, median and mode allowed for type of numeric variables imputation")
  }
  }
  
  #Identify character/categorical variables if is.null(factvarlist)
  if (is.null(factvarlist)) {
    factvarlist <- var_type[[2]]
  }
  
  if (!is.null(factvarlist)) {
    for(i in factvarlist ){
      
      summ <- list()
      if (tolower(type_fact) == "mode") {
        Mode <- function(x) {
          x <- x[!is.na(x)]
          ux <- unique(x)
          ux[which.max(tabulate(match(x, ux)))]
        }
        summ[[i]] <- Mode(levels(data[[i]])[data[[i]]])
        data[[paste0(i,"_imputed")]] <- as.factor(ifelse(is.na(data[[i]]),summ[[i]],levels(data[[i]])[data[[i]]]))
        data[[paste0(i,"_impute_flag")]] <- ifelse(is.na(data[[i]]),"Yes","No")
      }
      else if (tolower(type_fact) == "missing") {
        data[[paste0(i,"_imputed")]] <- as.factor(ifelse(is.na(data[[i]]),"Missing",levels(data[[i]])[data[[i]]]))
        data[[paste0(i,"_impute_flag")]] <- ifelse(is.na(data[[i]]),"Yes","No")
      }
      else stop("ERROR: Only 'mode' and 'missing' allowed for type of factor variables imputation")
    }
  }
  return(data)
}

#Test 

data = readRDS("/stats/projects/all/R_Tools_Development/data/salaries_data.Rds")
data$text_fct <- as.factor(c(rep(1,97),rep(20.2,150),rep(300.15,150)))
data[1,] <- c(NA,NA,NA,NA,NA,NA,0,NA)
data[2,] <- c(NA,NA,NA,NA,NA,NA,0,NA)
data[5,] <- c(NA,NA,NA,NA,NA,NA,0,NA)
data$salary = as.integer(data$salary)
data_imputed <- impute(data=data,numvarlist=NULL,type_num = "mode",round=2,factvarlist=NULL,type_fact="mode")
data_imputed <- impute(data=data)



