

#' Imputation of missing values.
#'
#' @param df A dataframe.
#' @param exclude_vars A vector of variables to be excluded form imputation.
#' @param num_vars A vector of numeric variables to be imputed.
#' @param impute_type_num Type of imputation for numeric variables to be used. Mean is Default method. Choices are min, max, mean, median, mode, and zero.
#' @param round Up to which decimal should imputed numeric values be rounded. Default rounds to whole number.
#' @param factor_vars A vector of factor variables to be imputed.
#' @param impute_type_fact Type of imputation for factor variables to be used. Adding 'missing' level is default method. Choices are missing, mode, and unknown.
#' @param output_type Option to create new labeled columns or impute values in the original columns. Choices are detailed (default) and direct. 
#' @return Input dataset with imputed values.
#' @examples
#' data <- impute(df=data,num_vars=c("salary","yrs.service"),impute_type_num = "mean",round=0,factor_vars=c("rank"),impute_type_fact = "mode")
#' data <- impute(df=data,num_vars=c("salary","yrs.service"),impute_type_num = "median",round=2,factor_vars=c("rank"),impute_type_fact="missing")
#' 

impute <- function(df, exclude_vars=NULL,num_vars=NULL,impute_type_num = "mean",round=0,
                   factor_vars=NULL,impute_type_fact = "missing", output_type = "detailed") {
  
  var_type <- var_type_identify(df=df,exclude_vars=exclude_vars)
  #Identify numeric variables if is.null(num_vars)
  if (is.null(num_vars)) {
    num_vars <- var_type[[1]]
  }
  
  if (output_type == "detailed") {  
    if (!is.null(num_vars)) {
      for(i in num_vars ){
        df[[i]] <- as.numeric(df[[i]])
        summ <- list()
        if (tolower(impute_type_num) == "mean") {
          summ[[i]] <- round(mean(df[[i]],na.rm=TRUE),round)
          df[[paste0(i,"_imputed")]] <- ifelse(is.na(df[[i]]),summ[[i]],df[[i]])
          df[[paste0(i,"_impute_flag")]] <- ifelse(is.na(df[[i]]),"Yes","No")
        }
        
        else if (tolower(impute_type_num) == "median") {
          summ[[i]] <- round(median(df[[i]],na.rm=TRUE),round)
          df[[paste0(i,"_imputed")]] <- ifelse(is.na(df[[i]]),summ[[i]],df[[i]])
          df[[paste0(i,"_impute_flag")]] <- ifelse(is.na(df[[i]]),"Yes","No")
        }
        
        else if (tolower(impute_type_num) == "mode") {
          Mode <- function(x) {
            x <- x[!is.na(x)]
            ux <- unique(x)
            ux[which.max(tabulate(match(x, ux)))]
          }
          summ[[i]] <- Mode(df[[i]])
          df[[paste0(i,"_imputed")]] <- ifelse(is.na(df[[i]]),summ[[i]],df[[i]])
          df[[paste0(i,"_impute_flag")]] <- ifelse(is.na(df[[i]]),"Yes","No")
        }
        else if (tolower(impute_type_num) == "max") {
          summ[[i]] <- max(df[[i]],na.rm=TRUE)
          df[[paste0(i,"_imputed")]] <- ifelse(is.na(df[[i]]),summ[[i]],df[[i]])
          df[[paste0(i,"_impute_flag")]] <- ifelse(is.na(df[[i]]),"Yes","No")
        }
        else if (tolower(impute_type_num) == "min") {
          summ[[i]] <- min(df[[i]],na.rm=TRUE)
          df[[paste0(i,"_imputed")]] <- ifelse(is.na(df[[i]]),summ[[i]],df[[i]])
          df[[paste0(i,"_impute_flag")]] <- ifelse(is.na(df[[i]]),"Yes","No")
        }
        else if (tolower(impute_type_num) == "zero") {
          df[[paste0(i,"_imputed")]] <- ifelse(is.na(df[[i]]),0,df[[i]])
          df[[paste0(i,"_impute_flag")]] <- ifelse(is.na(df[[i]]),"Yes","No")
        }
        else stop("ERROR: Only min, max, mean, median, mode and zero allowed for type of numeric variables imputation")
      }
    }
    
    
    #Identify character/categorical variables if is.null(factor_vars)
    if (is.null(factor_vars)) {
      factor_vars <- var_type[[2]]
    }
    
    if (!is.null(factor_vars)) {
      for(i in factor_vars ){
        
        summ <- list()
        if (tolower(impute_type_fact) == "mode") {
          Mode <- function(x) {
            x <- x[!is.na(x)]
            ux <- unique(x)
            ux[which.max(tabulate(match(x, ux)))]
          }
          summ[[i]] <- Mode(levels(df[[i]])[df[[i]]])
          df[[paste0(i,"_imputed")]] <- as.factor(ifelse(is.na(df[[i]]),summ[[i]],levels(df[[i]])[df[[i]]]))
          df[[paste0(i,"_impute_flag")]] <- ifelse(is.na(df[[i]]),"Yes","No")
        }
        else if (tolower(impute_type_fact) == "missing") {
          df[[paste0(i,"_imputed")]] <- as.factor(ifelse(is.na(df[[i]]),"Missing",levels(df[[i]])[df[[i]]]))
          df[[paste0(i,"_impute_flag")]] <- ifelse(is.na(df[[i]]),"Yes","No")
        }
        else if (tolower(impute_type_fact) == "unknown") {
          df[[paste0(i,"_imputed")]] <- as.factor(ifelse(is.na(df[[i]]),"Unknown",levels(df[[i]])[df[[i]]]))
          df[[paste0(i,"_impute_flag")]] <- ifelse(is.na(df[[i]]),"Yes","No")
        }
        else stop("ERROR: Only 'missing', 'mode', and 'unknown' allowed for type of factor variables imputation")
      }
    }
  }
  
  else if (output_type == "direct") {
    
    if (!is.null(num_vars)) {
      for(i in num_vars ){
        df[[i]] <- as.numeric(df[[i]])
        summ <- list()
        if (tolower(impute_type_num) == "mean") {
          summ[[i]] <- round(mean(df[[i]],na.rm=TRUE),round)
          df[[i]] <- ifelse(is.na(df[[i]]),summ[[i]],df[[i]])
        }
        
        else if (tolower(impute_type_num) == "median") {
          summ[[i]] <- round(median(df[[i]],na.rm=TRUE),round)
          df[[i]] <- ifelse(is.na(df[[i]]),summ[[i]],df[[i]])
        }
        
        else if (tolower(impute_type_num) == "mode") {
          Mode <- function(x) {
            x <- x[!is.na(x)]
            ux <- unique(x)
            ux[which.max(tabulate(match(x, ux)))]
          }
          summ[[i]] <- Mode(df[[i]])
          df[[i]] <- ifelse(is.na(df[[i]]),summ[[i]],df[[i]])
        }
        else if (tolower(impute_type_num) == "max") {
          summ[[i]] <- max(df[[i]],na.rm=TRUE)
          df[[i]] <- ifelse(is.na(df[[i]]),summ[[i]],df[[i]])
        }
        else if (tolower(impute_type_num) == "min") {
          summ[[i]] <- min(df[[i]],na.rm=TRUE)
          df[[i]] <- ifelse(is.na(df[[i]]),summ[[i]],df[[i]])
        }
        else if (tolower(impute_type_num) == "zero") {
          df[[i]] <- ifelse(is.na(df[[i]]),0,df[[i]])
        }
        else stop("ERROR: Only min, max, mean, median, mode and zero allowed for type of numeric variables imputation")
      }
    }
    
    #Identify character/categorical variables if is.null(factor_vars)
    if (is.null(factor_vars)) {
      factor_vars <- var_type[[2]]
    }
    
    if (!is.null(factor_vars)) {
      for(i in factor_vars ){
        
        summ <- list()
        if (tolower(impute_type_fact) == "mode") {
          Mode <- function(x) {
            x <- x[!is.na(x)]
            ux <- unique(x)
            ux[which.max(tabulate(match(x, ux)))]
          }
          summ[[i]] <- Mode(levels(df[[i]])[df[[i]]])
          df[[i]] <- as.factor(ifelse(is.na(df[[i]]),summ[[i]],levels(df[[i]])[df[[i]]]))
        }
        else if (tolower(impute_type_fact) == "missing") {
          df[[i]] <- as.factor(ifelse(is.na(df[[i]]),"Missing",levels(df[[i]])[df[[i]]]))
        }
        else if (tolower(impute_type_fact) == "unknown") {
          df[[i]] <- as.factor(ifelse(is.na(df[[i]]),"Unknown",levels(df[[i]])[df[[i]]]))
        }
        else stop("ERROR: Only 'missing', 'mode', and 'unknown' allowed for type of factor variables imputation")
      }
    }
  }
  
  return(df)
}

#Test 
data = readRDS("/stats/projects/all/R_Tools_Development/data/salaries_data.Rds")
data$text_fct <- as.factor(c(rep(1,97),rep(20.2,150),rep(300.15,150)))
data[1,] <- c(NA,NA,NA,NA,NA,NA,0,NA)
data[2,] <- c(NA,NA,NA,NA,NA,NA,0,NA)
data[5,] <- c(NA,NA,NA,NA,NA,NA,0,NA)
data$salary = as.integer(data$salary)
data_imputed <- impute(df=data,num_vars=NULL,impute_type_num = "mode",round=2,factor_vars=NULL,impute_type_fact="mode")
data_imputed <- impute(df=data)



dt_original = data.frame("id"=c(1,NA,3,4,5),
                         "numcol"=c(20,29,32,NA,40), 
                         "factcol"=c("a","b",NA,"c","d"),
                         "zerocol"=c(6,NA,8,9,NA)
)

dt_impute_direct <- impute(df=dt_original, num_vars = "numcol", impute_type_num = "mean",
                           factor_vars = "factcol", impute_type_fact = "unknown",
                           exclude_vars = "id", output_type = "direct")

dt_impute_direct2 <- impute(df=dt_impute_direct, num_vars = "zerocol", impute_type_num = "zero",
                            exclude_vars = "id", output_type = "direct")



