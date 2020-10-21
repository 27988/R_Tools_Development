
#' Partition data into train and test datasets.
#'
#' @param df A dataframe if filetype is dataframe or file path.
#' @param filetype_in Type of file read. Provide extension of file if not dataframe. Permissible filetypes are Rds, rds, csv, xls and xlsx.
#' @param seed Random seed value.
#' @param splitkey A key (variable) used to identify all records of the same entity (e.g.patient ID).
#' @param stratifyby A single column name or vector of column names that will be used to stratify data.
#' @param trainprop Proportion that should be retained in train dataset. Value ranges between 0 and 1. Default is 0.70.
#' @return Dataframes as train and test.
#' @examples
#' res = split(df=data,filetype_in = "dataframe", seed=50, splitkey = NULL, stratifyby = c("rank","discipline"), trainprop=0.70)
#' res = split(df=data, filetype_in = "dataframe",seed=50, splitkey = NULL, stratifyby = c("discipline"), trainprop=0.70)
#' res = split(df="/stats/projects/all/R_Tools_Development/data/salaries_data.xlsx", filetype_in = "xlsx",seed=50, splitkey = NULL, stratifyby = c("discipline"), trainprop=0.70)
#' res = split(df="/stats/projects/all/R_Tools_Development/data/salaries_data.csv", filetype_in = "csv", seed=50, splitkey = NULL, stratifyby = NULL,  trainprop=0.50)
#' res = split(df=data, filetype_in = "dataframe",seed=50, splitkey = "patient_id", stratifyby = c("rank"),  trainprop=0.50) 
#' train <- res$train; test <- res$test


split <- function(df, filetype_in = "dataframe",seed=50, splitkey = NULL, stratifyby = NULL, trainprop=0.70) {
  
  set.seed(seed)
  
  data <- filetype(df=df,filetype_in=filetype_in)
  
  #Check if splitkey has valid column name
  if (!is.null(splitkey)) { 
    if (!(splitkey %in% colnames(data))) {stop("ERROR: splitkey is not a valid column name")}}
  
  #Check if trainprop is valid
  if (trainprop > 1 | trainprop <0) {stop("ERROR: trainprop should be <= 1 and >= 0")}
  
  #If unique ID is not required
  if (is.null(splitkey)) {
      # If stratified sampling is required
      if (!is.null(stratifyby)){
        list_t <- NULL
        list_t <- data[[stratifyby[1]]]
        if (length(stratifyby) > 1) {
          for (i in 2: length(stratifyby)){
            list_t <- list(list_t,data[[stratifyby[i]]])
          }
        }
        
        sp <- base::split(seq_len(nrow(data)), list_t)
        samples <- lapply(sp, function(x) sample(x, replace=F,size=trainprop*length(x)))
        train <- data[unlist(samples), ]
        test <- data[-unlist(samples),]
        
      } else {
        samples <- sample(1:dim(data)[1],replace=F,size=trainprop*dim(data)[1])
        train <- data[samples,]
        test <- data[-samples,]
      }
     
  } else {
    unique_id <- unique(data[[splitkey]])
      # If stratified sampling is required
      if (!is.null(stratifyby)){
        list_t <- NULL
        list_t <- data[[stratifyby[1]]]
        if (length(stratifyby) > 1) {
          for (i in 2: length(stratifyby)){
            list_t <- list(list_t,data[[stratifyby[i]]])
          }
        }
        
        sp <- base::split(seq_len(nrow(data)), list_t)
        samples <- lapply(sp, function(x) sample(x, replace=F,size=trainprop*length(x)))
        samples <- unique_id[unlist(samples)]
        train <- data[data[[splitkey]] %in% samples, ]
        test <- data[!(data[[splitkey]] %in% samples),]
        
      } else {
        samples <- sample(1:dim(data)[1],replace=F,size=trainprop*dim(data)[1])
        samples <- unique_id[samples]
        train <- data[data[[splitkey]] %in% samples,]
        test <- data[!(data[[splitkey]] %in% samples),]
      }
     
  }
  
  split.data <- list()
  split.data$train <- train
  split.data$test <- test
  
  #Check if result is produced correctly
  if (dim(train)[1] == 0 | dim(test)[1] == 0) {
    stop("ERROR: One or a combination of the following issues have occurred:
                  1. stratifyby is a numeric variable or has too many levels
                  2. trainprop in combination with stratifyby is too small to produce a valid data cut")}
  
  return(split.data)
}


#Test 
library(xlsx)
data = readRDS("/stats/projects/all/R_Tools_Development/data/salaries_data.Rds")

res = split(df=data,filetype_in = "dataframe", seed=50, splitkey = NULL, stratifyby = c("rank","discipline"), trainprop=0.37)
res = split(df="/stats/projects/all/R_Tools_Development/data/salaries_data.Rds", filetype_in = "Rds",seed=50, splitkey = NULL, stratifyby = c("discipline"), trainprop=0.70)
res = split(df=data, filetype_in = "dataframe",seed=50, splitkey = NULL, stratifyby = NULL,  trainprop=0.50)
res = split(df=data, filetype_in = "dataframe",seed=50, splitkey = "patient_id", stratifyby = c("rank"),  trainprop=0.50) 

id <- c(rep(1,10),rep(2,10),rep(3,10),rep(4,10),rep(5,10),rep(6,10),rep(7,30),8:314)
data$id <- id

res = split(df=data,filetype_in = "dataframe", seed=50, splitkey = "id", stratifyby = c("rank"),  trainprop=0.49) 
res = split(df="/stats/projects/all/R_Tools_Development/data/salaries_data.xlsx", filetype_in = "xlsx",seed=50, splitkey = NULL, stratifyby = c("discipline"), trainprop=0.70)
res = split(df=data, filetype_in = "Rdata", seed=50, splitkey = NULL, stratifyby = NULL,  trainprop=0.50)

res = split(df=data,filetype_in = "dataframe", seed=50, splitkey = "atreyee", stratifyby = c("mpg_int"), trainprop=0.56)


