
#' Partition data into train and test datasets.
#'
#' @param data A dataframe if filetype is dataframe or file path.
#' @param filetype Type of file read. Provide extension of file if not dataframe. Permissible filetypes are Rds, rds, csv, xls and xlsx.
#' @param seed Random seed value.
#' @param splitkey A key (variable) used to identify all records of the same entity (e.g.patient ID)
#' @param stratifyby A single column name or vector of column names that will be used to stratify data.
#' @param trainprop Proportion that should be retained in train dataset. Value ranges between 0 and 1. Default is 0.70.
#' @return dataframes as train and test.
#' @examples
#' res = split(data=data,filetype = "dataframe", seed=50, splitkey = NULL, stratifyby = c("rank","discipline"), trainprop=0.70)
#' res = split(data=data, filetype = "dataframe",seed=50, splitkey = NULL, stratifyby = c("discipline"), trainprop=0.70)
#' res = split(data="/stats/projects/all/R_Tools_Development/data/salaries_data.xlsx", filetype = "xlsx",seed=50, splitkey = NULL, stratifyby = c("discipline"), trainprop=0.70)
#' res = split(data="/stats/projects/all/R_Tools_Development/data/salaries_data.csv", filetype = "csv", seed=50, splitkey = NULL, stratifyby = NULL,  trainprop=0.50)
#' res = split(data=data, filetype = "dataframe",seed=50, splitkey = "patient_id", stratifyby = c("rank"),  trainprop=0.50) 
#' train <- res$train; test <- res$test


split <- function(data, filetype = "dataframe",seed=50, splitkey = NULL, stratifyby = NULL, trainprop=0.70) {
  
  set.seed(seed)
  
  if (tolower(filetype) == "dataframe") {data = data}
  else if (tolower(filetype) == "rds") {data = readRDS(data)}
  else if (tolower(filetype) == "xls") {data = read.xlsx(data, sheetName = 1, header = TRUE)}
  else if (tolower(filetype) == "xlsx") {data =  read.xlsx(data, sheetName = 1, header = TRUE)}
  else if (tolower(filetype) == "csv") {data = read.csv(data,header = TRUE, sep = ",")}
  else stop("ERROR: File type is not compatible")
  
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
  
  return(split.data)
}


#Test 
library(xlsx)
data = readRDS("/stats/projects/all/R_Tools_Development/data/salaries_data.Rds")

res = split(data=data,filetype = "dataframe", seed=50, splitkey = NULL, stratifyby = c("rank","discipline"), trainprop=0.70)
res = split(data="/stats/projects/all/R_Tools_Development/data/salaries_data.Rds", filetype = "Rds",seed=50, splitkey = NULL, stratifyby = c("discipline"), trainprop=0.70)
res = split(data=data, filetype = "dataframe",seed=50, splitkey = NULL, stratifyby = NULL,  trainprop=0.50)
res = split(data=data, filetype = "dataframe",seed=50, splitkey = "patient_id", stratifyby = c("rank"),  trainprop=0.50) 

id <- c(rep(1,10),rep(2,10),rep(3,10),rep(4,10),rep(5,10),rep(6,10),rep(7,30),8:314)
data$id <- id

res = split(data=data,filetype = "dataframe", seed=50, splitkey = "id", stratifyby = c("rank"),  trainprop=0.50) 
res = split(data="/stats/projects/all/R_Tools_Development/data/salaries_data.xlsx", filetype = "xlsx",seed=50, splitkey = NULL, stratifyby = c("discipline"), trainprop=0.70)
res = split(data=data, filetype = "Rdata", seed=50, splitkey = NULL, stratifyby = NULL,  trainprop=0.50)
