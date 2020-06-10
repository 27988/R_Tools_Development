
#' Partition data into train, test and validation (optional) datasets.
#'
#' @param data A dataframe that needs to be split.
#' @param seed Random seed value.
#' @param uniqueid A key (variable) used to identify all records of the same entity (e.g.patient ID)
#' @param splitby A single column name or vector of column names that will be used to stratify data.
#' @param valid_req Takes "Y" or "N"; default is "N". "Y" if validation dataset should be produced.
#' @param trainprop Proportion that should be retained in train dataset. Value ranges between 0 and 1. Default is 0.70.
#' @param testprop Proportion that should be retained in test if validation dataset is also created. Value ranges between 0 and 1. Default is 0.50.
#' @return dataframes as train, test and/or validation.
#' @examples
#' split(data=data, seed=50, uniqueid = NULL, splitby = c("rank","discipline"), valid_req = "Y", trainprop=0.70, testprop= 0.50)
#' split(data=data, seed=50, uniqueid = NULL, splitby = c("discipline"), valid_req = "Y", trainprop=0.70, testprop= 0.50)
#' split(data=data, seed=50, uniqueid = NULL, uniqueid = NULL, splitby = c("rank","discipline"), valid_req = "N", trainprop=0.70, testprop= 0.50)
#' split(data=data, seed=50, splitby = NULL, valid_req = "N", trainprop=0.50, testprop= 0.50)
#' split(data=data, seed=50, uniqueid = "patid", splitby = c("rank"), valid_req = "N", trainprop=0.50, testprop= 0.50)
#' split(data=data, seed=50, uniqueid = "patid", splitby = c("rank"), valid_req = "Y", trainprop=0.50, testprop= 0.50)

split <- function(data, seed=50, uniqueid = NULL, splitby = NULL, valid_req = "N", trainprop=0.70, testprop= 0.50) {
  
  set.seed(seed)
  
  #If unique ID is not required
  if (is.null(uniqueid)) {
    #If validation dataset is not required
    if (valid_req == "N") {
      # If stratified sampling is required
      if (!is.null(splitby)){
        list_t <- NULL
        list_t <- data[[splitby[1]]]
        if (length(splitby) > 1) {
          for (i in 2: length(splitby)){
            list_t <- list(list_t,data[[splitby[i]]])
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
      # If stratified sampling is required
      if (!is.null(splitby)){
        list_t <- NULL
        list_t <- data[[splitby[1]]]
        if (length(splitby) > 1) {
          for (i in 2: length(splitby)){
            list_t <- list(list_t,data[[splitby[i]]])
          }
        }
        
        sp <- base::split(seq_len(nrow(data)), list_t)
        samples <- lapply(sp, function(x) sample(x, replace=F,size=trainprop*length(x)))
        train <- data[unlist(samples), ]
        test_tmp <- data[-unlist(samples),]
        
        list_t <- NULL
        list_t <- test_tmp[[splitby[1]]]
        if (length(splitby) > 1) {
          for (i in 2: length(splitby)){
            list_t <- list(list_t,test_tmp[[splitby[i]]])
          }
        }
        sp_v <- base::split(seq_len(nrow(test_tmp)), list_t)
        samples_v <- lapply(sp_v, function(x) sample(x, replace=F,size=testprop*length(x)))
        test <- test_tmp[unlist(samples_v), ]
        validation <- test_tmp[-unlist(samples_v),]
        
      } else {
        samples <- sample(1:dim(data)[1],replace=F,size=trainprop*dim(data)[1])
        train <- data[samples,]
        test_tmp <- data[-samples,]
        
        samples_v <- sample(1:dim(test_tmp)[1],replace=F,size=testprop*dim(test_tmp)[1])
        test <- test_tmp[samples_v,]
        validation <- test_tmp[-samples_v,]
      }
    }
  } else {
    unique_id <- unique(data[[uniqueid]])
    #If validation dataset is not required
    if (valid_req == "N") {
      # If stratified sampling is required
      if (!is.null(splitby)){
        list_t <- NULL
        list_t <- data[[splitby[1]]]
        if (length(splitby) > 1) {
          for (i in 2: length(splitby)){
            list_t <- list(list_t,data[[splitby[i]]])
          }
        }
        
        sp <- base::split(seq_len(nrow(data)), list_t)
        samples <- lapply(sp, function(x) sample(x, replace=F,size=trainprop*length(x)))
        samples <- unique_id[unlist(samples)]
        train <- data[data[[uniqueid]] %in% samples, ]
        test <- data[!(data[[uniqueid]] %in% samples),]
        
      } else {
        samples <- sample(1:dim(data)[1],replace=F,size=trainprop*dim(data)[1])
        samples <- unique_id[samples]
        train <- data[data[[uniqueid]] %in% samples,]
        test <- data[!(data[[uniqueid]] %in% samples),]
      }
    } else {
      # If stratified sampling is required
      if (!is.null(splitby)){
        list_t <- NULL
        list_t <- data[[splitby[1]]]
        if (length(splitby) > 1) {
          for (i in 2: length(splitby)){
            list_t <- list(list_t,data[[splitby[i]]])
          }
        }
        
        sp <- base::split(seq_len(nrow(data)), list_t)
        samples <- lapply(sp, function(x) sample(x, replace=F,size=trainprop*length(x)))
        samples <- unique_id[unlist(samples)]
        train <- data[data[[uniqueid]] %in% samples, ]
        test_tmp <- data[!(data[[uniqueid]] %in% samples),]
        
        unique_id_test <- unique(test_tmp[[uniqueid]])
        list_t <- NULL
        list_t <- test_tmp[[splitby[1]]]
        if (length(splitby) > 1) {
          for (i in 2: length(splitby)){
            list_t <- list(list_t,test_tmp[[splitby[i]]])
          }
        }
        sp_v <- base::split(seq_len(nrow(test_tmp)), list_t)
        samples_v <- lapply(sp_v, function(x) sample(x, replace=F,size=testprop*length(x)))
        samples_v <- unique_id_test[unlist(samples_v)]
        test <- test_tmp[test_tmp[[uniqueid]] %in% samples_v, ]
        validation <- test_tmp[!(test_tmp[[uniqueid]] %in% samples_v),]
        
      } else {
        samples <- sample(1:dim(data)[1],replace=F,size=trainprop*dim(data)[1])
        samples <- unique_id[samples]
        train <- data[data[[uniqueid]] %in% samples,]
        test_tmp <- data[!(data[[uniqueid]] %in% samples),]
        
        unique_id_test <- unique(test_tmp[[uniqueid]])
        samples_v <- sample(1:dim(test_tmp)[1],replace=F,size=testprop*dim(test_tmp)[1])
        samples_v <- unique_id_test[samples_v]
        test <- test_tmp[test_tmp[[uniqueid]] %in% samples_v,]
        validation <- test_tmp[!(test_tmp[[uniqueid]] %in% samples_v),]
      }
    }
  }
  
  split.data <- list()
  split.data$train <- train
  split.data$test <- test
  if (valid_req != "N") {
    split.data$validation <- validation
  }
  return(split.data)
}


#Test 
data = readRDS("/stats/projects/all/R_Tools_Development/data/salaries_data.Rds")

res = split(data=data, seed=50, uniqueid = NULL, splitby = c("rank","discipline"), valid_req = "Y", trainprop=0.70, testprop= 0.50)
res = split(data=data, seed=50, uniqueid = NULL, splitby = c("discipline"), valid_req = "Y", trainprop=0.70, testprop= 0.50)
res = split(data=data, seed=50, uniqueid = NULL, splitby = c("rank","discipline"), valid_req = "N", trainprop=0.70, testprop= 0.50)
res = split(data=data, seed=50, uniqueid = NULL, splitby = NULL, valid_req = "N", trainprop=0.50, testprop= 0.50)
res = split(data=data, seed=50, uniqueid = "patid", splitby = c("rank"), valid_req = "N", trainprop=0.50, testprop= 0.50) #  not working!
res = split(data=data, seed=50, uniqueid = "patid", splitby = c("rank"), valid_req = "Y", trainprop=0.50, testprop= 0.50) #  not working!

