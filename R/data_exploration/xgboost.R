#' xgboost function for model building.
#'
#' @param df_train A dataframe for training the model
#' 
#' @examples
#' 


xgb_function <- function(df_train,
                         df_test,
                         exclude_vars = NULL,
                         tasktype,
                         outcome,
                         objective,
                         normalize = F,
                         normalize_type = "standardize",
                         weights = NULL,){
  
  
  library(xgboost)
  library(readr)
  library(stringr)
  library(caret)
  library(car)
  
  if (!is.numeric(df_train[[outcome]]) & !is.factor(df_train[[outcome]])) {stop("ERROR: Outcome variable should be either numeric or factor")}
  
  #Regression outcome should always be numeric
  if (tasktype == "Regression") {
    df_train[[outcome]] <- ifelse(is.numeric(df_train[[outcome]] ),df_train[[outcome]] ,as.numeric(levels(df_train[[outcome]]))[df_train[[outcome]]] )
    df_test[[outcome]] <- ifelse(is.numeric(df_test[[outcome]] ),df_test[[outcome]] ,as.numeric(levels(df_train[[outcome]]))[df_train[[outcome]]] )
  }
  
  if (tasktype == "Classification") {
    traintask1 <- df_train[,-which(colnames(df_train) %in% c(exclude_vars,outcome))]
  } else  if (tasktype == "Regression") {
    traintask1 <- df_train[,-which(colnames(df_train) %in% exclude_vars)]
  } else stop("ERROR: Tasktype should be either Regression or Classification")
  
  testtask1 <- df_test[,which(colnames(df_test) %in% colnames(traintask1))]
  
  # one hot encoding 
 
  if (full_rank == T) {
    dummy.train <- caret::dummyVars("~ .", data = traintask1, fullRank = T) 
    dummy.test <- caret::dummyVars("~ .", data = testtask1, fullRank = T) 
    
    traintask = data.frame(predict(dummy.train, newdata = traintask1))
    testtask = data.frame(predict(dummy.test, newdata = testtask1))
  } else {
    dummy.train <- caret::dummyVars("~ .", data = traintask1, fullRank = F) 
    dummy.test <- caret::dummyVars("~ .", data = testtask1, fullRank = F) 
    
    traintask = data.frame(predict(dummy.train, newdata = traintask1))
    testtask = data.frame(predict(dummy.test, newdata = testtask1))
  }
  
  
  testtask <- testtask[,which(colnames(testtask) %in% colnames(traintask))]
  
  #Separate out predictor and outcome
  traintask_x <- traintask[,-which(colnames(traintask) %in% traintask[[outcome]])]
  traintask_y <-  traintask[[outcome]]
  
  testtask_x <- testtask[,-which(colnames(testtask) %in% testtask[[outcome]])]
  testtask_y <-  testtask[[outcome]]
  
  
  #Normalize numeric variables
  mean_normalize <- list()
  sd_normalize <- list()
  min_normalize <- list()
  max_normalize <- list()
  if (normalize == T) {
    mean_normalize <- lapply(traintask, function(x) mean(x,na.rm=T))
    sd_normalize <- lapply(traintask, function(x) sd(x,na.rm=T))
    min_normalize <- lapply(traintask, function(x) min(x,na.rm=T))
    max_normalize <- lapply(traintask, function(x) max(x,na.rm=T))
    
    if (normalize_type == "standardize") {
      traintask <-  (traintask - mean_normalize)/sd_normalize
      testtask <-  (testtask - mean_normalize)/sd_normalize
    } else  if (normalize_type == "center") {
      traintask <-  traintask - mean_normalize
      testtask <-  testtask - mean_normalize
    } else  if (normalize_type == "scale") {
      traintask <-  traintask/sd_normalize
      testtask <-  testtask/sd_normalize
    } else  if (normalize_type == "range") {
      traintask <-  (traintask - min_normalize)/(max_normalize - min_normalize)
      testtask <-  (testtask - min_normalize)/(max_normalize - min_normalize)
    }
  }
  
  #positive class of classification task
  if (tasktype == "Classification"){
    message(paste0("MESSAGE: Outcome Variable has ",length(levels(traintask_y)), " levels"))
    if (length(levels(traintask_y)) < 2) {stop("ERROR: Outcome variables has less than 2 levels")
    } else {
    if (is.null(positive_class)) {positive_class = levels(traintask_y)[1]}
    } 
  } 

  #Train xgboost model
  xgb <- xgboost(data = traintask, 
                 label = , 
                 eta = 0.1,
                 max_depth = 15, 
                 nround=25, 
                 subsample = 0.5,
                 colsample_bytree = 0.5,
                 seed = 1,
                 eval_metric = "merror",
                 objective = "multi:softprob",
                 num_class = 12,
                 nthread = 3,
                 silent = show_info
  )
  
  # predict values in test set
  y_pred <- predict(xgb, data.matrix(X_test[,-1]))
  
} #end of xgb_function