#' MLR sub function for model building.
#'
#' @param train
#' @param test 
#' @param excludeVars 
#' @param tasktype
#' @param target 
#' @param positive.class 
#' @examples
#' 


mlr_function <- function(train,
                         test,
                         excludeVars = NULL,
                         tasktype,
                         outcome,
                         positive.class = NULL) {
 
  traintask1 <- train[,-which(colnames(train) %in% excludeVars)]
  testtask1 <- test[,which(colnames(test) %in% colnames(traintask1))]
  
  # one hot encoding 
  library(caret)
  dummy.train <- caret::dummyVars("~ .", data = traintask1, fullRank = T) 
  dummy.test <- caret::dummyVars("~ .", data = testtask1, fullRank = T) 
  
  traintask = data.frame(predict(dummy.train, newdata = traintask1))
  testtask = data.frame(predict(dummy.test, newdata = testtask1))
  
  testtask <- testtask[,which(colnames(testtask) %in% colnames(traintask))]
  
  detach("package:caret", unload=TRUE)
  library(mlr)
  
  #create tasks
  if (tasktype == "Regresion") {
  traintask <- makeRegrTask(data = traintask,target = target, fixup.data = "no",check.data = FALSE)
  testtask <- makeRegrTask(data = testtask,target = target, fixup.data = "no",check.data = FALSE)
  } else if (tasktype == "Classification") {
    
    if (is.null(positive.class)) {positive.class = levels(traintask[[target]])[1]}
    #create a task
    trainTask <- makeClassifTask(data = traintask,target = target,positive = positive.class)
    testTask <- makeClassifTask(data = testtask, target = target,positive = positive.class)
  }
}






