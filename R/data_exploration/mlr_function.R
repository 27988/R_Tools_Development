#' MLR sub function for model building.
#'
#' @param df_train
#' @param df_test 
#' @param exclude_vars 
#' @param tasktype
#' @param target 
#' @param positive.class 
#' @param seed
#' @examples
#' 


mlr_function <- function(df_train,
                         df_test,
                         exclude_vars = NULL,
                         tasktype,
                         outcome,
                         positive.class = NULL,
                         seed = 100) {
 
  traintask1 <- df_train[,-which(colnames(df_train) %in% exclude_vars)]
  testtask1 <- df_test[,which(colnames(df_test) %in% colnames(traintask1))]
  
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
  if (tasktype == "Regression") {
  traintask <- makeRegrTask(data = traintask,target = target, fixup.data = "no",check.data = FALSE)
  testtask <- makeRegrTask(data = testtask,target = target, fixup.data = "no",check.data = FALSE)
  } else if (tasktype == "Classification") {
    
    if (is.null(positive.class)) {positive.class = levels(traintask[[target]])[1]}
    #create a task
    trainTask <- makeClassifTask(data = traintask,target = target,positive = positive.class)
    testTask <- makeClassifTask(data = testtask, target = target,positive = positive.class)
  } else stop("ERROR: Specify a valid tasktype: Regression or Classification")
  
  
  set.seed(seed)
  
  ###### Linear Regression Models ######
  
  # Define a search space for learner's parameter
  
  params <- makeParamSet( makeDiscreteParam("booster",values = c("gbtree","gblinear")), makeIntegerParam("max_depth",lower = 3L,upper = 10L), 
                          makeNumericParam("min_child_weight",lower = 1L,upper = 10L), makeNumericParam("subsample",lower = 0.5,upper = 1), 
                          makeNumericParam("colsample_bytree",lower = 0.5,upper = 1))
  
  
  # Choose a resampling strategy
  rdesc = makeResampleDesc("CV", iters = 5L)
  
  # Choose a performance measure
  meas = rmse
  
  # Choose a tuning method
  ctrl = makeTuneControlCMAES(budget = 100L)
  
  #parameter tuning
  mytune <- tuneParams(learner = lrn, task = traintask,  resampling = rdesc, measures = rmse, par.set = params, control = ctrl, show.info = T)
  
}






