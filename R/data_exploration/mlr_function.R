#' MLR sub function for model building.
#'
#' @param df_train
#' @param df_test
#' @param model 
#' @param exclude_vars 
#' @param tasktype
#' @param target 
#' @param positive_class 
#' @param seed
#' @param full_rank
#' @examples
#' 


mlr_function <- function(df_train,
                         df_test,
                         model,
                         exclude_vars = NULL,
                         tasktype,
                         outcome,
                         normalize = T,
                         family_LR = "gaussian",
                         positive_class = NULL,
                         seed = 100,
                         full_rank = T,
                         tune = T,
                         alphaL = 0,
                         alphaU = 1,
                         nlambdaL = 1L,
                         nlambdaU = 100L,
                         lambdaL = 0,
                         lambdaU = 1,
                         threshL = 1e-07,
                         threshU = 0.1,
                         maxitL = 1L,
                         maxitU = 100L,
                         meas = "rmse",
                         gridsearch = "random",
                         search_maxit = 20L,
                         search_reso = 10L,
                         rdesc = "CV",
                         rdesc_iters = 5L,
                         rdesc_split = 2/3,
                         rdesc_folds = 10,
                         rdesc_reps = 10,
                         rdesc_stratify = FALSE,
                         rdesc_stratify_cols = NULL,
                         show_info = T) {
  
  
  if (tasktype == "Classification") {
    traintask1 <- df_train[,-which(colnames(df_train) %in% c(exclude_vars,outcome))]
  } else  if (tasktype == "Regression") {
    traintask1 <- df_train[,-which(colnames(df_train) %in% exclude_vars)]
  } else stop("ERROR: Tasktype should be either Regression or Classification")
  
  testtask1 <- df_test[,which(colnames(df_test) %in% colnames(traintask1))]
  
  # one hot encoding 
  library(caret)
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
  
  detach("package:caret", unload=TRUE)
  library(mlr)
  
  #create tasks
  if (tasktype == "Regression") {
    trainTask <- makeRegrTask(data = traintask,target = outcome)
    testTask <- makeRegrTask(data = testtask,target = outcome)
  } else if (tasktype == "Classification") {
    
    train_names <- colnames(traintask)
    test_names <- colnames(testtask)
    
    traintask <- data.frame(traintask,df_train[[outcome]])
    colnames(traintask) <- c(train_names,outcome)
    testtask <- data.frame(testtask,df_test[[outcome]])
    colnames(testtask) <- c(test_names,outcome)
    
    if (is.null(positive_class)) {positive_class = levels(traintask[[outcome]])[1]}
    #create a task
    if (length(levels(traintask[[outcome]])) == 2) {
      trainTask <- makeClassifTask(data = traintask,target = outcome,positive = positive_class)
      testTask <- makeClassifTask(data = testtask, target = outcome,positive = positive_class)
    } else if (length(levels(traintask[[outcome]])) > 2) {
      trainTask <- makeClassifTask(data = traintask,target = outcome)
      testTask <- makeClassifTask(data = testtask, target = outcome)
    } else stop("ERROR: Outcome variables has less that 2 levels")
  } else stop("ERROR: Specify a valid tasktype: Regression or Classification")
  
  
  if (normalize == T) {
    #normalize the variables
    trainTask <- normalizeFeatures(trainTask,method = "standardize")
    testTask <- normalizeFeatures(testTask,method = "standardize")
  }
  
  set.seed(seed)
  
  # Define a search space for learner's parameter
  model_choice <- function(model) {
    if (tolower(model) == "continuous regression") {
      lrn <- makeLearner("regr.glmnet",predict.type = "response")
      
      params <- makeParamSet(makeDiscreteParam(id = "family", value = family_LR),
                             makeNumericParam(id = "alpha",  lower = alphaL, upper = alphaU),
                             makeIntegerParam(id = "nlambda",  lower = nlambdaL, upper = nlambdaU),
                             makeNumericParam(id = "lambda", lower = lambdaL, upper = lambdaU),
                             makeNumericParam(id = "thresh", lower = threshL, upper = threshU),
                             makeIntegerParam(id = "maxit", lower = maxitL,upper = maxitU)
      )
      
      # Choose a performance measure
      if (tolower(meas) == "rmse"){
        meas = rmse
      } else if (tolower(meas) == "mse"){
        meas = mse
      } else if (tolower(meas) == "sse"){
        meas = sse
      } else if (tolower(meas) == "mae"){
        meas = mae
      } else if (tolower(meas) == "medse"){
        meas = medse
      }
      
    } else if (tolower(model) == "categorical regression") {
      
      lrn <- makeLearner("classif.glmnet",predict.type = "prob")
      
      params <- makeParamSet(
        makeNumericParam(id = "alpha",  lower = alphaL, upper = alphaU),
        makeIntegerParam(id = "nlambda",  lower = nlambdaL, upper = nlambdaU),
        makeNumericParam(id = "lambda", lower = lambdaL, upper = lambdaU),
        makeNumericParam(id = "thresh", lower = threshL, upper = threshU),
        makeIntegerParam(id = "maxit", lower = maxitL,upper = maxitU)
      )
      
      # Choose a performance measure
      if (tolower(meas) == "acc"){
        meas = acc
      } else if (tolower(meas) == "auc"){
        meas = auc
      } else if (tolower(meas) == "f1"){
        meas = f1
      } else if (tolower(meas) == "fdr"){
        meas = fdr
      } else if (tolower(meas) == "fn"){
        meas = fn
      } else if (tolower(meas) == "fp"){
        meas = fp
      } else if (tolower(meas) == "fpr"){
        meas = fpr
      } else if (tolower(meas) == "mmce"){
        meas = mmce
      } else if (tolower(meas) == "ppv"){
        meas = ppv
      } else if (tolower(meas) == "tn"){
        meas = tn
      } else if (tolower(meas) == "tnr"){
        meas = tnr
      } else if (tolower(meas) == "tp"){
        meas = tp
      } else if (tolower(meas) == "tpr"){
        meas = tpr
      } 
      
    } #end of logistic
    
    
    # Choose a tuning method
    if (tolower(gridsearch) == "random") {
      ctrl = makeTuneControlRandom(maxit = search_maxit)
    } else if (tolower(gridsearch) == "complete") {
      ctrl = makeTuneControlGrid(resolution = search_reso)
    }
    
    # Choose a resampling strategy
    if (rdesc_stratify == FALSE) {
      if (tolower(rdesc) == "cv") {
        rdesc = makeResampleDesc("CV", iters = rdesc_iters)
      } else if (tolower(rdesc) == "holdout") {
        rdesc = makeResampleDesc("Holdout", split = rdesc_split)
      } else if (tolower(rdesc) == "subsample") {
        rdesc = makeResampleDesc("Subsample", iters = rdesc_iters, split = rdesc_split)
      } else if (tolower(rdesc) == "loo") {
        rdesc = makeResampleDesc("LOO")
      } else if (tolower(rdesc) == "bootstrap") {
        rdesc = makeResampleDesc("Bootstrap", iters = rdesc_iters)
      } else if (tolower(rdesc) == "repcv") {
        rdesc = makeResampleDesc("RepCV", folds = rdesc_folds , reps = rdesc_reps) 
      } 
    } else {
      if (tolower(rdesc) == "cv") {
        rdesc = makeResampleDesc("CV", iters = rdesc_iters, stratify = TRUE, stratify.cols =  rdesc_stratify_cols)
      } else if (tolower(rdesc) == "holdout") {
        rdesc = makeResampleDesc("Holdout", split = rdesc_split, stratify = TRUE, stratify.cols =  rdesc_stratify_cols)
      } else if (tolower(rdesc) == "subsample") {
        rdesc = makeResampleDesc("Subsample", iters = rdesc_iters, split = rdesc_split, stratify = TRUE, stratify.cols =  rdesc_stratify_cols)
      } else if (tolower(rdesc) == "loo") {
        rdesc = makeResampleDesc("LOO")
      } else if (tolower(rdesc) == "bootstrap") {
        rdesc = makeResampleDesc("Bootstrap", iters = rdesc_iters, stratify = TRUE, stratify.cols =  rdesc_stratify_cols)
      } else if (tolower(rdesc) == "repcv") {
        rdesc = makeResampleDesc("RepCV", folds = rdesc_folds , reps = rdesc_reps, stratify = TRUE, stratify.cols =  rdesc_stratify_cols) 
      } 
    }
    
    
    #parameter tuning
    mytune <- tuneParams(learner = lrn, task = trainTask,  resampling = rdesc, measures = meas, par.set = params, control = ctrl, show.info = show_info)
    
    #set hyperparameters
    lrn_tune <- setHyperPars(lrn,par.vals = mytune$x)
    
    #train model
    trained_model <- train(learner = lrn_tune,task = trainTask)
    
    return(list(mytune$x,mytune$y,trained_model))
  } #end of model_choice
  
  
  #### Model should be tuned or not
  if (tolower(model) == "continuous regression") {
    if (tune == T) {
      model_exec = model_choice(model = "continuous regression")
      trained_model = model_exec[[3]]
    } else {
      lrn <- makeLearner("regr.cvglmnet",predict.type = "response")
      trained_model = train(lrn, task = trainTask)
    }
  } else if (tolower(model) == "categorical regression") {
    if (tune == T) {
      model_exec = model_choice(model = "categorical regression")
      trained_model = model_exec[[3]]
    } else {
      lrn <- makeLearner("classif.cvglmnet",predict.type = "prob")
      trained_model = train(lrn, task = trainTask)
    }
  }
  
  
  #predict model
  test_pred <- predict(trained_model,testTask)
  test_pred <- test_pred[[2]]
  if (tasktype == "Regression") {
    test_pred <- data.frame(ID = df_test[[exclude_vars]],test_pred[,-which(colnames(test_pred) %in% c("id"))])
  } else if (tasktype == "Classification"){
    test_pred <- data.frame(ID = df_test[[exclude_vars]],test_pred[,-which(colnames(test_pred) %in% c("id","response"))])
  }
  
  train_pred <- predict(trained_model,trainTask)
  train_pred <- train_pred[[2]]
  if (tasktype == "Regression") {
    train_pred <- data.frame(ID = df_train[[exclude_vars]],train_pred[,-which(colnames(train_pred) %in% c("id"))])
  } else if (tasktype == "Classification"){
    train_pred <- data.frame(ID = df_train[[exclude_vars]],train_pred[,-which(colnames(train_pred) %in% c("id","response"))])
  }
  
  if (length(levels(traintask[[outcome]])) == 2 | is.numeric(traintask[[outcome]])) {
    coef = coef(getLearnerModel(trained_model,more.unwrap = T))
    features = coef[,1]
  } else {features <- list()
  for (i in 1:(length(coef(getLearnerModel(trained_model,more.unwrap = T)))))
  {coef = coef(getLearnerModel(trained_model,more.unwrap = T))[i]
  features[[i]] = coef[[1]][,1]}
  }
  
  if (tune == T) {
    return(list(test_pred,train_pred,model_exec[[1]],model_exec[[2]],features))
  } else {return(list(test_pred,train_pred,features))}
}



#Test
data = readRDS("/stats/projects/all/R_Tools_Development/data/salaries_data.Rds")

res = split(df=data,filetype_in = "dataframe", seed=50, splitkey = "patient_id", trainprop=0.7)
train = res[[1]]
test = res[[2]]



test_res <- mlr_function(df_train=train,
                         df_test=test,
                         model="categorical regression",
                         exclude_vars = "patient_id",
                         tasktype="Classification",
                         outcome="rank",
                         normalize = T,
                         family_LR = "gaussian",
                         positive_class = NULL,
                         seed = 100,
                         full_rank = T,
                         tune = T,
                         alphaL = 0,
                         alphaU = 1,
                         nlambdaL = 1L,
                         nlambdaU = 100L,
                         lambdaL = 0,
                         lambdaU = 0.05,
                         threshL = 1e-07,
                         threshU = 1,
                         maxitL = 10L,
                         maxitU = 100L,
                         meas = "acc",
                         gridsearch = "random",
                         search_maxit = 20L,
                         search_reso = 10L,
                         rdesc = "CV",
                         rdesc_iters = 5L,
                         rdesc_split = 2/3,
                         rdesc_folds = 10,
                         rdesc_reps = 10,
                         rdesc_stratify = FALSE,
                         rdesc_stratify_cols = NULL,
                         show_info = T)
