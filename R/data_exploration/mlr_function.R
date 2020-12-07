#' MLR sub function for model building.
#'
#' @param df_train A dataframe for training the model
#' @param df_test A dataframe to be used for test/ validation
#' @param model Type of model. Permissible choices are 'glmnet', 'random forest' and 'decision tree'
#' @param exclude_vars List of variables to be excluded from the datasets for model building
#' @param tasktype Type of model task. Can be 'Regression' or 'Classification. 
#' @param outcome Target variable (y in the model formula)
#' @param normalize Should the data be normalized? Logical value. Default is F. Normailizing data with this input will produce results in the normalized scale.
#' @param normalize_type Type of normalization to be used. Default is 'standardize'. Can be 'standardize', 'center', 'scale' and 'range'
#' @param weights Weights of the model. Default is NULL
#' @param family_LR Type of linear Regreesion family if choice is regression glmnet. Default is 'gaussian'
#' @param positive_class Positive Class for a binary outcome. 
#' @param seed Set seed for randomization
#' @param full_rank Should one hot encoding matrix be full rank? Default is T
#' @param tune Should hyperparameters be tuned? Default is T 
#' @param alphaL Glmnet hyperparameter: Lower limit for elastic net mixing parameter
#' @param alphaU Glmnet hyperparameter: Upper limit for elastic net mixing parameter
#' @param nlambdaL Glmnet hyperparameter: Lower limit for number of lambda (penalty parameter)
#' @param nlambdaU Glmnet hyperparameter: Upper limit for number of lambda (penalty parameter)
#' @param lambdaL  Glmnet hyperparameter: Lower limit for lambda (penalty parameter)
#' @param lambdaU Glmnet hyperparameter: Upper limit for lambda (penalty parameter)
#' @param threshL Glmnet hyperparameter: Lower limit for convergence threshold for coordinate descent.
#' @param threshU Glmnet hyperparameter: Lower limit for convergence threshold for coordinate descent.
#' @param maxitL Glmnet hyperparameter: Lower limit for maximum number of passes over the data for all lambda values
#' @param maxitU Glmnet hyperparameter: Upper limit for maximum number of passes over the data for all lambda values
#' @param mtryL Random Forest hyperparameter: Lower limit for number of variables randomly sampled as candidates at each split
#' @param mtryU Random Forest hyperparameter: Upper limit for number of variables randomly sampled as candidates at each split
#' @param nodesizeL Random Forest hyperparameter: Lower limit for minimum size of terminal nodes
#' @param nodesizeU Random Forest hyperparameter: Upper limit for minimum size of terminal nodes
#' @param ntreeL Random Forest hyperparameter: Lower limit for number of trees to grow
#' @param ntreeU Random Forest hyperparameter: Upper limit for number of trees to grow
#' @param maxnodesL Random Forest hyperparameter: Lower limit for maximum number of terminal nodes trees in the forest can have
#' @param maxnodesU Random Forest hyperparameter: Upper limit for maximum number of terminal nodes trees in the forest can have
#' @param minsplitL Decision Tree hyperparameter: Lower limit for minimum number of observations that must exist in a node in order for a split to be attempted
#' @param minsplitU Decision Tree hyperparameter: Upper limit for minimum number of observations that must exist in a node in order for a split to be attempted
#' @param minbucketL Decision Tree hyperparameter: Lower limit for e minimum number of observations in any terminal <leaf> node
#' @param minbucketU Decision Tree hyperparameter: Upper limit for e minimum number of observations in any terminal <leaf> node
#' @param cpL Decision Tree hyperparameter: Lower limit for complexity parameter
#' @param cpU Decision Tree hyperparameter: Upper limit for complexity parameter
#' @param maxdepthL Decision Tree hyperparameter: Lower limit for maximum depth of any node of the final tree
#' @param maxdepthU Decision Tree hyperparameter: Upper limit for maximum depth of any node of the final tree
#' @param meas Metric to be used tuning the model. Sets of measures are different for Regression and Classification tasktypes
#' @param gridsearch Type of grid search for tuning. Default is 'random' 
#' @param search_maxit Number of CV iterations for random grid search
#' @param search_reso Number of steps in complete grid search
#' @param rdesc Resampling strategy; default is CV
#' @param rdesc_iters Number of iterations for CV, Subsample or bootstrap resampling
#' @param rdesc_split Number of splits for Holdout or Subsample resampling
#' @param rdesc_folds Number of folds for RepCV resampling
#' @param rdesc_reps Number of repetitions for RepCV resampling
#' @param rdesc_stratify Should stratified sampling be used? Logical value; default is FALSE
#' @param rdesc_stratify_cols Columns for stratified resampling
#' @param show_info Show tuning log; default is T
#' @examples
#' test_res <- mlr_function(df_train=train,
#' df_test=test,
#' model="random forest",
#' exclude_vars = "patient_id",
#' tasktype="Regression",
#' outcome="salary",
#' normalize = T,
#' normalize_type = "scale",
#' weights = NULL,
#' family_LR = "gaussian",
#' positive_class = NULL,
#' seed = 100,
#' full_rank = T,
#' tune = T,
#' alphaL = 0,
#' alphaU = 1,
#' nlambdaL = 1L,
#' nlambdaU = 100L,
#' lambdaL = 0,
#' lambdaU = 0.05,
#' threshL = 1e-07,
#' threshU = 1,
#' maxitL = 10L,
#' maxitU = 100L,
#' mtryL = 1L,
#' mtryU = 4L,
#' nodesizeL = 1L,
#' nodesizeU = 2L,
#' ntreeL = 1L,
#' ntreeU = 4L,
#' maxnodesL = 2L,
#' maxnodesU = 4L,
#' minsplitL = 1L,
#' minsplitU = 2L,
#' minbucketL = 1L,
#' minbucketU = 2L,
#' cpL = 0.01,
#' cpU = 0.1,
#' maxdepthL = 1L,
#' maxdepthU = 2L,
#' meas = "mse",
#' gridsearch = "random",
#' search_maxit = 2L,
#' search_reso = 10L,
#' rdesc = "CV",
#' rdesc_iters = 5L,
#' rdesc_split = 2/3,
#' rdesc_folds = 10,
#' rdesc_reps = 10,
#' rdesc_stratify = FALSE,
#' rdesc_stratify_cols = NULL,
#' show_info = T)


mlr_function <- function(df_train,
                         df_test,
                         model,
                         exclude_vars = NULL,
                         tasktype,
                         outcome,
                         normalize = F,
                         normalize_type = "standardize",
                         weights = NULL,
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
                         mtryL = 1L,
                         mtryU = 4L,
                         nodesizeL = 1L,
                         nodesizeU = 2L,
                         ntreeL = 1L,
                         ntreeU = 4L,
                         maxnodesL = 2L,
                         maxnodesU = 4L,
                         minsplitL = 1L,
                         minsplitU = 2L,
                         minbucketL = 1L,
                         minbucketU = 2L,
                         cpL = 0.01,
                         cpU = 0.1,
                         maxdepthL = 1L,
                         maxdepthU = 2L,
                         meas = "rmse",
                         gridsearch = "random",
                         search_maxit = 2L,
                         search_reso = 10L,
                         rdesc = "CV",
                         rdesc_iters = 5L,
                         rdesc_split = 2/3,
                         rdesc_folds = 10,
                         rdesc_reps = 10,
                         rdesc_stratify = FALSE,
                         rdesc_stratify_cols = NULL,
                         show_info = T) {
  
  if (!is.numeric(df_train[[outcome]]) & !is.factor(df_train[[outcome]])) {stop("ERROR: Outcome variable should be either numeric or factor")}
  if (!(model %in% c('glmnet', 'random forest', 'decision tree'))) {stop("ERROR: Permissible model choices are 'glmnet', 'random forest' and 'decision tree'")}
  
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
    
    message(paste0("MESSAGE: Outcome Variable has ",length(levels(traintask[[outcome]])), " levels"))
    if (is.null(positive_class)) {positive_class = levels(traintask[[outcome]])[1]}
    #create a task
    if (length(levels(traintask[[outcome]])) == 2) {
      trainTask <- makeClassifTask(data = traintask,target = outcome,positive = positive_class)
      testTask <- makeClassifTask(data = testtask, target = outcome,positive = positive_class)
    } else if (length(levels(traintask[[outcome]])) > 2) {
      trainTask <- makeClassifTask(data = traintask,target = outcome)
      testTask <- makeClassifTask(data = testtask, target = outcome)
    } else stop("ERROR: Outcome variables has less than 2 levels")
  } else stop("ERROR: Specify a valid tasktype: Regression or Classification")
  
  
  set.seed(seed)
  
  # Define a search space for learner's parameter
  model_choice <- function(model) {
    
    if (tasktype == "Regression") {
      
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
    } else if (tasktype == "Classification") {
        
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
      }
    
    if (tasktype == "Regression") {
      
      if (tolower(model) == "glmnet") {
        lrn <- makeLearner("regr.glmnet",predict.type = "response")
        
        params <- makeParamSet(makeDiscreteParam(id = "family", value = family_LR),
                               makeNumericParam(id = "alpha",  lower = alphaL, upper = alphaU),
                               makeIntegerParam(id = "nlambda",  lower = nlambdaL, upper = nlambdaU),
                               makeNumericParam(id = "lambda", lower = lambdaL, upper = lambdaU),
                               makeNumericParam(id = "thresh", lower = threshL, upper = threshU),
                               makeIntegerParam(id = "maxit", lower = maxitL,upper = maxitU)
        )
        
        
      } else if (tolower(model) == "random forest") {
        
        lrn = makeLearner("regr.randomForest")
        
        params <- makeParamSet(makeIntegerParam("mtry",lower = mtryL,upper = mtryU),
                               makeIntegerParam("nodesize",lower = nodesizeL,upper = nodesizeU),
                               makeIntegerParam("ntree",lower = ntreeL,upper = ntreeU),
                               makeIntegerParam("maxnodes",lower = maxnodesL,upper = maxnodesU)
        )
        
        
      } else if (tolower(model) == "decision tree") {
        
        lrn = makeLearner("regr.rpart")
        
        params <- makeParamSet(
          makeIntegerParam("minsplit",lower = minsplitL, upper = minsplitU),
          makeIntegerParam("minbucket", lower = minbucketL, upper = minbucketU),
          makeNumericParam("cp", lower = cpL, upper = cpU),
          makeIntegerParam("maxdepth",lower = maxdepthL,upper = maxdepthU)
        )
        
      } 
    } else if (tasktype == "Classification") {
    
       if (tolower(model) == "glmnet") {
        
        lrn <- makeLearner("classif.glmnet",predict.type = "prob")
        
        params <- makeParamSet(
          makeNumericParam(id = "alpha",  lower = alphaL, upper = alphaU),
          makeIntegerParam(id = "nlambda",  lower = nlambdaL, upper = nlambdaU),
          makeNumericParam(id = "lambda", lower = lambdaL, upper = lambdaU),
          makeNumericParam(id = "thresh", lower = threshL, upper = threshU),
          makeIntegerParam(id = "maxit", lower = maxitL,upper = maxitU)
        )
        
        
      }  else if (tolower(model) == "random forest") {
        
        lrn = makeLearner("classif.randomForest", predict.type = "prob", fix.factors.prediction = TRUE)
        
        params <- makeParamSet(makeIntegerParam("mtry",lower = mtryL,upper = mtryU),
                               makeIntegerParam("nodesize",lower = nodesizeL,upper = nodesizeU),
                               makeIntegerParam("ntree",lower = ntreeL,upper = ntreeU),
                               makeIntegerParam("maxnodes",lower = maxnodesL,upper = maxnodesU)
        )
        
        
      }  else if (tolower(model) == "decision tree") {
        
        lrn = makeLearner("classif.rpart",predict.type = "prob")
        
        params <- makeParamSet(
          makeIntegerParam("minsplit",lower = minsplitL, upper = minsplitU),
          makeIntegerParam("minbucket", lower = minbucketL, upper = minbucketU),
          makeNumericParam("cp", lower = cpL, upper = cpU),
          makeIntegerParam("maxdepth",lower = maxdepthL,upper = maxdepthU)
        )
      }
      
    }
    
    
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
    trained_model <- train(learner = lrn_tune,task = trainTask,weights = weights)
    
    return(list(mytune$x,mytune$y,trained_model))
  } #end of model_choice
  
  
  #### Model should be tuned or not
  if (tasktype == "Regression") {
  if (tolower(model) == "glmnet") {
    if (tune == T) {
      model_exec = model_choice(model = "glmnet")
      trained_model = model_exec[[3]]
    } else {
      lrn <- makeLearner("regr.cvglmnet",predict.type = "response")
      trained_model = train(lrn, task = trainTask,weights = weights)
      model_exec <- list(list(alpha=1,nlambda=100,lambda=trained_model$lambda.min,thresh = 1e-07, maxit = 1e+05),tune_measure_test = NULL)
    }
  } else if (tolower(model) == "random forest") {
    if (tune == T) {
      model_exec = model_choice(model = "random forest")
      trained_model = model_exec[[3]]
    } else {
      lrn <- makeLearner("regr.randomForest",predict.type = "response")
      trained_model = train(lrn, task = trainTask,weights = weights)
      model_exec <- list(list(nodesize=5, ntree=500),tune_measure_test = NULL)
    }
  }  else if (tolower(model) == "decision tree") {
    if (tune == T) {
      model_exec = model_choice(model = "decision tree")
      trained_model = model_exec[[3]]
    } else {
      lrn <- makeLearner("regr.rpart",predict.type = "response")
      trained_model = train(lrn, task = trainTask,weights = weights)
      model_exec <- list(list(minsplit =20, cp=0.01, maxdepth=30),tune_measure_test = NULL)
    }
  }
  
  } else if (tasktype == "Classification") {
    
    if (tolower(model) == "glmnet") {
    if (tune == T) {
      model_exec = model_choice(model = "glmnet")
      trained_model = model_exec[[3]]
    } else {
      lrn <- makeLearner("classif.cvglmnet",predict.type = "prob")
      trained_model = train(lrn, task = trainTask,weights = weights)
      model_exec <- list(list(alpha=1,nlambda=100,lambda=trained_model$lambda.min,thresh = 1e-07, maxit = 1e+05),tune_measure_test = NULL)
      
    }
    } else if (tolower(model) == "random forest") {
      if (tune == T) {
        model_exec = model_choice(model = "random forest")
        trained_model = model_exec[[3]]
      }  else {
      lrn <- makeLearner("classif.randomForest",predict.type = "prob")
      trained_model = train(lrn, task = trainTask,weights = weights)
      model_exec <- list(list(nodesize=5, ntree=500),tune_measure_test = NULL)
    }
  }  else if (tolower(model) == "decision tree") {
    if (tune == T) {
      model_exec = model_choice(model = "decision tree")
      trained_model = model_exec[[3]]
    } else {
      lrn <- makeLearner("classif.rpart",predict.type = "prob")
      trained_model = train(lrn, task = trainTask,weights = weights)
      model_exec <- list(list(minsplit =20, cp=0.01, maxdepth=30),tune_measure_test = NULL)
    }
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
  
  if (model == "glmnet") {
  if (length(levels(traintask[[outcome]])) == 2 | is.numeric(traintask[[outcome]])) {
    coef = coef(getLearnerModel(trained_model,more.unwrap = T))
    features = coef[,1]
  } else {features <- list()
  for (i in 1:(length(coef(getLearnerModel(trained_model,more.unwrap = T)))))
  {coef = coef(getLearnerModel(trained_model,more.unwrap = T))[i]
  features[[i]] = coef[[1]][,1]}
  }
  } else {
    mat <- getFeatureImportance(trained_model) 
    features <- tidyr::gather(mat$res, "variable", "importance") %>% arrange(desc(importance))
  }
  
   #normalize list 
   normalize_metrics <- list(mean = mean_normalize, SD = sd_normalize, Minimum = min_normalize, Maximum = max_normalize)
    return(list(test_pred = test_pred, train_pred = train_pred, hyperparameters = model_exec[[1]], 
                CV_measure = model_exec[[2]],model_features = features,normalize_metrics = normalize_metrics))

}



#Test
data = readRDS("/stats/projects/all/R_Tools_Development/data/salaries_data.Rds")

res = split(df=data,filetype_in = "dataframe", seed=50, splitkey = "patient_id", trainprop=0.7)
train = res[[1]]
test = res[[2]]


library(tidyverse)
test_res <- mlr_function(df_train=train,
                         df_test=test,
                         model="random forest",
                         exclude_vars = "patient_id",
                         tasktype="Regression",
                         outcome="salary",
                         normalize = T,
                         normalize_type = "scale",
                         weights = NULL,
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
                         mtryL = 1L,
                         mtryU = 4L,
                         nodesizeL = 1L,
                         nodesizeU = 2L,
                         ntreeL = 1L,
                         ntreeU = 4L,
                         maxnodesL = 2L,
                         maxnodesU = 4L,
                         minsplitL = 1L,
                         minsplitU = 2L,
                         minbucketL = 1L,
                         minbucketU = 2L,
                         cpL = 0.01,
                         cpU = 0.1,
                         maxdepthL = 1L,
                         maxdepthU = 2L,
                         meas = "mse",
                         gridsearch = "random",
                         search_maxit = 2L,
                         search_reso = 10L,
                         rdesc = "CV",
                         rdesc_iters = 5L,
                         rdesc_split = 2/3,
                         rdesc_folds = 10,
                         rdesc_reps = 10,
                         rdesc_stratify = FALSE,
                         rdesc_stratify_cols = NULL,
                         show_info = T)



