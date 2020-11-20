#Eval Function --------------------------------------------------------

#' Evaluation Function for Model Outputs
#'
#' @param df_pred a numeric value of predicted probability or numeric value of predicted score
#' @param df_true a factor of true classes or numeric value of true score
#' @param threshold a logic value. Default is False. If the a classification model has predicted probability, 
#' set it to TRUE to convert probability to classes by user specific threshold value 
#' @param threshold_value a numeric value between 0 ~ 1, required for classification model with predicted probabilities inputs. 
#' Use it to obtain predicted classes. If predicted probablility is greater than or equal to this value, the predicted class will be 1, 0 otherwise
#' @param eval_metric types of evaluation metrics, currently accept confusion matrics, logloss, ROC-AUC curve,PR-AUC curve, quantile plot, brier score, 
#' root mean squred error, mean absolute error and r suqred 
#' @param bin a numeric bin value for quantile plot, default is 10
#' @return 
#' @examples

# build under R.3.5.2




eval_fun <- function(df_pred, df_true, threshold = FALSE, threshold_value = NULL, eval_metric, bin = 10){
  
  require(caret)
  require(tidyverse)
  require(Metrics)
  require(ggplot2)
  require(yardstick)
  
  #make sure df_true is in 0,1 level so it match what we covert in threshold (it is assuming in our truth column, first level is our case)
  levels(df_true)[levels(df_true)== levels(df_true)[1]] <- "1"
  levels(df_true)[levels(df_true)== levels(df_true)[2]] <- "0"
  levels(df_true) <-rev(levels(df_true))
  #get predicted class based on probability values
  if (threshold == TRUE & !is.null(threshold)){
    df_pred<-ifelse(df_pred >= threshold_value, "1", "0") 
    df_pred<-as.factor(df_pred)
  }
  
  if (eval_metric == "confusion matrix") {
    cm<- caret::confusionMatrix(df_pred, df_true) #The functions requires that the factors have exactly the same levels
    res<-cm
  }
  else if (eval_metric == "logloss") {
    df_true<-as.numeric(as.character(df_true))
    ll<- Metrics::logLoss(df_true, df_pred) #df_pred in probability and df_true as numeric 0,1
    res<-ll
  }
  else if (eval_metric == "roc") {  # keep predicted column as probability if need roc curve
    df<-data.frame(df_true, df_pred)
    colnames(df)[1] <- "df_true"
    colnames(df)[2] <- "df_pred"
    yardstick::roc_auc(df, df_true, df_pred) ->sc1
    yardstick::roc_curve(df, df_true, df_pred) %>%   # df_true is a binary factor with two classes and df_pred is numeric pred prob.
      ggplot(aes(x = 1 - specificity, y = sensitivity)) +  
      geom_path(lwd = 1, alpha = 0.8, colour = "blue") +
      labs(title = "Receiver Operator Curve") + 
      coord_equal() + 
      xlim(0, 1) + ylim(0, 1) +
      theme_bw() + 
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
            legend.text = element_text(size = 14),
            legend.title = element_text(size=14),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            axis.title = element_text(size = 14)) +
      geom_text(x = 0.8, y = 0.1, fontface =2, size =5, colour = "red",
                aes(label = paste0("ROC AUC:", round(`.estimate`, 3))), 
                data = sc1) -> res
  }
  else if (eval_metric == "pr"){ # keep predicted column as probability if need pr curve
    df<-data.frame(df_true, df_pred)
    colnames(df)[1] <- "df_true"
    colnames(df)[2] <- "df_pred"
    yardstick::pr_auc(df, df_true, df_pred) ->sc2
    yardstick::pr_curve(df, df_true, df_pred) %>%  
      ggplot(aes(x = recall, y = precision)) +  
      geom_path(lwd = 1, alpha = 0.8, colour = "green3") +
      labs(title = " Precision Recall Curve") + 
      coord_equal() + 
      xlim(0, 1) + ylim(0, 1) +
      theme_bw()+
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
            legend.text = element_text(size = 14),
            legend.title = element_text(size=14),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            axis.title = element_text(size = 14)) +
      geom_text(x = 0.8, y = 0.1, fontface =2, size =5, colour = "red",
                aes(label = paste0("PR AUC:", round(`.estimate`, 3))), 
                data = sc2) -> res
  }
  else if (eval_metric == "rmse") {
    rmse<- Metrics::rmse(df_true, df_pred)
    res<-rmse
  }
  else if (eval_metric == "mae") {
    mae<- Metrics::mae(df_true, df_pred)
    res<-mae
  }
  else if (eval_metric == "rsqure"){
    res<-(1- sum((df_pred - df_true) ^ 2)/sum((df_true - mean(df_true))^2))
  }
  else if (eval_metric == "quantile" & !is.null(bin)){ #df_pred(in prob? in class?)
    df_true<-as.numeric(as.character(df_true))
    df_pred<-as.numeric(df_pred)
    qt <- data.frame(response = df_pred,  actual= df_true) %>% 
      arrange(response) %>% 
      mutate(bin_value = ntile(response, bin)) %>% 
      group_by(bin_value) %>%
      summarise(sum_actual= mean(actual), sum_pred = mean(response), .groups = 'drop')
    qt<-as.data.frame(qt)
    qt1<-qt[,c(1,2)]
    qt1$Group <- rep("Actual", nrow(qt1))
    colnames(qt1)[2] <- "Value"
    qt2<-qt[,c(1,3)]
    qt2$Group <- rep("Predict", nrow(qt2))
    colnames(qt2)[2] <- "Value"
    qt<-rbind(qt1, qt2)
    res <- ggplot(data=qt,
                  aes(x=bin_value, y=Value, colour= Group)) +
      geom_line(lwd = 1, alpha=0.8) + labs(title="Quantile Plot", x ="Bin", y = "Value") +
      theme_bw() +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
            legend.text = element_text(size = 14),
            legend.title = element_text(size=14),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            axis.title = element_text(size = 14))
  }
  else if (eval_metric == "brier"){  #df_pred should be in probability and df_true in numeric 0,1
    df_true<-as.numeric(as.character(df_true))
    res <-mean((df_pred- df_true)^2)
  }
  else stop ("ERROR: Please select a vaild evaluation matrix")
  return(res)
}


#Test
library(yardstick)
data("two_class_example")
eval_fun(two_class_example$Class1, two_class_example$truth, eval_metric = "confusion matrix", threshold = T, threshold_value = 0.5)
eval_fun(two_class_example$Class1, two_class_example$truth, eval_metric = "pr")
eval_fun(two_class_example$Class1, two_class_example$truth, eval_metric = "roc")
eval_fun(two_class_example$Class1, two_class_example$truth, eval_metric = "quantile")




