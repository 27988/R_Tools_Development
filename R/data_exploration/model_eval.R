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
#' root mean squred error, mean absolute error, r suqred and Hosmer-Lemeshow statistics 
#' @param bin a numeric bin value for quantile plot, default is 10
#' @param  g a numeric g value for  Hosmer-Lemeshow test, default is 10
#' @param stat_type specify the type of Hosmer-Lemeshow test, default is H-L-C statistics. If need H-L-H statistics, set stat_type = "H"
#' @return 
#' @examples

# build under R.3.5.2


eval_fun <- function(df_pred, df_true, threshold = FALSE, threshold_value = NULL, eval_metric, bin = 10, g = 10, stat_type = "C"){
  
  require(caret)
  require(tidyverse)
  require(Metrics)
  require(ggplot2)
  require(yardstick)
  
  #make sure df_true level match what we covert in threshold (it is assuming in our truth column, first level is our case)
  levels(df_true)[levels(df_true)== levels(df_true)[1]] <- "1"
  levels(df_true)[levels(df_true)== levels(df_true)[2]] <- "0"
  
  #get predicted class based on probability values
  if (threshold == TRUE & !is.null(threshold)){
    df_pred<-ifelse(df_pred >= 0.5, "1", "0") 
    df_pred<-as.factor(df_pred)
    df_pred <- factor(df_pred, levels = c("1", "0")) # make sure df_pred level match with df_true level
  }
  
  # H-L test 
  hosmer_lemeshow <- function(df_pred, df_true, g, stat_type){
    
    mtx = cbind(df_true, y_not = 1- df_true, df_pred, prob_not = 1-df_pred)
    mtx = as.data.frame(mtx)
    mtx = mtx[order(mtx$df_pred),]
    n <- length(df_pred)/g
    nr <- nrow(mtx)
    
    ## C statistics, same number of instances in each bin
    if (stat_type == "C"){
      split_mtx = split(mtx, rep(1:ceiling(nr/n), each=n, length.out=nr))  
      
      C_stat = 0
      for (i in 1:length(split_mtx)){
        obs = sum(split_mtx[[i]]$df_true == 1)
        exp = sum(split_mtx[[i]]$df_pred)
        obs_not = sum(split_mtx[[i]]$df_true == 0)
        exp_not = sum(split_mtx[[i]]$prob_not)
        if (exp == 0 || exp_not == 0){
          next
        }
        bin_sum = ((obs - exp)**2)/exp + ((obs_not - exp_not)**2)/exp_not
        C_stat = C_stat + bin_sum
      }
      PVAL = 1 - pchisq(C_stat, g - 2)
      
      stat = cbind(PVAL, C_stat)
      stat <- as.data.frame(stat)  
      colnames(stat)[1] <- "P-value"
      colnames(stat)[2] <- "C Statistics"
      
    }else if (stat_type == "H"){ ### H statistics, equal intervals
      split_mtx = split(mtx, cut(mtx$df_pred, seq(0,1,1/g), include.lowest=TRUE))
      split_mtx = split_mtx[sapply(split_mtx, nrow)>0]
      
      H_stat = 0
      for (i in 1:length(split_mtx)){
        obs = sum(split_mtx[[i]]$df_true == 1)
        exp = sum(split_mtx[[i]]$df_pred)
        obs_not = sum(split_mtx[[i]]$df_true == 0)
        exp_not = sum(split_mtx[[i]]$prob_not)
        if (exp == 0 || exp_not == 0){
          next
        }
        bin_sum = ((obs - exp)**2)/exp + ((obs_not - exp_not)**2)/exp_not
        H_stat = H_stat + bin_sum
      }
      PVAL = 1 - pchisq(H_stat, g - 2)
      
      stat = cbind(PVAL, H_stat)
      stat <- as.data.frame(stat)
      colnames(stat)[1] <- "P-value"
      colnames(stat)[2] <- "H Statistics"
      
    }
    return(stat)
  }
  
  # H-L Diagram
  reliability_diagram <- function(df_pred, df_true, g, stat_type) {
    
    #levels(df_true)[levels(df_true)== levels(df_true)[1]] <- "1"
    #levels(df_true)[levels(df_true)== levels(df_true)[2]] <- "0"
    
    #df_true<-as.numeric(as.character(df_true))
    #df_pred<-as.numeric(df_pred)
    df<-data.frame(df_true, df_pred)
    
    min.pred <- min(df_pred)
    max.pred <- max(df_pred)
    min.max.diff <- max.pred - min.pred
    
    if (stat_type == "H"){
      mtx = cbind(df_true, df_pred)
      mtx = as.data.frame(mtx)
      mtx = mtx[order(mtx$df_pred),]
      res = data.frame(V1= numeric(0), V2 = numeric(0))
      split_mtx = split(mtx, cut(mtx$df_pred, seq(0,1,1/g), include.lowest=TRUE))
      
      for (i in 1:length(split_mtx)){
        col_mean = colMeans(split_mtx[[i]])
        if (sum(is.na(col_mean)) > 0) {
          next
        }
        res[i,] = col_mean
      }
      
      res2 = data.frame(ni= numeric(0))
      for (i in 1: length(split_mtx)){
        row_c = nrow(as.data.frame(split_mtx[i]))
        res2[i,] = row_c
      }
      
      resf<-cbind(res, res2)
      
      hosmer_lemeshow(df_pred, df_true, g, stat_type = "H") ->hls
      
      ggplot(resf, aes(x=V2,  y=V1)) +  geom_point()+ geom_line(color = "red", size = 1) +
        geom_text(label=resf$ni, vjust= -1.7) +
        xlab("Mean Predicted") + ylab("Mean Observed") + labs(title = "Hosmer-Lemeshow H-Statistics") +
        xlim(0, 1) + ylim(-0.05, 1.05) + geom_abline(intercept = 0, slope = 1, color="blue", linetype="dashed", size=0.8) +
        theme_bw() + theme(legend.position="bottom") +
        geom_point(data = df, aes(x=df_pred,  y=df_true), alpha= 0.2) +
        geom_text(x = 0.85, y = 0.1, size =4, colour = "blue",
                  aes(label = paste0("H Statistics: ", round(hls$`H Statistics`, 3)))) +
        geom_text(x = 0.85, y = 0.15, size =4, colour = "blue",
                  aes(label = paste0("P-value: ", round(hls$`P-value`, 4))))+
        theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
              legend.text = element_text(size = 14),
              legend.title = element_text(size=14),
              axis.text.x = element_text(size = 14),
              axis.text.y = element_text(size = 14),
              axis.title = element_text(size = 14)) ->res1 }   
    
    else if (stat_type == "C"){
      ## C statistics, same number of instances in each bin
      mtx = cbind(df_true, df_pred)
      mtx = as.data.frame(mtx)
      mtx = mtx[order(mtx$df_pred),]
      n <- length(df_pred)/g            
      nr <- nrow(mtx)
      #split_mtx = split(mtx, ntile(1:nr, round(n)))
      split_mtx = split(mtx, rep(1:ceiling(nr/n), each=n, length.out=nr))
      res = data.frame(V1= numeric(0), V2 = numeric(0))
      for (i in 1:length(split_mtx)){
        res[i,] = colMeans(split_mtx[[i]])
      }
      ni = rep(nrow(as.data.frame(split_mtx[1])), nrow(res))
      resf<- cbind(res, ni)
      
      hosmer_lemeshow(df_pred, df_true, g, stat_type = "C") ->hls
      
      ggplot(resf, aes(x=V2,  y=V1)) +  geom_point()+ geom_line(color = "red", size = 1) +
        geom_text(label=resf$ni, vjust= -1.7) +
        xlab("Mean Predicted") + ylab("Mean Observed") + labs(title = "Hosmer-Lemeshow C-Statistics") +
        xlim(0, 1) + ylim(-0.05, 1.05) + geom_abline(intercept = 0, slope = 1, color="blue", linetype="dashed", size=0.8) +
        theme_bw() + theme(legend.position="bottom") +
        geom_point(data = df, aes(x=df_pred,  y=df_true), alpha= 0.2) +
        geom_text(x = 0.85, y = 0.1, size =4, colour = "blue",
                  aes(label = paste0("C Statistics: ", round(hls$`C Statistics`, 3)))) +
        geom_text(x = 0.85, y = 0.15, size =4, colour = "blue",
                  aes(label = paste0("P-value: ", round(hls$`P-value`, 4))))+
        theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
              legend.text = element_text(size = 14),
              legend.title = element_text(size=14),
              axis.text.x = element_text(size = 14),
              axis.text.y = element_text(size = 14),
              axis.title = element_text(size = 14)) ->res1
    }
    
    return(res1)
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
  else if (eval_metric == "hl" & !is.null(g) & stat_type == "C") {
    df_true<-as.numeric(as.character(df_true))
    df_pred<-as.numeric(df_pred)
    res<-reliability_diagram(df_pred, df_true, g, stat_type = "C") 
  }
  else if (eval_metric == "hl" & !is.null(g) & stat_type == "H") {
    df_true<-as.numeric(as.character(df_true))
    df_pred<-as.numeric(df_pred)
    res<-reliability_diagram(df_pred, df_true, g, stat_type = "H") 
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
eval_fun(two_class_example$Class1, two_class_example$truth, eval_metric = "hl")
eval_fun(two_class_example$Class1, two_class_example$truth, eval_metric = "hl", g=20, stat_type = "H")


