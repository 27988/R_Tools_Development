
#' Imputation of missing values.
#'
#' @param df_training A data frame or data table.
#' @param df_testing  A test data frame or data table. It is optional.
#' @param numvarlist  A vector of numeric variables to be imputed. If NULL, all numeric variables will be imputed.
#' @param min_outlimit The 1.5 * interquartile range (IQR=Q3-Q1) is subtracted from the value associated with this percentile; The value is between zero and one
#' @param max_outlimit The 1.5 * interquartile range (IQR=Q3-Q1) is added to the value associated with this percentile; The value is between zero and one
#' @param impute_type Type of statistic used for imputation; possible types: mean, median, max, min, mode. Default is mean.
#' @param round_mean_median Digit of rounding if impute_type is mean or median.
#' @param multiplemode When there are more than 1 value for mode of a column, the user can choose to impute with min or max of those modes. It will only be used when impute_type="mode"
#' @return input datasets with imputed values as new columns; table of summary statistics of training dataset before and after outlier check and imputation; list of columns imputed
#' @examples
#' t <-outlier_fn(df_training=df_a,  numvarlist=NULL, min_outlimit=0.25, max_outlimit=0.75, impute_type = "mode", round_mean_median=0 , multiplemode="max")
#' t <-outlier_fn(df_training=df_a, df_testing=df_b , numvarlist=NULL, min_outlimit=0.25, max_outlimit=0.75, impute_type = "mode", round_mean_median=0 , multiplemode="max")
#' t <-outlier_fn(df_training=df_a, df_testing=df_b , numvarlist=NULL, min_outlimit=0.25, max_outlimit=0.75, impute_type = "mean", round_mean_median=0 , multiplemode="max") 



outlier_fn = function(df_training, df_testing=NULL , numvarlist=NULL 
                   , min_outlimit=0.25, max_outlimit=0.75
                   , impute_type = "mode", round_mean_median=0, multiplemode = "min"){ 

library(data.table)
  getmode <- function(x) {
    x <- x[!is.na(x)]
    uniqx <- unique(x)
    tab <- tabulate(match(x, uniqx))
    uniqx[tab == max(tab)]
  }
  
 training_dt <-  setDT(df_training) 
 testing_dt <- setDT(df_testing)
 
 if(is.null(numvarlist)){
 numeric_columns <- names(dplyr::select_if(training_dt,is.numeric))
 } else{
   numeric_columns=numvarlist
 }
 
 summary_table <- data.frame(matrix(ncol=20, nrow=0))

 for (j in numeric_columns){
   
   min <-  quantile(training_dt[[j]], probs=min_outlimit, na.rm=TRUE)[[1]]
   max <-  quantile(training_dt[[j]], probs=max_outlimit, na.rm=TRUE)[[1]]
   iqr <- (quantile(training_dt[[j]], probs=0.75, na.rm=TRUE)[[1]] -
          quantile(training_dt[[j]], probs=0.25, na.rm=TRUE)[[1]])

   training_dt[[paste0(j,"_imputed")]] <- ifelse(is.na(training_dt[[j]]), "Yes-Missing"
                                            , ifelse(training_dt[[j]] < min - 1.5 * (iqr) | 
                                                        training_dt[[j]] > max + 1.5 * (iqr) ,  "Yes-Outlier" 
                                                     , "No"))
 
   training_dt[[paste0(j,"_excout")]] <- ifelse(training_dt[[j]] < min - 1.5 * (iqr), NA
                                                , ifelse(training_dt[[j]] > max + 1.5 * (iqr), NA
                                                         , training_dt[[j]]))
   
   train_stat <- NA
   if (tolower(impute_type) == "mean") {
     train_stat <- round(mean(training_dt[[paste0(j,"_excout")]], na.rm=TRUE),round_mean_median)  
     training_dt[[paste0(j,"_excout")]] <- ifelse(is.na(training_dt[[paste0(j,"_excout")]] ), train_stat 
                                                  , training_dt[[paste0(j,"_excout")]] )
   }
   else if (tolower(impute_type) == "median") {
     train_stat <- round(median(training_dt[[paste0(j,"_excout")]], na.rm=TRUE), round_mean_median)  
     training_dt[[paste0(j,"_excout")]] <- ifelse(is.na(training_dt[[paste0(j,"_excout")]] ), train_stat 
                                                  , training_dt[[paste0(j,"_excout")]] )
   }
   else if (tolower(impute_type) == "mode") {
      if (multiplemode=="min") {
            train_stat <- min(getmode(training_dt[[paste0(j,"_excout")]])) 
          }else if (multiplemode=="max") {
             train_stat <- max(getmode(training_dt[[paste0(j,"_excout")]])) 
          }else stop("ERROR: Only 'min' and 'max' allowed for multiple mode imputation")
     #train_stat <- getmode(training_dt[[paste0(j,"_excout")]])
     training_dt[[paste0(j,"_excout")]] <- ifelse(is.na(training_dt[[paste0(j,"_excout")]] ), train_stat 
                                                  , training_dt[[paste0(j,"_excout")]] )
   }
   else if (tolower(impute_type) == "max") {
     train_stat <- max(training_dt[[paste0(j,"_excout")]], na.rm=TRUE)
     training_dt[[paste0(j,"_excout")]] <- ifelse(is.na(training_dt[[paste0(j,"_excout")]] ), train_stat 
                                                  , training_dt[[paste0(j,"_excout")]] )
   }
   else if (tolower(impute_type) == "min") {
     train_stat <- min(training_dt[[paste0(j,"_excout")]], na.rm=TRUE)
     training_dt[[paste0(j,"_excout")]] <- ifelse(is.na(training_dt[[paste0(j,"_excout")]] ), train_stat 
                                                  , training_dt[[paste0(j,"_excout")]] )
   }
   else stop("ERROR: Only minimum, maximum, mean, median and mode allowed for type of numeric variables imputation")
 
 
   # now for test, do the same but compare with train outliers and replace missing with train averages 
   if(!is.null(testing_dt)){
     testing_dt[[paste0(j,"_imputed")]] <- ifelse(is.na(testing_dt[[j]]), "Yes-Missing"
                                                     , ifelse(testing_dt[[j]] < min - 1.5 * (iqr) | 
                                                                 testing_dt[[j]] > max + 1.5 * (iqr) ,  "Yes-Outlier" 
                                                              , "No"))
     
     testing_dt[[paste0(j,"_excout")]] <- ifelse(testing_dt[[j]] < min - 1.5 * (iqr), NA
                                                  , ifelse(testing_dt[[j]] > max + 1.5 * (iqr), NA
                                                           , testing_dt[[j]]))
   
   testing_dt[[paste0(j,"_excout")]] <- ifelse(is.na(testing_dt[[paste0(j,"_excout")]] ), train_stat , testing_dt[[paste0(j,"_excout")]] )
   }
 
   summary_table <- rbind( summary_table   ,
                             t(c(j
                                , min(training_dt[[j]], na.rm=TRUE) 
                                , quantile(training_dt[[j]], probs=0.25, na.rm=TRUE)
                                , median(training_dt[[j]], na.rm=TRUE) 
                                , mean(training_dt[[j]], na.rm=TRUE) 
                                , quantile(training_dt[[j]], probs=0.75, na.rm=TRUE)
                                , max(training_dt[[j]], na.rm=TRUE) 
                                , ifelse(length(getmode(training_dt[[j]]))==1,
                                                getmode(training_dt[[j]]), list(getmode(training_dt[[j]]))
                                )
                                , min - 1.5 * (iqr) 
                                , max + 1.5 * (iqr) 
                                , sum(is.na(training_dt[[j]]))
                                , ifelse(is.na(sum(training_dt[[j]] < min - 1.5 * (iqr) | 
                                      training_dt[[j]] > max + 1.5 * (iqr)
                                      )), 0 , sum(training_dt[[j]] < min - 1.5 * (iqr) | 
                                                    training_dt[[j]] > max + 1.5 * (iqr)
                                      )
                                      )# number of outliers
                                , train_stat # impute_value
                                , min(training_dt[[paste0(j,"_excout")]], na.rm=TRUE) 
                                , quantile(training_dt[[paste0(j,"_excout")]], probs=0.25, na.rm=TRUE)
                                , median(training_dt[[paste0(j,"_excout")]], na.rm=TRUE) 
                                , mean(training_dt[[paste0(j,"_excout")]], na.rm=TRUE) 
                                , quantile(training_dt[[paste0(j,"_excout")]], probs=0.75, na.rm=TRUE)
                                , max(training_dt[[paste0(j,"_excout")]], na.rm=TRUE) 
                                , list(getmode(training_dt[[paste0(j,"_excout")]]))
                                , ifelse(length(getmode(training_dt[[paste0(j,"_excout")]]))==1,
                                         getmode(training_dt[[paste0(j,"_excout")]])
                                         , list(getmode(training_dt[[paste0(j,"_excout")]]))
                                )
                              )))

 }
 colnames(summary_table) <- c("variable", "min_orig", "p25_orig",  "median_orig"
                              , "mean_orig", "p75_orig",  "max_orig", "mode_orig" 
                              , "min_outlier", "max_outlier" 
                              , "n_na_orig", "n_outlier"
                              , "impute_value"
                              , "min_impute", "p25_impute",  "median_impute"
                              , "mean_impute", "p75_impute",  "max_impute", "mode_impute") 
 # what to return:
 return(list(
  df_training=training_dt, df_testing=testing_dt
  , numvarlist = numeric_columns, summary_table=summary_table
))  
}



#testing
library(datasets)
main_data <- esoph # Data from a case-control study of (o)esophageal cancer in Ille-et-Vilaine, France
set.seed(123)
random_row = sort(sample(nrow(main_data), nrow(main_data)*.7))
df_a <- main_data[random_row,]
df_b <- main_data[-random_row,]
df_a <- tibble::add_row(df_a, .before=1) 
df_b <- tibble::add_row(df_b, .before=1) 
df_a$checkmode <- c(rep(NA, 4), rep(1,29),rep(2,29))
df_b$checkmode <- c(NA, NA, NA, NA, 1:24)

df_training <- df_a
df_testing <- df_b 

t1 <- outlier_fn(df_training=df_a, df_testing=df_b , numvarlist=NULL 
        , min_outlimit=0.25, max_outlimit=0.75
        , impute_type = "mean", round_mean_median=0 , multiplemode="min")

t2 <- outlier_fn(df_training=df_a, df_testing=df_b , numvarlist=NULL 
                , min_outlimit=0.25, max_outlimit=0.75
                , impute_type = "mode", round_mean_median=0 , multiplemode="min")

t3 <- outlier_fn(df_training=df_a, df_testing=df_b , numvarlist=NULL 
                 , min_outlimit=0.25, max_outlimit=0.75
                 , impute_type = "mode", round_mean_median=0 , multiplemode="max")

t4 <- outlier_fn(df_training=df_a, df_testing=df_b , numvarlist=NULL 
                 , min_outlimit=0.25, max_outlimit=0.75
                 , impute_type = "mode", round_mean_median=0 , multiplemode="median")

t5 <- outlier_fn(df_training=df_a, df_testing=df_b , numvarlist=NULL 
                 , min_outlimit=0.25, max_outlimit=0.75
                 , impute_type = "average", round_mean_median=0 , multiplemode="min")


df_a <- main_data[random_row,]
df_b <- main_data[-random_row,]
df_a <- tibble::add_row(df_a, .before=1) 
df_b <- tibble::add_row(df_b, .before=1) 
df_a$checkmode <- c(rep(NA, 2), rep(1,31),rep(2,29))
df_b$checkmode <- c(NA, NA, 1:26)

t6 <- outlier_fn(df_training=df_a, df_testing=df_b , numvarlist=NULL 
                 , min_outlimit=0.25, max_outlimit=0.75
                 , impute_type = "mode", round_mean_median=0 , multiplemode="min")

t7 <- outlier_fn(df_training=df_a, df_testing=df_b , numvarlist=NULL 
                 , min_outlimit=0.25, max_outlimit=0.75
                 , impute_type = "mode", round_mean_median=0 , multiplemode="max")


################## Imputation test
data_imputed <- impute(data=df_training ,numvarlist=NULL,type_num = "max",round=2,factvarlist=NULL,type_fact="mode")
data_imputed <- impute(data=df_training)

#labeling of factor worked perfectly :)
# what about modes of train set, would be used later in test?

# what if miss place numeric and factor varlist, can add an error?
data_imputed <- impute(data=df_training ,numvarlist=c("agegp"),type_num = "max",round=2,factvarlist=NULL ,type_fact="mode")
data_imputed <- impute(data=df_training ,numvarlist=NULL,type_num = "max",round=2,factvarlist=c("agegp"),type_fact="mode")

# what about multiple mode: replace by 1 and then 2 and 1 and then 2
data_imputed <- impute(data=df_training ,numvarlist=c("checkmode"),type_num = "mode",round=2,factvarlist=c("agegp"),type_fact="mode")

   
