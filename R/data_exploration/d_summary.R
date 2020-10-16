# R tool Development 

# descriptive statistics


#############################################################################################################


#' Summary of Descriptive Statistics
#'
#' @param formula An object of class formula, describing how variables to be summarized. 
#' @param df Input data for summary statistics, default is data frame. If data is not in data frame, please put your data path here
#' @param filetype_in Input file type, currently can accept data frame, rds, csv, xls, xlsx, default is data frame.
#' @param filetype_out Output type, currently can be rmd, rds, html and pdf, default is rmd.
#' @param output_path If output type is rds, html or pdf, please put your save path here. 
#' @param to_numeric An optional input to change variable class to numeric
#' @param to_factor An optional input to change variable class to factor
#' @param level_droprange set the maximum level of a character variable that will be included in the summary table, 
#' if a character variable's level exceeds this range, it will be automatically drop from the summary table, default value is 20.
#' @return Descriptive Summary Tables
#' @examples
#' 
#' d_summary(sex ~ race + age, mockstudy)
#' d_summary(sex ~ race + age, mockstudy, cat.stats = "countrowpct")  # if you need to calculate row percentage
#' d_summary(sex ~., mockstudy, level_droprange = 5) # set level drop range rather than default
#' d_summary(sex ~ race + age, mockstudy, filetype_out = "rds", output_path = "/home/yyang/r_pack/r_test.Rds")
#' d_summary(sex ~ age + bmi + race, mockstudy, filetype_out = "pdf", output_path = "/home/yyang/r_pack/r_test.pdf")
#' d_summary(sex ~., mockstudy, filetype_out = "pdf", output_path = "/home/yyang/r_pack/r_test.pdf", level_droprange = 5)





d_summary <- function(formula, df, filetype_in = "dataframe", to_numeric = NULL, to_factor = NULL, level_droprange = 20,  filetype_out = "rmd", output_path = NULL, ...) {
  
  
  if (tolower(filetype_in) == "dataframe") {data = df}  else if (tolower(filetype_in) == "rds") {
    data = readRDS(df)}  else if (tolower(filetype_in) == "csv") {
      data = read.csv(df, header = TRUE, sep = ",")}  else if (tolower(filetype_in) == "xls") {
        data = read.xls(df, sheetName = 1, header = TRUE)}  else if (tolower(filetype_in) == "xlsx") {
          data = read.xlsx(df, sheetName = 1, header = TRUE)}  else stop ("ERROR: Please select a valid file type")
  
  library(arsenal)
  library(tidyverse)
  library(kable)
  library(kableExtra)
  
  if (!is.null(to_numeric)) {
    for(i in to_numeric ){
      data[[i]] <- as.numeric(as.character(data[[i]]))
    }
  }
  
  if (!is.null(to_factor)) {
    for(i in to_factor){
      data[[i]] <- as.factor(as.character(data[[i]]))
    }
  }
  
  
  var_type_identify <- function (data,excludeVars=NULL) {
    
    #Identify numeric variables 
    numVars <- NA
    
    if (is.na(numVars[1])) {
      for (i in 1:dim(data)[2]) {
        yesnum <-  is.numeric(data[,i]) | is.integer(data[,i])
        numVars <- append(numVars,ifelse(yesnum == "TRUE", colnames(data)[i],numVars),after = length(numVars))
      }
      
    }
    numVars <- na.omit(numVars)
    numVars <- numVars[!(numVars %in% excludeVars)]
    
    #Identify character/categorical variables
    
    categoricalVars <- NA
    
    if (is.na(categoricalVars[1])) {
      categoricalVars <- NA
      for (i in 1:dim(data)[2]) {
        yescat <-  is.character(data[,i]) | is.factor(data[,i])
        categoricalVars <- append(categoricalVars,ifelse(yescat == "TRUE", colnames(data)[i],categoricalVars),after = length(categoricalVars))
      }
    }
    categoricalVars <- na.omit(categoricalVars)
    categoricalVars <- categoricalVars[!(categoricalVars %in% excludeVars)]
    
    return(list(numVars,categoricalVars))
  }
  
  var_type_cf <- var_type_identify(data)[[2]]    # get character/factor variablie lists
  
  if (!is.null(var_type_cf)) {
    for(i in var_type_cf){
      data[[i]] <- as.factor(as.character(data[[i]]))
    }
  }
  
  if (! is.null(level_droprange)) {
    l1 <- setDT(as.data.frame(sapply(data[,sapply(data, is.factor)], nlevels)), keep.rownames = TRUE)  
    names(l1)[2] <- "nlevels"
    l2 <- subset(l1, l1$nlevels>level_droprange)
    l3<-l2$rn   # drop list names
  }
  
  if (!purrr::is_empty(l3)) {data_a <- data[, -which(names(data) %in% l3)] # remove factor column which level exceed the setting range
  }  else if (purrr::is_empty(l3)) {data_a <- data}  else stop ("ERROR: Please check level droprange")
  
  
  f1 <- as.formula(formula)
  tab1<-tableby(f1, data_a, ...)
  tab1s<- summary(tab1, text = TRUE, ...)
  
  if (tolower(filetype_out) == "rmd") {
    tab1s %>%
      as.data.frame() %>%
      kable()     %>%
      kable_styling(bootstrap_options = c("striped", "hover")) %>%
      save_kable(file = "table1.html", self_contained = T)
    # result<-tab2 
  } 
  else if (tolower(filetype_out) == "rds" & !is.null(output_path)){
    tab2<-as.data.frame(tab1s)
    tab2<-dplyr::rename(tab2, variables = "")
    saveRDS(tab2,output_path)
  }
  else if (tolower(filetype_out) == "html" & !is.null(output_path) ){
    write2html(tab1, output_path)
  }
  else if (tolower(filetype_out) == "pdf" & !is.null(output_path) ){
    write2pdf(tab1, output_path)
  }
  else stop ("ERROR: Please check if filetype_out or output_path is missing/valid")
  
  if (! purrr::is_empty(l3) & filetype_out == "rmd") {
    message ("Warning: Variables shown below are dropped from summary since they exceed maximum level_droprange")
    return((list(l3, browseURL("table1.html") )))
  }
  else if (! purrr::is_empty(l3) & filetype_out != "rmd") {
    message ("Warning: Variables shown below are dropped from summary since they exceed maximum level_droprange")
    return(l3)
  }
  else if (purrr::is_empty(l3) & filetype_out == "rmd") {
    return(browseURL("table1.html") )
  }
  
}


#Test

library(data.table)
library(kableExtra)
library(knitr)


d_summary(rank ~ ., data,level_droprange=3)
d_summary(rank ~ ., data,level_droprange=4,filetype_out = "pdf", output_path = "/stats/projects/all/R_Tools_Development/data/r_test.pdf")

