# R tool Development 

# descriptive statistics

# Author: Yi Yang

#############################################################################################################


#' Summary of Descriptive Statistics
#'
#' @param formula An object of class formula, describing how variables to be summarized. 
#' @param data input data for summary statistics, default is data frame. If data is not in data frame, please put your data path here
#' @param filetype input filetype, currently can accept data frame, rds, csv, xls, xlsx, default is data frame.
#' @param savetype output type, currently can be rmd, rds, html and pdf, default is rmd.
#' @param savepath if output type is rds, html and pdf, please put your save path here. 
#' @param to_numeric an optional input to change variable class to numeric
#' @param to_factor an optional input to change variable class to factor
#' @return input dataset with imputed values.
#' @examples
#' 
#' d_summary(sex ~ race + age, mockstudy)
#' d_summary(sex ~ race + age, mockstudy, to_factor = "race")
#' d_sum<-d_summary(sex ~ race + age, mockstudy, to_factor = "race")
#' d_summary(sex ~., mockstudy, savetype = "rds", savepath = "/home/yyang/r_pack/r_test.Rds")
#' d_summary(sex ~ age + bmi + race, to_factor = "race", mockstudy, savetype = "pdf", savepath = "/home/yyang/r_pack/r_test.pdf")
#' d_summary(sex ~ age + bmi + race, to_factor = "race", mockstudy, savetype = "html", savepath = "/home/yyang/r_pack/r_test.html")



# require pacakage: tidyverse, arsenal

d_summary <- function(formula, data, filetype = "dataframe", to_numeric = NULL, to_factor = NULL, savetype = "rmd", savepath = NULL) {
  
  if (tolower(filetype) == "dataframe") {data = data}
  else if (tolower(filetype) == "rds") {data = readRDS(data)}
  else if (tolower(filetype) == "csv") {data = read.csv(data, header = TRUE, sep = ",")}
  else if (tolower(filetype) == "xls") {data = read.xls(data, sheetName = 1, header = TRUE)}
  else if (tolower(filetype) == "xlsx") {data = read.xlsx(data, sheetName = 1, header = TRUE)}
  else stop ("ERROR: Please select a valid file type")
  
  library(arsenal)
  library(tidyverse)
  
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
  
  data_a<-dplyr::select_if(data, purrr::negate(is.character))
  data_b<-names(dplyr::select_if(data, is.character))
  
  f1 <- as.formula(formula)
  tab1<-tableby(f1, data_a)
  tab1s<- summary(tab1, text = TRUE)
  
  if (tolower(savetype) == "rmd") {
    tab1s %>%
      as.data.frame() %>%
      kable() %>%
      kable_styling(bootstrap_options = c("striped", "hover")) -> tab2
    result<-tab2
  } 
  else if (tolower(savetype) == "rds" & !is.null(savepath)){
    tab2<-as.data.frame(tab1s)
    tab2<-dplyr::rename(tab2, variables = "")
    saveRDS(tab2,savepath)
  }
  else if (tolower(savetype) == "html" & !is.null(savepath) ){
    write2html(tab1, savepath)
  }
  else if (tolower(savetype) == "pdf" & !is.null(savepath) ){
    write2pdf(tab1, savepath)
  }
  else stop ("ERROR: Please check if savetype or savepath is missing/valid")
  
  if (! purrr::is_empty(data_b) & savetype == "rmd") {
    message ("Warning: You have some character variables below dropped from summary, use to_factor to convert")
    return(list (data_b, result))
  }
  else if (! purrr::is_empty(data_b) & savetype != "rmd") {
    message ("Warning: You have some character variables below dropped from summary, use to_factor to convert")
    return(data_b)
  }
  
}



######## test
d_summary(sex ~ race + age, mockstudy)
d_summary(sex ~ race + age, mockstudy, to_factor = "race")
d_sum<-d_summary(sex ~ race + age, mockstudy, to_factor = "race")
d_summary(sex ~., mockstudy, savetype = "rds", savepath = "/home/yyang/r_pack/r_test.Rds")
d_summary(sex ~ age + bmi + race, to_factor = "race", mockstudy, savetype = "pdf", savepath = "/home/yyang/r_pack/r_test.pdf")
d_summary(sex ~ age + bmi + race, to_factor = "race", mockstudy, savetype = "html", savepath = "/home/yyang/r_pack/r_test.html")






















