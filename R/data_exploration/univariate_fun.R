#' Conduct data exploration on individual variables
#'
#' @param df Input dataset; function accepts data.frame objects, as well as direct inputs of .rda/.RData, .rds, .csv, .xls, and .xlsx files
#' @param outcome Outcome variable name
#' @param exclude_vars Optional character string or vector of character strings specifying variables that should be excluded from analysis, I.E. identifiers, record version indicators, etc.
#' @param filetype_out Type of output. Defaults to "rmd," other valid options are "pdf", "rds" or "list" / "object"
#' @param output_path Directory to save output. Default is the current working directory
#' @return 
#' @examples
#' univariate_fun(df = salaries_data, exclude_vars = "patient_id, plot = TRUE)
univariate_fun <- function(df, outcome , exclude_vars = NULL,  filetype_out = "rmd", output_path) {
 
  require(tidyverse)
  require(grid)
  require(gridExtra)
  library(plotly)
  library(ggplot2)
  library(rmarkdown)
  library(knitr)

  #Check to install packages
  list.of.packages <- c("devtools")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  
  #Check to install Github packages
  # list.of.packages.github <- c("rstudio/webshot2")
  # new.packages <- list.of.packages.github[!(list.of.packages.github %in% installed.packages()[,"Package"])]
  # if(length(new.packages)) install_github(new.packages)
  
  #Flag_path set o 'no' initially
  flag_path <- 'no'
  
  #defining internal functions
    exclude_parser <- function(df, exclude_vars) {
    if (
      sum(
        stringr::str_detect(
          paste0(names(df), collapse = "|"), exclude_vars)) < length(exclude_vars)) 
    {
      missing_index <- which(
        !stringr::str_detect(paste0(names(df), collapse = "|"), exclude_vars))
      stop(sprintf("Variable %s not found in dataset %s", 
                   exclude_vars[missing_index], 
                   deparse(substitute(df))))
    } else {return(TRUE)}
  }
  #preparing df by converting character variables to factors and numeric variables 
  #   into discrete factors
  df_prep_fun <- function(df) {
    colnames(df) <- str_replace_all(colnames(df), pattern = "\\.", replacement = "\\_")
    outcome_var <- select(df,outcome)
    df %>%
      select(-all_of(outcome)) %>%
      mutate_if(Negate(is.numeric),            #   without removing anything
                .funs = ~ as.factor(x = .)) %>% # converting non-numeric variables to factors
      ## 7/17 note - 
      # buckets - maybe 5, maybe user inputs#
      mutate_if(is.numeric, ~ cut_number(x = ., n = 5) %>% 
                  as.factor(.)) %>%           
      bind_cols(outcome_var, .) %>%
      return(.)
  }
  
  #### checking input filetypes 
  accepted_filetypes <- c("rds", "rda", "rdata", "xls", "xlsx", "csv")
  df_name <- deparse(substitute(df))
  
  if(str_detect(tolower(df_name), pattern = paste0(accepted_filetypes, collapse = "|"))) 
  {
    if (str_detect(tolower(df_name), "rds"))
    {
      df <- readRDS(df)
    }
    else if(str_detect(tolower(df_name), "rda|rdata")) {
      df <- get(load(df))
    }
    else if (str_detect(tolower(df_name), "xls|xlsx")) 
    {
      df <- read.xlsx(df, sheetName = 1, header = TRUE)
    }
    else if (str_detect(tolower(df_name), "csv")) 
    {
      df <- read.csv(df, header = TRUE, sep = ",")
    }
  }   else if (class(df) == "data.frame") {
    df <- df
  }   else stop("input must be data.frame, R object, or delimited file!")
  
  exclude_parser(df = df, exclude_vars = exclude_vars)
  #running univariate analysis 
  df <- df_prep_fun(df)
  
    df_out <- df %>% 
      select(-all_of(exclude_vars),-all_of(outcome)) %>%
      lapply(table) %>% 
      lapply(as.data.frame) %>% 
      do.call(rbind, .) %>% 
      rownames_to_column(., var = "variable") %>%
      dplyr::rowwise(.) %>%
      mutate(variable = str_split(variable, pattern = "\\.", simplify = T)[[1]]) %>%
      rename("level" = Var1, "n" = Freq) %>%
      group_by(variable) %>%
      mutate(total = sum(n), prop = round(n/total, 3)) %>% 
      ungroup(.) %>%
      select(-total)
    
    
      summ_out <- function(x){
      call <- substitute(df   %>%
                           group_by_(x)  %>%
                         summarise(MeanOutcome = round(mean(out),4)) ,list(out = as.name(outcome)))
    eval(call) %>% mutate(variable = x) %>% rename(level = as.name(x))
    }
    
    col_names <- colnames(df)[!colnames(df) %in% c(exclude_vars,outcome)]
    summ_out_f <- NULL
    for (i in 1:length(col_names)){
      temp <- summ_out(col_names[i])
      summ_out_f <- rbind(summ_out_f,temp)
    }
    
    df_out <- merge(df_out,summ_out_f,by=c("variable","level"),all=T)
    df_out <- base::split(df_out, df_out$variable)
    
    predictor_names <- df %>%
      select(-all_of(exclude_vars),-all_of(outcome)) %>%
      colnames(.)
    
    
    #plotly
    f <- list(
      family = "Courier New, monospace",
      size = 18,
      color = "#7f7f7f"
    )
   
    y <- list(
      title = "<b>Proportion of Levels (bar)"
      
    )
    
    t <- list(
      family = "Times serif",
      size = 18,
      color = 'black')
    
    plots <- lapply(predictor_names, function(x) 
    {
      df_temp <- df_out[[x]]
      df_temp <- df_temp[,-1]
      plot_temp <- plot_ly(data=df_temp) %>%
        add_trace(x=~level, y=~prop,type="bar",showlegend=FALSE,marker = list(color ='lightgrey'),hoverinfo = 'none' ) %>%
        add_lines(x=~level, y=~MeanOutcome,
                  type="scatter", mode="lines",line=list(width=2.2),yaxis="y2",
                  hoverinfo = 'none'  ) %>%
        layout(showlegend = FALSE,yaxis = y,
               yaxis2 = list(title="<b>Mean Outcome (line)", overlaying = "y", side = "right",automargin = T,rangemode='tozero',
                             yaxis2 = list(title="Mean Outcome (line)"),yaxis = list(title="Proportion of Levels (bar)"),
                             showlegend = FALSE))
               
      
      # data_temp <- plot_ly(type="table",
      #                      header=list(values=names(df_temp),
      #                                  fill=list(color =rep("grey",dim(df_temp)[2])),
      #                                  font = list(family = "Arial", size = 14, color = "white"),
      #                                  line = list(width = 1, color = 'black')),
      #                      cells=list(values=unname(df_temp)),
      #                      domain = list(x=c(0,0.3),y=c(0,1))) %>%  layout(showlegend = FALSE)
      # combo_out <- plotly::subplot(plot_temp,data_temp,nrows = 2) %>%
      #   layout(title=x,titlefont=t,xaxis = list(domain=c(0.5,1.0)),
      #          xaxis2 = list(domain=c(0,0.5)),yaxis2 = list(title="Mean Outcome (line)"),yaxis = list(title="Proportion of Levels (bar)"),
      #                                                                                                   showlegend = FALSE)
      
      return(plot_temp)
    })
    
    names(plots) <- predictor_names
    
    univariate_list_object <- list(
      predictor_names = predictor_names,
      df = df_out[predictor_names],
      plots = plots)
    names(univariate_list_object$plots) <- predictor_names
    
    if(missing(output_path)) {
      output_path <- paste0(getwd(), "/")
      flag_path <- 'yes'
    } else output_path <- output_path
    
    if(tolower(filetype_out) %in% "list") {
       return(univariate_list_object)
    }    else if(tolower(filetype_out) %in% "pdf")     {
      
      univariate_list_name <- paste0('univariate-list-object_',Sys.Date(), '.RDS')
      saveRDS(univariate_list_object, file = paste0(output_path, univariate_list_name))
      
      top_block_yaml <- c('---',
                          'title: "Univariate Analysis Report"',
                          # paste0('subtitle: "Data Source: ', df_name, '"', collapse = ""),
                          paste0('author: "', Sys.info()[["user"]],'"'),
                          paste0('date: "', format(Sys.Date(), '%m/%d/%Y'), '"'),
                          'output: pdf_document',
                          '---',
                          '',
                          '```{r setup, include=FALSE}',
                          'knitr::opts_chunk$set(echo = TRUE,warning=FALSE, message=FALSE)',
                          '',
                          'library("tidyverse")',
                          'library("grid")',
                          'library("gridExtra")',
                          'library("ggpubr")',
                          'library("knitr")',
                          'library("webshot")',
                          'library("plotly")',
                          'library("htmlwidgets")',
                          'library("devtools")',
                          '',
                          if (flag_path == "no") {paste0('univariate_list_object <- readRDS(file = ".', output_path,
                                 univariate_list_name, '")')} else
                                   {paste0('univariate_list_object <- readRDS(file = "', output_path,
                                           univariate_list_name, '")')},
                          '```\n')
      
      name_vec <- univariate_list_object$predictor_names
      table_vec <- paste('univariate_list_object$df$', name_vec, sep = '')
      plot_vec <- paste('univariate_list_object$plots$', name_vec, sep = '')
      
      body <- paste('#', name_vec, '\n\n:::: {style="display: flex;"} \n\n::: {} \n\n',
                    '\n \n```{r, echo = FALSE}\n#left column\n','knitr::kable(',
                    table_vec,',format="latex",row.names = FALSE)',
                    '\n```\n\n:::\n\n::: {}\n\n```{r,  echo = FALSE}\n#right column\n',
                    'htmlwidgets::saveWidget(\n',plot_vec,', "myPivot.html", selfcontained = T)\n',
                    'webshot2::webshot(url = "myPivot.html", file = "myPivot.png")',
                    '\n```\n\n::: \n\n::::\n\n')
      
      
      write(c(top_block_yaml, body), file = paste0(output_path, "univariate_report",".Rmd"))
      # render(paste0(output_path, "univariate_report",".Rmd"))
      browseURL(render(paste0(output_path, "univariate_report",".Rmd")))
      
      #Remove temp files
      file.remove(paste0(output_path,"myPivot.png"))
      file.remove(paste0(output_path,"myPivot.html"))
      file.remove(paste0(output_path,univariate_list_name))
      file.remove(paste0(output_path,"univariate_report.tex"))
      
    }    else if (tolower(filetype_out) %in% c("rmd","html")) {
      univariate_list_name <- paste0('univariate-list-object_',Sys.Date(), '.RDS')
      saveRDS(univariate_list_object, file = paste0(output_path, univariate_list_name))
      
         top_block_yaml <- c('---',
                          'title: "Univariate Analysis Report"',
                          # paste0('subtitle: "Data Source: ', df_name, '"', collapse = ""),
                          paste0('author: "', Sys.info()[["user"]],'"'),
                          paste0('date: "', format(Sys.Date(), '%m/%d/%Y'), '"'),
                          'output: html_document',
                          '---',
                          '',
                          '```{r setup, include=FALSE}',
                          'knitr::opts_chunk$set(echo = TRUE,warning=FALSE, message=FALSE)',
                          '',
                          'library("tidyverse")',
                          'library("grid")',
                          'library("gridExtra")',
                          'library("ggpubr")',
                          'library("knitr")',
                          'library("plotly")',
                          'library(kableExtra)',
                          '',
                          if (flag_path == "no") {paste0('univariate_list_object <- readRDS(file = ".', output_path,
                                                  univariate_list_name, '")')} else
                                      {paste0('univariate_list_object <- readRDS(file = "', output_path,
                                                                 univariate_list_name, '")')},
                          '```\n')
      
      name_vec <- univariate_list_object$predictor_names
      table_vec <- paste('univariate_list_object$df$', name_vec, sep = '')
      plot_vec <- paste('univariate_list_object$plots$', name_vec, sep = '')
      
      body <- paste('#', name_vec,
                    '\n \n```{r, echo = FALSE}\n#left column\n','knitr::kable(',
                    table_vec,',row.names = FALSE)',  '%>%
                      kable_styling(bootstrap_options = c("striped", "hover"))',
                    '\n```\n\n','\n\n```{r,  echo = FALSE}\n#right column\n', plot_vec,'\n```\n\n')

      
      write(c(top_block_yaml, body), file = paste0(output_path, "univariate_report",".Rmd"))
      # render(paste0(output_path, "univariate_report",".Rmd"))
      browseURL(render(paste0(output_path, "univariate_report",".Rmd")))
      
      #Remove temp files
      file.remove(paste0(output_path,univariate_list_name))
     
      
    }  else if (tolower(filetype_out) %in% c("rds")) {
      saveRDS(univariate_list_object, file = paste0(output_path,"univariate_report.Rds"))}    else {
      stop("Please specify valid output format! (One of list, rmd, rds, html or pdf)")
    }
  }



output <- univariate_fun("/stats/projects/all/R_Tools_Development/data/salaries_data.Rds", 
                         exclude_vars = "patient_id", outcome = "salary")

output <- univariate_fun("/stats/projects/all/R_Tools_Development/data/salaries_data.Rds", 
                         exclude_vars = "patient_id", outcome = "salary",
                          filetype_out = "htmL", output_path = "./data/")

output <- univariate_fun("/stats/projects/all/R_Tools_Development/data/salaries_data.Rds", 
                         exclude_vars = "patient_id", outcome = "salary",
                          filetype_out = "rds", output_path = "./data/")

output <- univariate_fun("/stats/projects/all/R_Tools_Development/data/salaries_data.Rds", 
                         exclude_vars = "patient_id", outcome = "salary",
                          filetype_out = "pdf", output_path = "./data/")

output <- univariate_fun("/stats/projects/all/R_Tools_Development/data/salaries_data.Rds", 
                         exclude_vars = "patient_id", outcome = "salary",
                          filetype_out = "list", output_path = "./data/")


