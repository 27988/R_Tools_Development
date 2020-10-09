#' Conduct data exploration on individual variables
#'
#' @param df Input dataset; function accepts data.frame objects, as well as direct inputs of .rda/.RData, .rds, .csv, .xls, and .xlsx files
#' @param facet_var Optional character string of a grouping variable or outcome variable to compare univariate results; defaults to pure univariate analysis
#' @param exclude_vars Optional character string or vector of character strings specifying variables that should be excluded from analysis, I.E. identifiers, record version indicators, etc.
#' @param plot Should plots be included in output? Will most likely be removed to force plot inclusion. Defaults to false
#' @param output_type Type of output. Defaults to "rmd," other valid options are "pdf" or "list" / "object"
#' @param output_path Directory to save output. Default is the current working directory
#' @return 
#' @examples
#' univariate_fun(df = salaries_data, facet_var = "salary", exclude_vars = "patient_id, plot = TRUE, save = TRUE)
univariate_fun <- function(df, facet_var = NULL, exclude_vars = NULL, plot = FALSE, output_type = "rmd",
                           output_path) {
  require(tidyverse)
  require(grid)
  require(gridExtra)
  #defining internal functions
  
  facet_var_parser <- function(df, facet_var_var) {
    if(!stringr::str_detect(paste0(names(df), collapse = "|"), facet_var)) {
      stop(sprintf("Variable %s not found in dataset %s", facet_var, deparse(substitute(df))))
    } else {return(TRUE)}
  }
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
  df_prep_fun <- function(df, facet_var = NULL) {
    colnames(df) <- str_replace_all(colnames(df), pattern = "\\.", replacement = "\\_")
    facet_var_var <- select(df, facet_var)
    df %>%
      select(-all_of(facet_var)) %>%          # if facet_var is unspecified, this will still run 
      mutate_if(Negate(is.numeric),            #   without removing anything
                .funs = ~ as.factor(x = .)) %>% # converting non-numeric variables to factors
      ## 7/17 note - 
      # buckets - maybe 5, maybe user inputs#
      mutate_if(is.numeric, ~ cut_number(x = ., n = 5) %>% 
                  as.factor(.)) %>%           
      bind_cols(facet_var_var, .) %>%
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
  }
  else if (class(df) == "data.frame") {
    df <- df
  }
  else stop("input must be data.frame, R object, or delimited file!")
  
  exclude_parser(df = df, exclude_vars = exclude_vars)
  #running univariate analysis 
  df <- df_prep_fun(df, facet_var = facet_var)
  
  # if facet_var is null, function will just conduct exploratory analysis on all variables in df 
  #  not specified in exclude_vars. If an facet_var is specified, below code will run...
  if(!is.null(facet_var)){
    if(facet_var_parser(df = df, facet_var = facet_var)) {
      df_facet_var <- df %>%
        select(facet_var)
      
      df <- df %>%
        select(-all_of(exclude_vars), -all_of(facet_var))
      
      if(class(df_facet_var[[facet_var]]) == "factor"){
        facet_var_dist <- df %>%
          lapply(., function(x) {
            table(x, df_facet_var[[facet_var]]) 
          }) %>%
          lapply(., as.data.frame) %>%
          do.call(rbind, .) %>%
          rownames_to_column(., var = "variable") %>%
          rename("level" = x) %>%
          dplyr::rowwise(.) %>%
          mutate(variable = str_split(variable, pattern = "\\.", simplify = T)[[1]]) %>%
          group_by(variable, level) %>%
          mutate(Freq = round(Freq/sum(Freq), 3)) %>%
          mutate(Freq = paste(Var2, Freq, sep = ":")) %>%
          pivot_wider(names_from = Var2, names_prefix = "freq_levels", values_from = Freq) %>%
          unite(col = facet_var_dist, contains("freq_levels"), sep = " | ")
        
        df_out <- df %>%
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
          select(-total) %>%
          left_join(facet_var_dist, by = c("variable", "level")) 
        if(plot == TRUE) {
          predictor_names <- colnames(df)
          df <- df %>%
            bind_cols(df_facet_var)
          
          plots_count <- lapply(predictor_names, function(x) 
          {
            df %>%
              ggplot(data = ., aes(x = .data[[x]], fill = .data[[facet_var]])) +
              geom_bar(position = "dodge")
          })
          plots_stack <- lapply(predictor_names, function(y) {
            df %>%
              ggplot(data = ., aes(x = .data[[y]], fill = .data[[facet_var]])) +
              geom_bar(position = "fill")
          })
          names(plots_count) <- predictor_names
          names(plots_stack) <- predictor_names
          plot_list <- c(plots_count, plots_stack)
          plot_list <- plot_list[order(names(plot_list))]
          
          return(list(summary_tbl = df_out, plots = plot_list))
        }
        else { return(df_out) }
      }
      else if (class(df_facet_var[[facet_var]]) %in% c("numeric", "integer", "double")) {
        facet_var_dist <- lapply(colnames(df), function(x)
        {
          df %>%
            bind_cols(df_facet_var) %>%
            group_by_(x) %>%
            summarize(facet_var_mean = round(mean(.data[[facet_var]], na.rm = TRUE), 3)) %>%
            rename(level = 1) %>%
            mutate(variable = x) %>%
            select(variable, level, facet_var_mean)
        }) %>%
          lapply(as.data.frame) %>%
          do.call(rbind, .) 
        
        df_out <- df %>%
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
          select(-total) %>%
          left_join(facet_var_dist, by = c("variable", "level")) 
        # return(df_out)
      }
    }
  }
  else {
    message('no facet_varing variable specified - producing descriptives for each variable')
    df_out <- df %>% 
      select(-all_of(exclude_vars)) %>%
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
    
    df_out <- split(df_out, df_out$variable)
    
    predictor_names <- df %>%
      select(-all_of(exclude_vars)) %>%
      colnames(.)
    
    plots <- lapply(predictor_names, function(x) 
    {
      df_temp <- df_out[[x]]
      # ggplot(data = ., aes(x = .data[[x]]$level, y = .data[[x]]$prop)) +
      plot_temp <- ggpubr::ggdotchart(data = df_temp,
                                      x = "level", y = "prop", add = "segments",
                                      add.params = list(color = "lightgray", size = 2),
                                      dot.size = 4,
                                      ggtheme = theme_bw()) +
        ggpubr::font("x.text", size = 8, vjust = 0.9, angle = 45)
      
      plot_temp <- ggpubr::ggpar(p = plot_temp, ylim = c(0, 1))
      
      # if(mean(nchar(unique(as.character(df_out[[x]]$level)))) > 10) {
      #   plot_temp <- plot_temp + 
      #     theme(axis.text.x = element_text(angle = 45))
      # } else plot_temp <- plot_temp
      return(plot_temp)
    })
    
    if(missing(output_path)) {
      output_path <- paste0(getwd(), "/output/")
    } else output_path <- output_path
    
    if(output_type %in% c("list", "l", "li", "lis")) {
      output_list <- list(summary_tbl = df_out,
                          summary_plots = plots)
      return(output_list)
    }
    else if(output_type %in% c("p", "pd", "pdf")) 
    {
      
      names(plots) <- predictor_names
      
      grid_layout <- rbind(c(1, 1, 2, 2),
                           c(3, 3, 4, 4),
                           c(3, 3, 4, 4),
                           c(3, 3, 4, 4))
      
      output_list <- purrr::pmap(
        .l = list(
          df_out[predictor_names], 
          predictor_names, 
          plots), 
        .f = ~ list(
          var_name = grid::textGrob(label = str_replace(..2, "^.{1}", toupper)), 
          empty = grid::textGrob(label = ""),
          summary_tbl = gridExtra::tableGrob(..1, rows = NULL), 
          var_plot = ..3)) %>%
        map(., ~ gridExtra::arrangeGrob(grobs = ., layout_matrix = grid_layout, 
                                        ncol = 2, nrow = 2))
      
      output_list <- marrangeGrob(output_list, ncol = 1, nrow = 3, 
                                  vp = grid::viewport(width = 0.90, height = 0.90))
      cowplot::ggsave2(plot = output_list, 
                       filename = paste0(output_path, "univariate-summary_", Sys.Date(), ".pdf"), 
                       width = 8.5, height = 11, units = "in")
    }
    else if (output_type %in% c("r", "rm", "rmd")) {
      # generating path for RDS data to be saved, based on output path
      if(missing(output_path)) {
        data_path <- paste0(getwd(), "/data/")
      } else {
        data_path <- paste0(dirname(output_path), '/data/')
      }
      
      univariate_list_object <- list(
        predictor_names = predictor_names,
        df = df_out[predictor_names],
        plots = plots)
      names(univariate_list_object$plots) <- predictor_names
      univariate_list_name <- paste0('univariate-list-object_', Sys.Date(), '.RDS')
      
      
      saveRDS(univariate_list_object, file = paste0(data_path, univariate_list_name))
      
      top_block_yaml <- c('---',
                          'title: "Univariate Analysis Report"',
                          # paste0('subtitle: "Data Source: ', df_name, '"', collapse = ""),
                          paste0('author: "', Sys.info()[["user"]],'"'),
                          paste0('date: "', format(Sys.Date(), '%m/%d/%Y'), '"'),
                          'output: html_document',
                          '---',
                          '',
                          '```{r setup, include=FALSE}',
                          'knitr::opts_chunk$set(echo = TRUE)',
                          '',
                          'library("tidyverse")',
                          'library("grid")',
                          'library("gridExtra")',
                          'library("ggpubr")',
                          '',
                          paste0('univariate_list_object <- readRDS(file = ".', data_path,
                                 univariate_list_name, '")'),
                          '```\n')
      
      
      # names_vec <- c("rank", "discipline", "yrs_since_phd", "yrs_service", "sex", "salary")
      
      name_vec <- univariate_list_object$predictor_names
      table_vec <- paste('univariate_list_object$df$', name_vec, sep = '')
      plot_vec <- paste('univariate_list_object$plots$', name_vec, sep = '')
      
      body <- paste('#', name_vec, '\n\n:::: {style="display: flex;"} \n\n::: {} \n\n', 
                    '\n \n```{r}\n#left column\n',
                    table_vec,
                    '\n```\n\n:::\n\n::: {}\n\n```{r}\n#right column\n', plot_vec, 
                    '\n```\n\n::: \n\n::::\n\n')
      
      
      write(c(top_block_yaml, body), file = paste0(output_path, "zzzzzzzzzz.Rmd"))
    }
    else {
      stop("Please specify valid output format! (One of list, rmd, or pdf)")
    }
  }
}

# function produces error when facet_var has period in name, need to address
output <- univariate_fun("./data/salaries_data.Rds", 
                         exclude_vars = "patient_id", 
                         plot = TRUE, output_type = "rmd", output_path = "./output/")


