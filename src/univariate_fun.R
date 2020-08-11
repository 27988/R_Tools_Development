#' Conduct data exploration on individual variables
#'
#' @param df
#' @param outcome
#' @param exclude_vars
#' @param plot
#' @return ...
#' @examples
#' univariate_fun(df = salaries_data, outcome = "salary", exclude_vars = "patient_id, plot = TRUE)
univariate_fun <- function(df, outcome = NULL, exclude_vars = NULL, plot = FALSE) {
  require(tidyverse)
  #defining internal functions
  
  outcome_parser <- function(df, outcome) {
    if(!stringr::str_detect(paste0(names(df), collapse = "|"), outcome)) {
      stop(sprintf("Variable %s not found in dataset %s", outcome, deparse(substitute(df))))
    } else {return(TRUE)}
  }
  #preparing df by converting character variables to factors and numeric variables into discrete factors
  df_prep_fun <- function(df, outcome = NULL) {
    colnames(df) <- str_replace_all(colnames(df), pattern = "\\.", replacement = "\\_")
    outcome_var <- select(df, outcome)
    df %>%
      select(-all_of(outcome)) %>%             #if outcome is unspecified, this will still run                                                     without removing anything
      mutate_if(Negate(is.numeric), 
                .funs = ~ as.factor(x = .)) %>% #converting non-numeric variables to factors
      ## 7/17 note - 
      # buckets - maybe 5, maybe user inputs#
      mutate_if(is.numeric, ~ cut_number(x = ., n = 5) %>% 
                  as.factor(.)) %>%           
      bind_cols(outcome_var, .) %>%
      return(.)
  }
  
  #running univariate analysis 
  df <- df_prep_fun(df, outcome = outcome)
  
  # if outcome is null, function will just conduct exploratory analysis on all variables in df 
  #  not specified in exclude_vars. If an outcome is specified, below code will run...
  if(!is.null(outcome)){
    if(outcome_parser(df = df, outcome = outcome)) {
      df_outcome <- df %>%
        select(outcome)
      
      df <- df %>%
        select(-all_of(exclude_vars), -all_of(outcome))
      
      if(class(df_outcome[[outcome]]) == "factor"){
        outcome_dist <- df %>%
          lapply(., function(x) {
            table(x, df_outcome[[outcome]]) 
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
          unite(col = outcome_dist, contains("freq_levels"), sep = " | ")
        
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
          left_join(outcome_dist, by = c("variable", "level")) 
        if(plot == TRUE) {
          predictor_names <- colnames(df)
          df <- df %>%
            bind_cols(df_outcome)
          
          plots_count <- lapply(predictor_names, function(x) 
          {
            df %>%
              ggplot(data = ., aes(x = .data[[x]], fill = .data[[outcome]])) +
              geom_bar(position = "dodge")
          })
          plots_stack <- lapply(predictor_names, function(y) {
            df %>%
              ggplot(data = ., aes(x = .data[[y]], fill = .data[[outcome]])) +
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
      else if (class(df_outcome[[outcome]]) %in% c("numeric", "integer", "double")) {
        outcome_dist <- lapply(colnames(df), function(x)
        {
          df %>%
            bind_cols(df_outcome) %>%
            group_by_(x) %>%
            summarize(outcome_mean = round(mean(.data[[outcome]], na.rm = TRUE), 3)) %>%
            rename(level = 1) %>%
            mutate(variable = x) %>%
            select(variable, level, outcome_mean)
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
          left_join(outcome_dist, by = c("variable", "level")) 
        # return(df_out)
      }
    }
  }
  else {
    message('no outcome specified - producing descriptives for each variable')
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
    
    predictor_names <- df %>%
      select(-all_of(exclude_vars)) %>%
      colnames(.)
    
    plots <- lapply(predictor_names, function(x) 
    {
      df %>%
        ggplot(data = ., aes(x = .data[[x]])) +
        geom_bar(position = "dodge") 
    })
    return(list(summary_tbl = df_out, plots = plots))
  }
}

# function produces error when outcome has period in name, need to address that
univariate_fun(iris, outcome = "Sepal_Length", plot = T)
