require(tidyverse)


outcome_parser <- function(df, outcome) {
  if(!stringr::str_detect(paste0(names(df), collapse = "|"), outcome)) {
    stop(sprintf("Variable %s not found in dataset %s", outcome, deparse(substitute(df))))
  } 
  else if (!(class(df[[outcome]]) %in% c("factor", "numeric", "integer", "double"))) {
    stop("Outcome variable is not a valid datatype!")
  } else { return(TRUE) }
}
# preparing df by converting character variables to factors and 
# numeric variables into discrete factors

df_prep_fun <- function(df, outcome = NULL) {
  outcome_var <- select(df, outcome)
  df %>%
    select(-all_of(outcome)) %>%             # if outcome is unspecified, this will still run 
                                              # without removing anything
    mutate_if(Negate(is.numeric), 
              .funs = ~ as.factor(x = .)) %>% # converting non-numeric variables to factors
    mutate_if(is.numeric, ~ cut_number(x = ., n = ceiling(length(.)/10)) %>% 
                as.factor(.)) %>%           # converting numeric variables to factors - with the
                                            # number of quantiles being determined by the length
                                            # of the input variable
    bind_cols(outcome_var, .) %>%
    return(.)
}

univariate_fun <- function(df, outcome = NULL, exclude_vars = NULL, plot = FALSE) {
  df <- df_prep_fun(df, outcome = outcome)
  # if outcome is null, function will just conduct exploratory analysis on all variables  
  # not called in exclude_vars. If an outcome is specified, extra calculations will be run
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
      }
      else {
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
      }
      df %>%
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
        left_join(outcome_dist, by = c("variable", "level")) %>%
        return(.)
    }
  }
  else {
    message('no outcome specified - producing descriptives for each variable')
    df %>% 
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
      select(-total) %>%
      return(.)
  }
}
