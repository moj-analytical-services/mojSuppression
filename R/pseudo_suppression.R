# these functions allow the code to look at rows/columns that you don't actively want to suppress on, but that if their values fall
# below a suppression threshold could lead to a user being able to make out values in alternate columns

# suppress on rows where we have a pseudo suppression flag (i.e. a column which has values that give away a value...)
# this function should typically be used in tandem with other suppression functions, otherwise you'll encouter issues with requiring
# two differing dfs (your main df to suppress and a subset df to apply the suppression to)
pseudo_suppress_columns <- function(df, # the original df to feed into the system
                                    subset_df, # this is the df we're actually suppressing on. 
                                    row_nos_to_suppress = NULL,
                                    columns_to_pseudo_suppress, # columns we want to "pseudo suppress" on
                                    pseudo_suppression_threshold, # the suppression threshold for values (anything less than or equal to this value is )
                                    pseudo_suppress_0 = TRUE, # specify whether 0s should be suppressed or not
                                    secondary_suppress_0 = TRUE # set to FALSE to prevent 0s from taking precendence for suppression (though will be suppressed if it's the only option)
) {
  
  # remove any flags that fall outside of our intended range
  if(length(row_nos_to_suppress) != 0) { 
    df <- df[row_nos_to_suppress,]
  }
  
  # find rows to suppress looking at whether our pseudo suppression cols raise any flags
  if(pseudo_suppress_0) {
    rows_to_suppress <- columns_to_pseudo_suppress %>% 
      purrr::map(~which(pull(df, .x)<=pseudo_suppression_threshold)) %>% 
      unlist() %>% 
      unique()
  } else {
    rows_to_suppress <- columns_to_pseudo_suppress %>% 
      purrr::map(~which(pull(df, .x)<=pseudo_suppression_threshold & pull(df, .x)>0)) %>% 
      unlist() %>% 
      unique()
  }
  
  
  
  # loop around our rows to suppress on and apply suppression markings to the smallest values
  for(row_no in rows_to_suppress) {
    
    # only run the code below if the row total > 0
    row_total <- sum(subset_df[row_no,])
    if(row_total > 0) {
      # replace 0s if we don't want to secondary suppress on them
      if(!secondary_suppress_0) {
        subset_df[subset_df == 0] <- 3333333
      }
      
      col_to_redact <- which(subset_df[row_no,] == min(subset_df[row_no,], na.rm = TRUE))
      set.seed(10) # set seed so we always sample the same vals (i.e. always suppress the same min value)
      # probably makes sense to convert this to an alternate measure in the future
      col_to_redact <- ifelse(length(col_to_redact) != 1, sample(col_to_redact, 1), col_to_redact) 
      
      subset_df[row_no,col_to_redact] <- 9999999
      
      # replace 0s if we don't want to secondary suppress on them
      if(!secondary_suppress_0) {
        subset_df[subset_df == 3333333] <- 0
      }
    }
    
  }
  
  
  return(subset_df)
}

# despite its name, this function relates to the pseudo_suppress_rows function
# in the function below, we identify column flags (i.e. column names which require suppression) to pseudo suppress on using row indexes
find_cols_to_pseudo_suppress <- function(df, # df to pseudo suppress on
                                         rows_to_pseudo_suppress, # a list of rows you want to pseudo suppress across
                                         suppression_threshold, # suppression threshold
                                         pseudo_suppress_0 = TRUE # specify whether 0 should be suppressed or not
) { 
  
  find_single_row_data <- function(df,
                                   row_index,
                                   suppression_threshold) {
    # find info for specified row
    row_data <- df[row_index,colnames(df %>% 
                                        dplyr::select_if(is.numeric))] %>% 
      unlist()
    # pull out our column names that require suppression
    if(pseudo_suppress_0) { 
      column_suppression_index <- names(which(row_data <= suppression_threshold)) # filter for values falling below our threshold
    } else {
      column_suppression_index <- names(which(row_data <= suppression_threshold & row_data > 0)) # filter for values falling below our threshold
    }
    
    return(column_suppression_index)
    
  }
  # loop around all of the row inputs and create our column flags
  column_flags <- rows_to_pseudo_suppress %>%
    purrr::map(~find_single_row_data(df,
                                     row_index = .x,
                                     suppression_threshold = suppression_threshold)) %>%
    unlist() %>%
    unique()
  
  return(column_flags)
}


# second function to follow along from the one above. This does the legwork and applies suppression to the required columns
pseudo_suppress_rows <- function(df, # df to pseudo suppress on
                                 subset_df, # our subset df which we will apply suppression to
                                 rows_to_pseudo_suppress, # a list of rows you want to pseudo suppress across
                                 cols_to_suppress = NULL, # specify the columns we're including in our subset_df. NULL by default if no filter is done
                                 pseudo_suppression_threshold,
                                 pseudo_suppress_0 = TRUE) {
  # flags to determine which columns require suppression
  column_flags <- find_cols_to_pseudo_suppress(df,
                                               rows_to_pseudo_suppress,
                                               pseudo_suppression_threshold,
                                               pseudo_suppress_0)
  
  # remove any columns_flags not found in our subset_df
  column_flags <- column_flags[column_flags %in% cols_to_suppress]
  
  for(col_name in column_flags) {
    row_to_redact <- which(subset_df[[col_name]] == min(subset_df[[col_name]], na.rm = TRUE))
    set.seed(10) # set seed so we always sample the same vals (i.e. always suppress the same min value)
    row_to_redact <- ifelse(length(row_to_redact) != 1, sample(row_to_redact, 1), row_to_redact) # randomly select one of our suppression entries (so we don't suppress more than is necessary)
    
    subset_df[row_to_redact,col_name] <- 9999999
  }
  
  
  return(subset_df)
}
