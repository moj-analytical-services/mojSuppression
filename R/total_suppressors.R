
# both of these suppressors work in the same way - prevent the suppression total (total when you add up values in a col or row that have been suppressed) 
# falling below or equal to our suppression threshold. These functions are part of an optional argument in our final suppression function
suppress_col_total_below_supp_thres <- function(
  suppressed_df, # version of your df that's already had suppression applied
  unsuppressed_df, # original unsuppressed version
  suppression_threshold,
  cols_to_suppress = NULL, # set cols to suppress on
  rows_to_suppress = NULL, # set rows to suppress on
  subset_df_along_row_and_col_arguments,
  priority_rows_to_suppress = NULL, # set priority rows to suppress on
  secondary_suppress_0,
  running_total_supp = FALSE
) {
  
  # replace 0s if we don't want to secondary suppress on them
  if(!secondary_suppress_0) {
    suppressed_df[suppressed_df == 0] <- 3333333
  }
  
  # initially, subset df if required (so we only suppress across selected row/col combos)
  if(subset_df_along_row_and_col_arguments) {
    if(!is.null(cols_to_suppress)) { # subset rows only if we're working with rows
      unsuppressed_df <- unsuppressed_df[,cols_to_suppress] # subset cols (keep only numeric cols...)
    }
    if(!is.null(rows_to_suppress)) { # subset rows only if we're working with rows
      unsuppressed_df <- unsuppressed_df[rows_to_suppress,]
    }
  }
  
  suppression_index <- which(as.matrix(suppressed_df) == 9999999, arr.ind=TRUE)
  ### COL VERSION OF OUR NEW SUPPRESSION METHOD
  # find our suppression totals by column
  # i.e. find the total value for each value suppressed in a given column
  suppression_total_list <- unique(suppression_index[,2]) %>% 
    purrr::map_df(~dplyr::tibble(total = sum(unsuppressed_df[as.data.frame(suppression_index) %>% 
                                                        dplyr::filter(col == .x) %>% 
                                                        dplyr::pull(row), .x] %>% 
                                        unlist(), na.rm = TRUE),
                          col = .x))
  
  # also find the frequency columns appear in our suppression summary data and filter any with more than
  # two suppression nodes. These are not put up for consideration for total suppression.
  # or where suppression nodes are less than 2 (in the special case where ncol > 2)
  frequency_of_cols <- plyr::count(suppression_index[,2]) %>% 
    dplyr::filter(freq > 4 | freq < 2)
  if(nrow(frequency_of_cols) > 0){
    suppression_total_list <- suppression_total_list %>% 
      dplyr::filter(!col %in% c(frequency_of_cols %>% 
                                  dplyr::pull(x)))
  }
  
  # save our suppression_list to min_supp_total list. Where we have values to suppress, also add a quick filter
  if(nrow(suppression_total_list) > 0) {
    min_suppression_total_list <- suppression_total_list %>% 
      dplyr::filter(total <= 2)
  } else {
    min_suppression_total_list <- suppression_total_list
  }
  
  # check whether we can use priority suppression here
  # if all of our active rows have 0s and the user has specified that they don't want 0 suppression
  # allow all rows to be suppressed
  if(!is.null(priority_rows_to_suppress) & !secondary_suppress_0 & nrow(suppression_total_list)>0) {
    cols_to_use <- suppression_total_list[['col']]
    # filter our data for only row/col combinations with potential 0s
    dummy_df <- suppressed_df[priority_rows_to_suppress, cols_to_use] %>% 
      dplyr::filter_all(~.x != 9999999)
    # check if any rows have less than 50% of cells occupied by 0s
    perc_0 <- 1:nrow(dummy_df) %>% 
      purrr::map_dbl(
        ~sum(unlist(dummy_df[.x,]) == 3333333)/length(cols_to_use)
      )
    # check if we want to remove our priority supp
    if(all(perc_0 >= 0.5)) {
      priority_rows_to_suppress <- NULL
    }
    
  }
  
  # check whether we can use priority suppression here
  # if all of our active rows have 0s and the user has specified that they don't want 0 suppression
  # allow all rows to be suppressed
  if(!is.null(priority_rows_to_suppress) & !secondary_suppress_0 & nrow(suppression_total_list)>0) {
    cols_to_use <- suppression_total_list[['col']]
    # filter our data for only row/col combinations with potential 0s
    dummy_df <- suppressed_df[priority_rows_to_suppress, cols_to_use] %>% 
      dplyr::filter_all(~.x != 9999999)
    # check if any rows have less than 50% of cells occupied by 0s
    perc_0 <- 1:nrow(dummy_df) %>% 
      purrr::map_dbl(
        ~sum(unlist(dummy_df[.x,]) == 3333333)/length(cols_to_use)
      )
    # check if we want to remove our priority supp
    if(all(perc_0 >= 0.5)) {
      priority_rows_to_suppress <- NULL
    }
    
  }
  
  # if we are applying priority suppression, trim our matrix down and create our row_index
  if(!is.null(priority_rows_to_suppress)) {
    suppressed_matrix <- as.matrix(suppressed_df[priority_rows_to_suppress,])
    row_index <- priority_rows_to_suppress
  } else {
    suppressed_matrix <- as.matrix(suppressed_df)
    row_index <- 1:nrow(suppressed_df)
  }
  
  
  if(running_total_supp & nrow(min_suppression_total_list)==1) {
    supp_col <- min_suppression_total_list[['col']]
    
    # find lowest value and suppress there (if there are multiple, just sample for simplicity)
    # if we have the time at a later date, look for the lowest min couplet
    x <- suppressed_matrix[,supp_col]
    row_to_redact <- which(x == min(x[!is.infinite(x)], na.rm = TRUE))
    set.seed(10) # set seed so we always sample the same vals
    row_to_redact <- ifelse(length(row_to_redact) != 1, sample(row_to_redact, 1), row_to_redact) 
    row_to_redact <- row_index[row_to_redact]
    
    # apply suppression
    suppressed_df[row_to_redact, supp_col] <- 9999999
    
    
  } else if(nrow(min_suppression_total_list) == 1) { # i.e. only one row requires attention
    # pull out our main col
    main_col_to_supp <- min_suppression_total_list %>% 
      dplyr::pull(col)
    # pull out our additional columns with suppression
    sub_cols_to_supp <- suppression_total_list %>% 
      dplyr::pull(col)
    # remove our main col
    sub_cols_to_supp <- sub_cols_to_supp[!sub_cols_to_supp==main_col_to_supp]
    
    min_value_pairs_list <- list(min_value_pairs = 11111111111111)
    for(i in sub_cols_to_supp) {
      
      # pull out our required columns
      matrix_to_use <- suppressed_matrix[,c(main_col_to_supp, i)]
      # find our min value pairs (name correspondings to the column in cross_df_matrix. Value corresponds to the total it has found to be the min)
      min_value_pairs <- find_min_value_pair(matrix_to_use, 2)
      
      # check whether our min value is the lowest so far
      if(min_value_pairs < min_value_pairs_list$min_value_pairs) {
        # following that, find our co-ords for our min value pair
        final_cols_to_supp <- colnames(matrix_to_use)
        final_row_to_supp <- as.numeric(names(min_value_pairs))
        # if not null, use our final_row value to guide where our suppression should occur in our priority rows
        if(!is.null(priority_rows_to_suppress)) { 
          final_row_to_supp <- priority_rows_to_suppress[final_row_to_supp]
        }
        
        # add this output to our list
        min_value_pairs_list <- list(
          min_value_pairs = min_value_pairs,
          final_cols_to_supp = final_cols_to_supp,
          final_row_to_supp = final_row_to_supp
        )
      }
      
    }
    
    # using the above list, suppress our minimum values
    # suppress the rows/cols found above. This is done using a "min-value pairs" approach (i.e. take a look at minimum value pairings across rows and pick the pairs that grant the lowest total collectively)
    # this helps to improve our suppression selection
    suppressed_df[min_value_pairs_list$final_row_to_supp, min_value_pairs_list$final_cols_to_supp] <- 9999999
    
  } else if(nrow(min_suppression_total_list) > 1) {
    
    # pull out our main col
    main_cols_to_supp <- min_suppression_total_list %>% 
      dplyr::pull(col)
    
    while(length(main_cols_to_supp)>0){
      
      # we shouldn't have any values where len(cols_requiring_supp)==1, so just check for whether the value is odd or even
      # if even, ony look for a min-value pair of length 2
      # otherwise, look for one with a min value of 3
      value_pair_len <- 2
      
      # pull out our required columns
      matrix_to_use <- suppressed_matrix[,main_cols_to_supp]
      # find our min value pairs (name correspondings to the column in cross_df_matrix. Value corresponds to the total it has found to be the min)
      min_value_pairs <- find_min_value_pair(matrix_to_use, value_pair_len)
      
      # check whether our min value is the lowest so far
      # following that, find our co-ords for our min value pair
      final_cols_to_supp <- names(sort(matrix_to_use[as.numeric(names(min_value_pairs)),]) %>% 
                                    utils::head(value_pair_len))
      final_row_to_supp <- as.numeric(names(min_value_pairs))
      
      if(!is.null(priority_rows_to_suppress)) { # if not null, use our final_row value to guide where our suppression should occur in our priority rows
        final_row_to_supp <- priority_rows_to_suppress[final_row_to_supp]
      }
      
      # using the above list, suppress our minimum values
      # suppress the rows/cols found above. This is done using a "min-value pairs" approach (i.e. take a look at minimum value pairings across rows and pick the pairs that grant the lowest total collectively)
      # this helps to improve our suppression selection
      suppressed_df[final_row_to_supp, final_cols_to_supp] <- 9999999
      
      # remove new columns that have had suppression applied
      main_cols_to_supp <- main_cols_to_supp[!colnames(matrix_to_use) %in% final_cols_to_supp]
    }
  }
  
  # replace 0s if we don't want to secondary suppress on them
  if(!secondary_suppress_0) {
    suppressed_df[suppressed_df == 3333333] <- 0
  }  
  return(suppressed_df)
  
}

# transpose our suppression df and apply suppress_col_total_below_supp_thres
suppress_row_total_below_supp_thres <- function(
  df_to_suppress,
  unsuppressed_df,
  cols_to_suppress,
  rows_to_suppress,
  suppression_threshold,
  subset_df_along_row_and_col_arguments,
  secondary_suppress_0,
  running_total_supp = FALSE
) {
  
  # create our transposed dfs
  transposed_df <- dplyr::as_tibble(t(df_to_suppress))
  
  # initially, subset df if required (so we only suppress across selected row/col combos)
  if(subset_df_along_row_and_col_arguments) {
    if(!is.null(cols_to_suppress)) { # subset rows only if we're working with rows
      unsuppressed_df <- unsuppressed_df[,cols_to_suppress] # subset cols (keep only numeric cols...)
    }
    if(!is.null(rows_to_suppress)) { # subset rows only if we're working with rows
      unsuppressed_df <- unsuppressed_df[rows_to_suppress,]
    }
  }
  # transpose our unsuppressed df
  transposed_unsupp_df <- dplyr::as_tibble(t(unsuppressed_df))
  
  # apply column suppression to "rows"
  transposed_df <- suppress_col_total_below_supp_thres(
    suppressed_df = transposed_df, # version of your df that's already had suppression applied
    unsuppressed_df = transposed_unsupp_df, # original unsuppressed version
    suppression_threshold = suppression_threshold,
    cols_to_suppress = NULL, # set cols to suppress on
    rows_to_suppress = NULL, # set rows to suppress on
    subset_df_along_row_and_col_arguments = FALSE,
    priority_rows_to_suppress = NULL, # set priority rows to suppress on
    secondary_suppress_0,
    running_total_supp = FALSE
  )
  
  # convert back to our original form
  column_names_df_to_supp <- colnames(df_to_suppress)
  df_to_suppress <- dplyr::as_tibble(t(transposed_df))
  # reset colnames
  colnames(df_to_suppress) <- column_names_df_to_supp
  
  return(df_to_suppress)
  
}