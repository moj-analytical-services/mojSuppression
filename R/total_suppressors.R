# both of these suppressors work in the same way - prevent the suppression total (total when you add up values in a col or row that have been suppressed) 
# falling below or equal to our suppression threshold. These functions are part of an optional argument in our final suppression function
create_supp_total_list <- function(suppression_index,
                                   unsuppressed_df,
                                   suppression_threshold, 
                                   secondary_suppress_0,
                                   total_list_out = FALSE
) {
  
  # find our suppression totals by column
  # i.e. find the total value for each value suppressed in a given column
  suppression_total_list <- unique(suppression_index[,2]) %>% 
    purrr::map_df(~dplyr::tibble(total = sum(unsuppressed_df[as.data.frame(suppression_index) %>% 
                                                        dplyr::filter(col == .x) %>% 
                                                        dplyr::pull(row), .x] %>% 
                                        unlist(), na.rm = TRUE),
                          col = .x))
  
  if(total_list_out) {
    return(suppression_total_list)
  } else {
    # find the frequency columns appear in our suppression summary data and filter any with more than
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
        dplyr::filter(total <= suppression_threshold)
    } else {
      min_suppression_total_list <- suppression_total_list
    }
    
    return(min_suppression_total_list$col)
  }
}

duplicated_supp_nodes <- function(suppression_index, unsuppressed_df, suppression_thres) {
  # create flags for rows we need to apply secondary suppression to
  # columns are flagged where they contain primary suppression values of only 1 or our threshold
  # i.e. if our threshold is set to 5 and only 5s have been suppressed in a column, we will flag it
  # data.frame(5, 5, 5, 10) -- here, we'd suppress the 10 too, as our column only contains 5s
  suppression_checker <- dplyr::tibble()
  col_index  <-  unique(suppression_index[,2])
  for (c in col_index) {
    # extract the rows to check
    row_index <- suppression_index[,1][suppression_index[,2] == c]
    # pull out the corresponding suppressed values
    col_values <- unsuppressed_df[row_index,] %>% dplyr::pull({{c}})
    # check if all values in the cols are either 1s or our suppression threshold value
    suppression_checker <- dplyr::bind_rows(
      suppression_checker,
      dplyr::tibble(col = c, flag = any(c(1,suppression_thres) %>% purrr::map_lgl(~all(col_values == .x) && length(col_values)>1)))
    )
  }
  
  if(nrow(suppression_checker)>0) {
    # filter only for columns that need additional suppression...
    suppression_checker <- suppression_checker %>% 
      dplyr::filter(flag == TRUE)
    
    return(suppression_checker$col)
  } else {
    return(c())
  }
  
  
}


suppress_col_total_below_supp_thres <- function(
  suppressed_df, # version of your df that's already had suppression applied
  unsuppressed_df, # original unsuppressed version
  suppression_threshold,
  where_to_suppress=c("col", "row"), # "col" is the special case we need to be cautious of
  cols_to_suppress = NULL, # set cols to suppress on
  rows_to_suppress = NULL, # set rows to suppress on
  subset_df_along_row_and_col_arguments,
  priority_rows_to_suppress = NULL, # set priority rows to suppress on
  secondary_suppress_0,
  running_total_supp = FALSE,
  run_from_cross_ref = FALSE,
  total_suppression = FALSE,
  indirect_suppression = FALSE
) {
  # grab a snapshot of our suppression dataframe, so we can return it early
  # to the user if we do not need to apply any suppression at this step.
  raw_suppressed <- suppressed_df
  stored_colnames <- colnames(suppressed_df)
  
  # replace 0s if we don't want to secondary suppress on them
  if(!secondary_suppress_0) {
    suppressed_df[suppressed_df == 0] <- 33333333
  }
  
  # initially, subset df if required (so we only suppress across selected row/col combos)
  if(subset_df_along_row_and_col_arguments) {
    if (is.null(rows_to_suppress)) rows_to_suppress=1:nrow(unsuppressed_df)
    if (is.null(cols_to_suppress)) cols_to_suppress=colnames(unsuppressed_df)
    unsuppressed_df <- unsuppressed_df[rows_to_suppress,cols_to_suppress] # subset cols (keep only numeric cols...)
  }
  
  # if we are suppressing rows only, then transpose
  if(where_to_suppress=="row") {
    suppressed_df <- dplyr::as_tibble(t(suppressed_df))
    unsuppressed_df <- dplyr::as_tibble(t(unsuppressed_df))
  }
  
  # create our suppression index. This documents where our suppression nodes 
  # are currently located in our matrix
  suppression_index <- which(as.matrix(suppressed_df) == 9999999, arr.ind=TRUE)
  
  # create a breakdown of all current suppression nodes
  suppression_total_list <- create_supp_total_list(
    suppression_index,
    unsuppressed_df,
    suppression_threshold, 
    secondary_suppress_0,
    TRUE
  )
  
  cols_to_supp <- c()
  # create our total suppression breakdown
  if(total_suppression) {
    cols_to_supp <- append(cols_to_supp, 
                           create_supp_total_list(
                             suppression_index,
                             unsuppressed_df,
                             suppression_threshold, 
                             secondary_suppress_0
                           )
    )
  }
  # create our total suppression breakdown
  if(indirect_suppression) {
    cols_to_supp <- append(cols_to_supp,
                           duplicated_supp_nodes(
                             suppression_index = suppression_index, 
                             unsuppressed_df = unsuppressed_df, 
                             suppression_thres = suppression_threshold
                           )
    )
  }
  # find unique solutions
  cols_to_supp <- unique(cols_to_supp)
  
  # check whether we can use priority suppression here
  # if all of our active rows have 0s and the user has specified that they don't want 0 suppression
  # allow all rows to be suppressed
  if(!is.null(priority_rows_to_suppress) & !secondary_suppress_0 & length(cols_to_supp)>0) {
    # filter our data for only row/col combinations with potential 0s
    dummy_df <- suppressed_df[priority_rows_to_suppress, cols_to_supp] %>% 
      dplyr::filter_all(~.x != 9999999)
    # check if any rows have less than 50% of cells occupied by 0s
    perc_0 <- 1:nrow(dummy_df) %>% 
      purrr::map_dbl(
        ~sum(unlist(dummy_df[.x,]) == 33333333)/length(cols_to_supp)
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
  
  if(length(cols_to_supp)==0) return(list(raw_suppressed))
  # using the above, apply secondary suppression where required
  if((running_total_supp & length(cols_to_supp)==1) | 
     all(where_to_suppress=="col") | all(where_to_suppress=="row")
  ) {
    
    
    total_suppress_col <- function(matrix, df, col_to_supp, where_to_supp) {
      
      # find lowest value and suppress there (if there are multiple, 
      # just sample for simplicity).
      x <- matrix[,col_to_supp]
      row_to_redact <- which(x == min(x[!is.infinite(x)], na.rm = TRUE))
      supp_df_list <- row_to_redact %>%
        purrr::map(
          ~{
            # overwrite each potential value with our suppression node
            df[.x, col_to_supp] <- 9999999
            
            # replace 0s if we don't want to secondary suppress on them
            if(!secondary_suppress_0 & !run_from_cross_ref) {
              df[df == 33333333] <- 0
              df[df == 3333333] <- 0 # lazy solution...
            }
            # return supp df
            df
          }
        )
      set.seed(55)
      out <- supp_df_list[[sample(1:length(supp_df_list), 1)]]

      return(out)
      
    }
    
    looped_total_suppress_col <- function(
      matrix, df, cols_to_supp, where_to_suppress
    ) {
      # simply run the total_suppress_col in a loop to account for multiple
      # column inputs
      
      for(col in cols_to_supp) {
        df <- total_suppress_col(
          matrix,
          df,
          col, 
          where_to_suppress
        )
      }
      
      # transpose if required...
      if(where_to_suppress=="row") {
        df <- dplyr::as_tibble(t(df))
        colnames(df) <- stored_colnames
      }
      
      return(df)
    }
    
    # now apply our total suppression!
    if(running_total_supp) {
      out <- looped_total_suppress_col(
        suppressed_matrix,
        suppressed_df,
        cols_to_supp, 
        where_to_suppress
      )
      return(list(out))
    }
    
    suppressed_matrix[suppressed_matrix==33333333] <- 3333333
    suppressed_df[suppressed_df==33333333] <- 3333333
    # if row supp, convert transpose back!
    out <- looped_total_suppress_col(
      suppressed_matrix,
      suppressed_df,
      cols_to_supp, 
      where_to_suppress)
    return(list(out))
    
    
  } else if(length(cols_to_supp) == 1) { # i.e. only one row requires attention
    # pull out our main col
    main_col_to_supp <- cols_to_supp
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
    
  } else if(length(cols_to_supp) > 1) {
    
    while(length(cols_to_supp)>0){
      
      # we shouldn't have any values where len(cols_requiring_supp)==1, so just check for whether the value is odd or even
      # if even, ony look for a min-value pair of length 2
      # otherwise, look for one with a min value of 3
      value_pair_len <- 2
      
      # pull out our required columns
      matrix_to_use <- suppressed_matrix[,cols_to_supp]
      if(!is.matrix(matrix_to_use)) matrix_to_use <- matrix(matrix_to_use, nrow = 1)
      # find our min value pairs (name correspondings to the column in cross_df_matrix. Value corresponds to the total it has found to be the min)
      min_value_pairs <- find_min_value_pair(matrix_to_use, value_pair_len)
      
      # check whether our min value is the lowest so far
      # following that, find our co-ords for our min value pair
      final_cols_to_supp <- names(sort(matrix_to_use[as.numeric(names(min_value_pairs)),]) %>% 
                                    head(value_pair_len))
      final_row_to_supp <- as.numeric(names(min_value_pairs))
      
      if(!is.null(priority_rows_to_suppress)) { # if not null, use our final_row value to guide where our suppression should occur in our priority rows
        final_row_to_supp <- priority_rows_to_suppress[final_row_to_supp]
      }
      
      # using the above list, suppress our minimum values
      # suppress the rows/cols found above. This is done using a "min-value pairs" approach (i.e. take a look at minimum value pairings across rows and pick the pairs that grant the lowest total collectively)
      # this helps to improve our suppression selection
      suppressed_df[final_row_to_supp, final_cols_to_supp] <- 9999999
      
      # remove new columns that have had suppression applied
      cols_to_supp <- cols_to_supp[!colnames(matrix_to_use) %in% final_cols_to_supp]
    }
  }
  
  # replace 0s if we don't want to secondary suppress on them
  if(!secondary_suppress_0 & !run_from_cross_ref) {
    suppressed_df[suppressed_df == 33333333] <- 0
  }
  # return our output as a list
  return(list(suppressed_df))
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
  running_total_supp = FALSE,
  total_suppression,
  indirect_suppression
) {
  # create our transposed dfs
  transposed_df <- dplyr::as_tibble(t(df_to_suppress))
  
  # initially, subset df if required (so we only suppress across selected row/col combos)
  if(subset_df_along_row_and_col_arguments) {
    unsuppressed_df <- unsuppressed_df[rows_to_suppress,cols_to_suppress] # subset cols (keep only numeric cols...)
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
    running_total_supp = running_total_supp,
    total_suppression = total_suppression,
    indirect_suppression = indirect_suppression
  )[[1]]
  # convert back to our original form
  column_names_df_to_supp <- colnames(df_to_suppress)
  df_to_suppress <- dplyr::as_tibble(t(transposed_df))
  # reset colnames
  colnames(df_to_suppress) <- column_names_df_to_supp
  return(df_to_suppress)
  
}