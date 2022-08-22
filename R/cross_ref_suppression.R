# The cross checker function family try to answer the question of "how do we most effectively suppress when looking at a 2d problem?"
# For simplistic suppression on just rows or columns, you can simply look at the next smallest value in each row or column and suppress that.
# Applying this logic to a 2d problem quickly results in errors and misplaced suppression nodes

# To counteract this, the cross checker functions attempt to look at rows and columns that need secondary suppression and then attempts to find
# column/row combinations that minimise our suppression total (total value of all suppression nodes), while not leading to superfluous suppression.
# This is done by looking at reference flags for both our rows and columns in an attempt to allow the code to see what has already got suppression,
# where requires suppression and to find the best home for our required secondary suppression.

# Consider the following example of row/column suppression (you can imagine there are totals attached if that helps):
# (random_df <- dplyr::tibble(a=c(55, 66, 77, "~"), b = c(11, 4, 55, 4), c = c(11, "~", 4, 60), d = c(4, 66, 77, 100), e = c(77, 32, 77, "~")))
# Here, we have 5 columns, with three pieces of suppression already in place.
# The code will see that in column "a" we have suppression on row 4, col "c" it's in row 2 and col "e" it's in row 4.
# With this, it will look to place suppression nodes on [a, 2], [c, 4], [e, 2] as this is where we previously had suppression nodes.

# For more complex data that needs suppression there are some extra bits in the code to keep everything ticking over, but that's the basic premise.
apply_cross_reference_suppression_to_cols <- function(
  supp_list, # just enter a list with a single df if you wish to use this function for another purpose
  columns_to_supp
) {
  
  # this function's purpose is to scan and check which rows (or cols if you're using the transposed version of the function) have primary suppression in place
  # and attempt to match these to 
  # these are then plugged into various functions to help pull out suppression co-ordinates that minimise total suppression within the output
  # if you're suppressing in a 2-d plain (cols + rows), it doesn't make sense to just look at columns/rows that need secondary suppression and
  # suppress where values <=2. It makes more sense to pull out specific co-ordinates where we already have existing suppression in a row or column
  
  # our main suppression target is the first item in our suppression list
  df <- supp_list[[1]]
  
  # scan our cols for suppression and find those where number of suppressed values is only 1
  cols_requiring_suppression <- columns_to_supp[columns_to_supp %>% purrr::map_lgl(~sum(df[[.x]] == 9999999) == 1)]
  # find all columns with at least one suppression flag
  total_cols_with_supp_flag <- columns_to_supp[columns_to_supp %>% purrr::map_lgl(~sum(df[[.x]] == 9999999) > 0)]
  
  create_cross_checker_list <- function(df, columns_to_suppress) {
    # pulls out a list of rows which we can use as a basis for our cross referenced suppression
    
    # pull out a list of row indexes for columns where suppression has occurred
    # if "null" is returned, return 0
    cross_checker_list <- list()
    for(i in 1:length(columns_to_suppress)) {
      row_index <- which(df[[columns_to_suppress[i]]] == 9999999)
      if(length(row_index) == 0) {row_index <- 0} # set to 0 if there are no suppression points
      cross_checker_list[[i]] <- row_index
    }
    # rename the list with our column names
    names(cross_checker_list) <- columns_to_suppress
    # check for any duplicated values & replace these with zero
    duplicated_values <- unlist(cross_checker_list)[duplicated(unlist(cross_checker_list))]
    # replace duplicated values with a dummy value
    for(i in 1:length(cross_checker_list)) {
      cross_checker_list[[i]][cross_checker_list[[i]] %in% duplicated_values] <- 0
    }
    
    return(cross_checker_list)
  }
  
  # generate our cross checker list
  cross_checker_list <- create_cross_checker_list(df, columns_to_supp)
  
  # loop around our columns that we've outlined as being potentials for cross reference suppression and find the minimum value that each column
  # has for valid cross-ref combinations. A cross-ref combination is defined at the top of this function.
  # while we still have columns that can be cross-referenced, the system will continue to loop
  if(sum(unlist(cross_checker_list)>0, na.rm = TRUE) >= length(cols_requiring_suppression) & 
     length(total_cols_with_supp_flag) > 1 & 
     all(sum(unlist(cross_checker_list)>0, na.rm = TRUE) > 0, length(cols_requiring_suppression) > 0)) {
    potential_suppression_list <- 1 # initialise (so our loop starts)
    loop_counter <- 0
    while(length(unlist(potential_suppression_list)) > 0) { # while the system is still picking up items that can be suppressed
      # stop the loop if we count an "infinite loop" (just >100 iterations)
      loop_counter <- loop_counter+1
      if(loop_counter > 1000) {stop("Infinite loop detected. Please review the underlying cross checker code before continuing.")}
      calculate_potential_suppression_list <- function(df, 
                                                       cross_checker_list,
                                                       cols_requiring_suppression) {
        potential_suppression_list <- list()
        for(i in cols_requiring_suppression) {
          # remove any rows matching the currently suppressed row
          potential_rows_to_suppress <- unlist(cross_checker_list)[!unlist(cross_checker_list) %in% cross_checker_list[[i]]] %>% unique() # establish all potential rows that could be suppressed
          potential_rows_to_suppress <- potential_rows_to_suppress[potential_rows_to_suppress!=0] # rm 0s
          column_data <- df %>% dplyr::pull(i) # pull down the original column's data
          column_data_potential_suppression <- column_data[potential_rows_to_suppress] # filter for our potential rows
          # if there's a potential row to use, then identify it. If, however, all potential rows are assigned "0" (see cross_checker_list), then the output will be numeric(0) here
          if(length(column_data_potential_suppression)>0) {
            # create our names for each entry
            reference_names <- paste0("colname ~ ", i, "; rowref ~ ", potential_rows_to_suppress)
            # reference <- c(i, potential_rows_to_suppress)
            names(column_data_potential_suppression) <- reference_names # add potential rows as names for our data
            column_data_potential_suppression <- column_data_potential_suppression[which(column_data_potential_suppression == min(column_data_potential_suppression, na.rm = TRUE))] # arrange to find order of preference for suppression - use which.min so we keep vector names...
            
            potential_suppression_list[[i]] <- column_data_potential_suppression
          }
        }
        return(potential_suppression_list)
      }
      # calculate our list of potential suppressions
      potential_suppression_list <- calculate_potential_suppression_list(df,
                                                                         cross_checker_list,
                                                                         cols_requiring_suppression)
      
      # add a function to parse our suppression references and apply suppression
      apply_supp_using_references <- function(
        df,
        suppression_item
      ) {
        # with this, identify row and column info for suppression
        col_to_suppress <- stringr::str_extract(names(suppression_item), "(?<=colname ~ ).*(?=;)")
        row_to_suppress <- as.numeric(stringr::str_extract(names(suppression_item), "(?<=rowref ~ )[a-zA-Z|0-9]*"))
        df[row_to_suppress, col_to_suppress] <- 9999999
        return(df)
      }
      
      # find minimum value outlined above and use this as our first point of suppression
      potential_suppression_list <- unlist(potential_suppression_list)
      suppression_item <- potential_suppression_list[potential_suppression_list == min(potential_suppression_list, na.rm = TRUE)]
      # if our code has 2+ possible minima, stage other results for future testing
      # this is to circumvent the pain of finding an appropriate algorithm for how and in what order we suppress
      if(length(suppression_item) != 1) {
        # to check suppression totals for all items, create branches for all our suppression candidates
        # so we can check what suppressing down that branch would result in.
        # this replaces the idea of using a logic based algorithm to decide HOW and WHERE we suppress
        # instead, we attempt many different branches to see what the most appropriate would be 
        # (brute forcing the correct answer - much like in backtracking, but slightly less sophisticated)
        # this approach only works due to how quickly the code can suppress a single table
        for(i in 2:length(suppression_item)) {
          # where required, return 0s to 33333333
          df[df == 3333333] <- 33333333
          supp_list[[length(supp_list)+1]] <- apply_supp_using_references(
            df,
            suppression_item[i]
          )
        }
        
        # apply regular suppression to the first item
        df <- apply_supp_using_references(
          df,
          suppression_item[1]
        )
        
      } else {
        df <- apply_supp_using_references(
          df,
          suppression_item
        )
      }
      
      # recreate our cross checker list with our updated data
      cross_checker_list <- create_cross_checker_list(df, columns_to_supp)
      # recalculate columns requiring suppression
      cols_requiring_suppression <- columns_to_supp[columns_to_supp %>% purrr::map_lgl(~sum(df[[.x]] == 9999999) == 1)]
      # recalculate our potential suppression list so we can recheck if another loop is necessary
      potential_suppression_list <- calculate_potential_suppression_list(df,
                                                                         cross_checker_list,
                                                                         cols_requiring_suppression)
    }
  }
  
  supp_list[[1]] <- df
  
  return(supp_list) # return updated supp_list
}

# where our cross checking has finished, but we still have outstanding columns to suppress, this function will allow us to iterate over these columns and continue our suppression
run_secondary_suppression_across_cross_checked_df <- function(df, 
                                                              columns_to_supp,
                                                              priority_row_suppression = NULL, # specify a row number to allow it make it the priority for secondary suppression. The designated row does not need to fall in your main suppression area 
                                                              # (not within cols_to_suppress and rows_to_suppress). Please only enter a single value for this. This is considered secondary suppression for argument purposes.
                                                              ordered_priority_suppression = FALSE # set to TRUE if you'd like your priority suppression to follow the hierachy established in either the priority_row_suppression or priority_col_suppression arguments
                                                              # (i.e. for row priority, entering c(7, 2) will mean that row 7 is suppressed first and then row 2 is used if 7 was already suppressed)
) {
  # The primary purpose of this function is to pull out minimum pairs for us to trial suppression solutions on
  # The more potential solutions we generate, the more likely our final result is to be correct, so iterating
  # over more DFs is arguably desirable in this scenario
  
  # find cols requiring secondary suppression
  cols_requiring_suppression <- columns_to_supp[columns_to_supp %>% purrr::map_lgl(~sum(df[[.x]] == 9999999) == 1)]
  # pulls out our rows we wish to check with our flexible suppression
  suppd_rows <- which(1:nrow(df) %>% purrr::map_lgl(~sum(unlist(df[.x,]) == 9999999) > 0))
  
  if(length(cols_requiring_suppression) > 1) {
    # IF WE'VE SKIPPED THE WHILE STEP ABOVE (due to the if statement not being met) - run everything below...
    # if we have columns requiring suppression work that can't be cross-referenced, apply initial secondary suppression to the column with the lowest min
    # if priority row supp is being undertaken, identify where to apply this
    if(is.null(priority_row_suppression)) {
      priority_row_suppression <- NULL
    } else {
      priority_row_suppression <- priority_row_suppression
      ordered_priority_suppression <- ordered_priority_suppression
      ifelse(ordered_priority_suppression == FALSE, 
             priority_rows <- priority_row_suppression, 
             priority_rows <- priority_row_suppression[1])  
    }
    
    # IF WE'VE SKIPPED THE WHILE STEP IN THE PREVIOUS FUNCTION (due to the if statement not being met) - run everything below...
    # if we have columns requiring suppression work that can't be cross-referenced, apply initial secondary suppression to the column with the lowest min
    # to make the process simpler, pull out a matrix of only our cols requiring suppression. This way our index is much simpler. Our matrix will be bound back onto the original df later
    if(is.null(priority_row_suppression)) {
      cross_df_matrix <- as.matrix(df[cols_requiring_suppression])
    } else {
      cross_df_matrix <- as.matrix(df[priority_rows, cols_requiring_suppression])
      # find the accompanying min value that's been identified (if this is 9999999 - suppressed - then we need to reidentify our suppression rows)
      minimum_matrix_value <- min(cross_df_matrix)
      min_val_check <- minimum_matrix_value == 9999999
      
      # following the above, only run some additional checks to make sure our selected row is suppressable if either our priority rows input == 1
      # this is only true where ordered_priority_suppression is TRUE (i.e. we have a suppression preference from our priority rows) or we only have one input for our priority rows
      # if this is >1, then by default only one row should have suppression in our original df.
      if(length(priority_rows == 1)&min_val_check) {
        if(length(priority_row_suppression)==1) { # if the user only input one row for priority supp, our only option is to return all rows and find the min normally
          cross_df_matrix <- as.matrix(df[cols_requiring_suppression])
          priority_row_suppression <- NULL # update variable so we get our expected output later
        } else { # else the user must have added multiple priority rows and instead selected that one should take priority. This priority row has returned some existing suppression, so move onto the next option
          priority_rows <- priority_row_suppression[2]
          cross_df_matrix <- as.matrix(df[priority_rows, cols_requiring_suppression])
        }
      }
    }
    
    # quick bit of code to generate our final solution
    create_solution <- function(df, final_row_to_supp, final_col_to_supp, values) {
      df[final_row_to_supp, final_col_to_supp] <- values
      return(df)
    }
    
    # we shouldn't have any values where len(cols_requiring_supp)==1, so just check for whether the value is odd or even
    # if even, ony look for a min-value pair of length 2
    # otherwise, look for one with a min value of 3
    value_pair_len <- 2
    
    # find our min value pairs (name correspondings to the column in cross_df_matrix. Value corresponds to the total it has found to be the min)
    min_value_pairs <- find_min_value_pair(matrix_to_use = cross_df_matrix,
                                           value_pair_len = value_pair_len)
    # following that, find our co-ords for our min value pair
    final_cols_to_supp <- names(sort(cross_df_matrix[as.numeric(names(min_value_pairs)),]) %>% 
                                  head(value_pair_len))
    final_row_to_supp <- as.numeric(names(min_value_pairs))
    if(!is.null(priority_row_suppression)) { # if not null, use our final_row value to guide where our suppression should occur in our priority rows
      final_row_to_supp <- priority_row_suppression[final_row_to_supp]
    }
    
    # create our min value solution...
    min_value_pair_solution <- create_solution(df, final_row_to_supp, final_cols_to_supp, 9999999)
    
    # now, check our flexible pair solution and evaluate output that as a potential solution too...
    if(length(suppd_rows)>1) {
      suppd_cross_df <- cross_df_matrix[suppd_rows,]
      f <- find_flexible_min_value_pair(matrix = suppd_cross_df)
      
      final_row_to_supp <- c(suppd_rows[f[1,1]], suppd_rows[f[2,1]]) # row indices
      final_cols_to_supp <- c(colnames(cross_df_matrix)[f[1,2]], colnames(cross_df_matrix)[f[2,2]]) # col indices
      # loop around both solutions and suppress...
      flexible_min_pair <- df
      for(i in 1:length(final_row_to_supp)) flexible_min_pair <- create_solution(flexible_min_pair, final_row_to_supp[i], final_cols_to_supp[i], 9999999)
      
    } else {
      flexible_min_pair <- min_value_pair_solution
    }
    
    # create a list with our two min value solutions
    min_values_list <- list(
      min_val_pair = min_value_pair_solution,
      flexible_min_pair = flexible_min_pair
    )
    
  }
  
  return(unique(min_values_list))
}

apply_cross_reference_suppression_cols <- function(supp_list, 
                                                   columns_to_supp,
                                                   priority_row_suppression = NULL, # specify a row number to allow it make it the priority for secondary suppression. The designated row does not need to fall in your main suppression area 
                                                   # (not within cols_to_suppress and rows_to_suppress). Please only enter a single value for this. This is considered secondary suppression for argument purposes.
                                                   ordered_priority_suppression = FALSE, # set to TRUE if you'd like your priority suppression to follow the hierachy established in either the priority_row_suppression or priority_col_suppression arguments
                                                   limit
                                                   # (i.e. for row priority, entering c(7, 2) will mean that row 7 is suppressed first and then row 2 is used if 7 was already suppressed)
) {
  
  if(length(columns_to_supp) == 1) {
    stop("Error, you've only entered one column to suppress on, but have requested row suppression. Please
         check your cols_to_suppress argument before continuing.")
  }
  # initially, run the code over the data
  # print(stringr::str_glue("Length of supp_list is currently: {length(supp_list)}"))
  supp_list <- apply_cross_reference_suppression_to_cols(supp_list, columns_to_supp)
  
  # calculate outstanding columns requiring suppression
  cols_requiring_suppression <- columns_to_supp[columns_to_supp %>% purrr::map_lgl(~sum(supp_list[[1]][[.x]] == 9999999) == 1)]
  
  # initialise prev_df
  prev_df <- supp_list[[1]]
  loop_counter <- 0
  while(length(cols_requiring_suppression) > 1 & loop_counter <= limit) {
    # check for an infinite loop
    loop_counter <- loop_counter+1
    if(loop_counter > 1000) {stop("Infinite loop detected. Please review the underlying cross checker code before continuing.")}
    
    if(loop_counter > 0) {
      # if dupe, then change our 0s back to the smaller value
      if(isTRUE(all.equal(supp_list[[1]], prev_df))) {
        supp_list[[1]][supp_list[[1]] == 33333333] <- 3333333
      }
    }
    
    out_list <- run_secondary_suppression_across_cross_checked_df(supp_list[[1]], 
                                                                  columns_to_supp,
                                                                  priority_row_suppression,
                                                                  ordered_priority_suppression
    ) # run code to apply secondary suppression to lowest col
    supp_list[[1]] <- out_list[[1]]
    out_list <- out_list[-1]
    # add our additional dfs to our supp_list
    supp_list <- c(supp_list, out_list)
    supp_list <- apply_cross_reference_suppression_to_cols(supp_list, columns_to_supp) # reapply our cross reference methodology
    # recalculate outstanding columns requiring suppression
    cols_requiring_suppression <- columns_to_supp[columns_to_supp %>% purrr::map_lgl(~sum(supp_list[[1]][[.x]] == 9999999) == 1)]
    
    # update our previous df flag and remove 0 conversion
    prev_df <- supp_list[[1]]
    supp_list[[1]][supp_list[[1]] == 3333333] <- 33333333
  }
  
  return(supp_list)
}

apply_cross_reference_suppression_to_rows <- function(supp_list, 
                                                      columns_to_suppress,
                                                      limit
) {
  # create a new list for our row suppression entries (this is to allow us to easily transpose the data back into its original format)
  transposed_df_list <- list(
    supp_list[[1]]
  )
  
  # this function should be used in conjunction with our other suppression functions. Therefore, it's expecting a trim downed version of your original data to be fed in
  rows_to_supp <- 1:nrow(transposed_df_list[[1]])
  total_rows_with_supp_flag <- rows_to_supp[rows_to_supp %>% purrr::map_lgl(~sum(unlist(transposed_df_list[[1]][.x,]) == 9999999) > 0)]
  # set initial flags
  row_flags <- rows_to_supp[rows_to_supp %>% purrr::map_lgl(~sum(transposed_df_list[[1]][.x,columns_to_suppress] %>% unlist() == 9999999) == 1)]
  loop_counter <- 0
  while(length(row_flags) > 1 & length(total_rows_with_supp_flag)>1) { # while we have at least one active flag, loop this process
    # check for an infinite loop
    loop_counter <- loop_counter+1
    if(loop_counter > 1000) {stop("Infinite loop detected. Please review the underlying cross checker code before continuing.")}
    
    transposed_df_list[[1]] <- t(transposed_df_list[[1]] %>% dplyr::select(columns_to_suppress)) # this transposes our origianl df and puts it in matrix form
    transposed_df_list[[1]] <- as.data.frame(transposed_df_list[[1]]) # transform back to df
    # with our df which has been inverted, reapply cross referenced suppression (this time, it should automatically detect where df gap can be filled for rows and failing that, apply sensible secondary suppression)
    # print(stringr::str_glue("Loop starting size {length(transposed_df_list)}"))
    transposed_df_list <- apply_cross_reference_suppression_cols(transposed_df_list, 
                                                                 colnames(transposed_df_list[[1]]),
                                                                 limit=limit
    )
    # print(stringr::str_glue("Loop exit size {length(transposed_df_list)}"))
    # loop around our transposed list and generate our cleaned output
    transposed_df_list <- transposed_df_list %>% 
      purrr::map(
        ~dplyr::as_tibble(t(.x)) %>% 
          `colnames<-` (columns_to_suppress)
      )
    # plug our list back through column cross ref supp
    transposed_df_list <- apply_cross_reference_suppression_cols(transposed_df_list, 
                                                                 columns_to_suppress,
                                                                 limit=limit)
    # recheck our flags. If there now exist 0 flags in total, 
    row_flags <- rows_to_supp[rows_to_supp %>% purrr::map_lgl(~sum(transposed_df_list[[1]][.x,columns_to_suppress] %>% unlist() == 9999999) == 1)]
    
  }
  # overwrite our first entry of our supp_df_list
  supp_list[[1]] <- transposed_df_list[[1]]
  
  # return our combined list
  return(c(supp_list,
           transposed_df_list[-1]))
}

perform_cross_ref_suppression_on_single_col <- function(supp_list, 
                                                        columns_to_suppress, 
                                                        col_requiring_suppression,
                                                        priority_row_suppression = NULL, # specify a row number to allow it make it the priority for secondary suppression. The designated row does not need to fall in your main suppression area 
                                                        # (not within cols_to_suppress and rows_to_suppress). Please only enter a single value for this. This is considered secondary suppression for argument purposes.
                                                        ordered_priority_suppression = FALSE, # set to TRUE if you'd like your priority suppression to follow the hierachy established in either the priority_row_suppression or priority_col_suppression arguments
                                                        # (i.e. for row priority, entering c(7, 2) will mean that row 7 is suppressed first and then row 2 is used if 7 was already suppressed)
                                                        secondary_suppress_0 = TRUE,
                                                        iteration_limit
) {
  
  # if we only have one column outstanding, apply secondary suppression along our remaining columns (by selecting the lowest available value) and then finalise by using our cross reference suppression technique
  create_cross_checker_list <- function(df, columns_to_suppress) {
    # find rows that have been suppressed and where they've been suppressed
    cross_checker_list <- list()
    for(i in 1:length(columns_to_suppress)) {
      row_index <- which(df[[columns_to_suppress[i]]] == 9999999)
      if(length(row_index) == 0) {row_index <- 0} # make it too large to ever realistically be selected as a suppression target
      cross_checker_list[[i]] <- row_index
    }
    names(cross_checker_list) <- columns_to_suppress
    
    return(cross_checker_list)
  }
  
  # generate our cross checker list
  cross_checker_list <- create_cross_checker_list(supp_list[[1]], 
                                                  columns_to_suppress
  )
  
  # where we have multiple cross referenced suppression notes already, slot our final supp note into a suitable row
  if(sum(unlist(cross_checker_list)>0, na.rm = TRUE) > 1) {
    # find rows that you can potentially suppress across
    rows_to_supp <- unique(unlist(cross_checker_list))
    rows_to_supp <- rows_to_supp[!rows_to_supp %in% cross_checker_list[[col_requiring_suppression]]]
    rows_to_supp <- rows_to_supp[rows_to_supp != 0] # remove 0 if present
    # pull column data
    column_data <- supp_list[[1]] %>% 
      dplyr::pull(col_requiring_suppression)
    column_data <- column_data[rows_to_supp] 
    suppression_index <- which(column_data == min(column_data, na.rm = TRUE))
    set.seed(55) # set a seed to allow for consistent suppression
    if(length(suppression_index) != 1) {suppression_index <- suppression_index[sample(1:length(suppression_index), 1)]}  # sample so we only have one suppression item
    suppression_row <- rows_to_supp[suppression_index]
    
    ## finally, apply suppression
    supp_list[[1]][suppression_row, col_requiring_suppression] <- 9999999
  } else { # the other scenario is that we have only one suppression marker in our entire df, so apply secondary suppression normally before cross referencing
    # if the user has specified that specific rows be given suppression priority, then apply supp accordingly
    # other apply standard secondary suppression
    if(!is.null(priority_row_suppression)) {
      supp_list[[1]] <- apply_priority_row_suppression(
        df = supp_list[[1]],
        cols_to_suppress = col_requiring_suppression,
        row_nos_to_suppress = 1:nrow(supp_list[[1]]),
        row_index = priority_row_suppression,
        row_index_ordered_suppression = ordered_priority_suppression,
        secondary_suppress_0 = secondary_suppress_0
      )
    } else {
      supp_list[[1]] <- supp_list[[1]] %>% # apply secondary suppression to cols
        dplyr::mutate_at(col_requiring_suppression, ~apply_secondary_suppression_cols(.x, 
                                                                                      secondary_suppress_0 = secondary_suppress_0)
        )
    }
    
    # apply cross reference code to properly assess where further suppression marks should go
    supp_list <- apply_cross_reference_suppression_cols(supp_list, 
                                                        columns_to_suppress,
                                                        priority_row_suppression = priority_row_suppression,
                                                        ordered_priority_suppression = ordered_priority_suppression,
                                                        limit=iteration_limit
    ) %>% 
      apply_cross_reference_suppression_to_rows(., columns_to_suppress, iteration_limit)
  }
  return(supp_list)
}

apply_final_cross_suppression <- function(supp_list, 
                                          columns_to_suppress,
                                          priority_row_suppression = NULL, # specify a row number to allow it make it the priority for secondary suppression. The designated row does not need to fall in your main suppression area 
                                          # (not within cols_to_suppress and rows_to_suppress). Please only enter a single value for this. This is considered secondary suppression for argument purposes.
                                          ordered_priority_suppression = FALSE, # set to TRUE if you'd like your priority suppression to follow the hierachy established in either the priority_row_suppression or priority_col_suppression arguments
                                          # (i.e. for row priority, entering c(7, 2) will mean that row 7 is suppressed first and then row 2 is used if 7 was already suppressed)
                                          secondary_suppress_0 = TRUE
) {
  
  # for the majority of cases, this won't be needed
  # however, under certain scenarios, no cross reference suppression is necessary in the early stages, or the rules this suppression is built around leave a final column needing suppression
  # for example, if you have a 2xn grid with only one suppression node, the code won't know where else to suppress in the early cross-ref stages as it only have a column value to work off. Once further information is created in later stages, 
  # this function can be used to clean things up
  
  # with our current df, check for columns with oustanding suppression
  cols_requiring_suppression <- columns_to_suppress[columns_to_suppress %>% purrr::map_lgl(~sum(supp_list[[1]][[.x]] == 9999999) == 1)]
  
  # quick check to ensure that we only have 0 or 1 cols remaining
  if(!length(cols_requiring_suppression) %in% c(0, 1)){
    stop("Attempting to run final suppression step where total columns requiring suppression are >1.")
  }
  
  # if we have an outstanding piece of column suppression, either add this to a row with pre-existing suppression, 
  # or apply normal secondary suppression before finding relevant cross-referenced indexes to suppress
  if(length(cols_requiring_suppression)==1) {
    supp_list <- perform_cross_ref_suppression_on_single_col(supp_list, 
                                                             columns_to_suppress, 
                                                             cols_requiring_suppression,
                                                             priority_row_suppression,
                                                             ordered_priority_suppression,
                                                             secondary_suppress_0 = secondary_suppress_0
    )
  }
  
  # check our current df for outstanding row suppression
  rows_to_supp <- 1:nrow(supp_list[[1]]) # df passed to this function should be a subset of the original with no character columns
  rows_requiring_suppression <- rows_to_supp[rows_to_supp %>% purrr::map_lgl(~sum(supp_list[[1]][.x,columns_to_suppress] %>% unlist() == 9999999) == 1)]
  
  # save original colnames so we can replace these after row suppression
  original_c_names <- colnames(supp_list[[1]])
  
  # if we have an outstanding piece of column suppression, either add this to a row with pre-existing suppression, 
  # or apply normal secondary suppression before finding relevant cross-referenced indexes to suppress
  if(length(rows_requiring_suppression)==1) {
    transposed_df <- as.data.frame(t(supp_list[[1]])) # transpose our df and apply our column suppression methodology
    transposed_df_list <- perform_cross_ref_suppression_on_single_col(
      list(transposed_df), 
      colnames(transposed_df), 
      colnames(transposed_df)[rows_requiring_suppression],
      secondary_suppress_0 = secondary_suppress_0)
    
    # loop around our transposed list and generate our cleaned output
    transposed_df_list <- transposed_df_list %>% 
      purrr::map(
        ~dplyr::as_tibble(t(.x)) %>% 
          `colnames<-` (original_c_names)
      )
    # replace the first value in our supp_list and return our final output
    supp_list[[1]] <- transposed_df_list[[1]]
    
    supp_list <- c(
      supp_list,
      transposed_df_list[-1]
    )
  }
  
  return(supp_list)
}


apply_cross_referenced_suppression <- function(supp_df_list,
                                               columns_to_suppress,
                                               priority_row_suppression = NULL, # specify a row number to allow it make it the priority for secondary suppression. The designated row does not need to fall in your main suppression area 
                                               # (not within cols_to_suppress and rows_to_suppress). Please only enter a single value for this. This is considered secondary suppression for argument purposes.
                                               ordered_priority_suppression = FALSE, # set to TRUE if you'd like your priority suppression to follow the hierachy established in either the priority_row_suppression or priority_col_suppression arguments
                                               # (i.e. for row priority, entering c(7, 2) will mean that row 7 is suppressed first and then row 2 is used if 7 was already suppressed)
                                               secondary_suppress_0 = TRUE,
                                               # arguments needed for total suppression
                                               total_suppression = FALSE,
                                               indirect_suppression = FALSE,
                                               unsuppressed_df = NULL,
                                               suppression_thres,
                                               rows_nos_to_suppress,
                                               limit_branching
){
  # convert our df to a list, if it isn't already in that format
  if(!is_list(supp_df_list)) {
    supp_df_list <- list(supp_df_list)
  }
  
  # replace 0s if we don't want to secondary suppress on them
  if(!secondary_suppress_0) {
    supp_df_list <- supp_df_list %>% 
      purrr::map(
        ~{
          .x[.x == 0] <- 33333333
          .x
        }
      )
  }
  # set iteration limits if we're limit our branching
  if(!is.null(limit_branching)) {iteration_limit = 2} else {iteration_limit=1000}
  
  # The next bit of code uses our supp_df_list (suppressed input list - this is either a single df, or multiple, depending on previous steps)
  # As we run the code, new items will be added to the list as 'suppression branches' (see code below)
  # These branches are simply instances where multiple decisions on what to suppress can be made (i.e. we have three 3s we can in theory suppress)
  # but it's not initially obvious which to suppress. If you do suppress manually, you'll come across plenty of examples where you have to take a minute
  # to think about what the correct suppression step would be. This code "thinks" by branching and evaluating multiple possible solutions,
  # allowing us to check all of these solutions and find the winner.
  
  # to simplify the alogrithm (at the cost of performance), run suppression checks across lots all paths we find, and evaluate to find the
  # best suppression selection available to us
  suppressed_list <- list()
  suppressed_sum <- c()
  
  # create a counter for our total iterations
  loop_iteration <- 0
  
  while(length(supp_df_list) > 0 & length(suppressed_list)<iteration_limit) {
    # if(loop_iteration > 1000) {stop("Infinite loop detected. Please contact the creator of the code for assistance.")}
    # print(paste0("Looping around main loop: ", loop_iteration))
    # where appropriate, revert our 0s to 33333333
    supp_df_list[[1]][supp_df_list[[1]] == 3333333] <- 33333333
    
    # create a quick function to apply all of our suppression functions...
    apply_suppression <- function(supp_df_list) {
      # we will need a while loop here
      # join all of our cross reference functions and create our final output
      supp_df_list <- apply_cross_reference_suppression_cols(supp_df_list, 
                                                             columns_to_suppress,
                                                             priority_row_suppression,
                                                             ordered_priority_suppression,
                                                             limit=iteration_limit
      ) %>%
        apply_cross_reference_suppression_to_rows(., columns_to_suppress, limit=iteration_limit)
      
      # finally, check if there is any additional suppression required
      supp_df_list <- apply_final_cross_suppression(supp_df_list, 
                                                    columns_to_suppress,
                                                    priority_row_suppression,
                                                    ordered_priority_suppression,
                                                    secondary_suppress_0=secondary_suppress_0
      )
      return(supp_df_list)
    }
    
    # apply suppression
    # print(stringr::str_glue("Length prior to supp {length(supp_df_list)}"))
    supp_df_list <- apply_suppression(supp_df_list)
    # print(stringr::str_glue("Length after supp {length(supp_df_list)}"))
    
    # if we are suppressing on our column totals, apply our total suppression!
    if(total_suppression | indirect_suppression) {
      
      # check only our current df and suppress totals if necessary
      supp_df_list[[1]] <- suppress_col_total_below_supp_thres(
        supp_df_list[[1]],
        unsuppressed_df,
        suppression_thres,
        cols_to_suppress = columns_to_suppress, # set cols to suppress on
        rows_to_suppress = rows_nos_to_suppress, # set rows to suppress on
        TRUE,
        priority_rows_to_suppress = priority_row_suppression,
        secondary_suppress_0 = secondary_suppress_0,
        run_from_cross_ref = TRUE,
        total_suppression = total_suppression,
        indirect_suppression = indirect_suppression
      )[[1]] %>%
        ## apply row total suppression
        # very slow...
        suppress_row_total_below_supp_thres(
          .,
          unsuppressed_df,
          suppression_threshold = suppression_thres,
          cols_to_suppress = columns_to_suppress, # set cols to suppress on
          rows_to_suppress = rows_nos_to_suppress, # set rows to suppress on
          TRUE,
          secondary_suppress_0 = secondary_suppress_0,
          total_suppression = total_suppression,
          indirect_suppression = indirect_suppression
        )
      
      # apply suppression again (following total being suppressed...)
      supp_df_list <- apply_suppression(supp_df_list)
    }
    # remove any duplicates we've picked up
    supp_df_list <- unique(supp_df_list)
    
    # add df to our suppressed list
    suppressed_list[[length(suppressed_list)+1]] <- list(
      df = supp_df_list[[1]]
    )
    suppressed_sum = append(suppressed_sum,sum(supp_df_list[[1]],na.rm=TRUE))
    # remove the first item in our list and iterate over the next version
    supp_df_list <- supp_df_list[-1]
    
    loop_iteration <- loop_iteration+1
    
  }
  
  # select our best case
  set.seed(55)
  df <- suppressed_list[[sample(which(suppressed_sum == max(suppressed_sum)),1)]]$df
  
  # replace 0s if we don't want to secondary suppress on them
  if(!secondary_suppress_0) {
    df[df == 33333333] <- 0
  }
  
  return(df)
}
