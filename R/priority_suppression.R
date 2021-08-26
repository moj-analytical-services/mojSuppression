# if priority row suppression has been specified, prioritise secondary suppression here
# this function looks at a specified. If that column has primary suppression applied, but no secondary suppression, the row_index entered below will act as a priority
# for where to secondary suppress

# assess whether secondary suppression is necessary. If it is, apply it to our priority row with the lowest value
priority_row_suppression_single_column <- function(df,
                                                   col,
                                                   row_index) {
  if(length(which(df[[col]] == 9999999)) == 1) { # i.e. no indirect secondary suppression
    if(length(row_index) > 1) { # just quickly check how many row inputs we have. If only one, then keep that
      # find which row index should be used for suppression
      min_value_row <- as.numeric(names(find_min_value_pair(as.matrix(df[row_index,]),
                                                            2)))
      row_index_to_suppress <- row_index[min_value_row]
    } else { row_index_to_suppress <- row_index }
    
    df[paste(row_index_to_suppress), col] <- 9999999
  }
  
  return(df)
}

apply_priority_row_suppression <- function(df,
                                           cols_to_suppress, # these are the columns to look to apply our priority suppression along
                                           row_nos_to_suppress = NULL, # when NULL, all rows are available for regular secondary suppression. If set to a set of numbers, only these will be secondary suppressed (priority suppression is performed separately to secondary suppression)
                                           # so your priority value does not need to lie in this range
                                           row_index, # and where to apply priority suppression if required. You can enter more than one row here - c(2, 7) is accepted, for example
                                           row_index_ordered_suppression = FALSE, # if set to TRUE, suppress sequentially across your row_index values. Please ensure that your ordering in row_index is correct if you use this argument 
                                           secondary_suppress_0 = TRUE
) {
  
  
  if(!any(row.names(df) %in% paste(row_index))) {
    stop("Error, your row index is not included in the df you're attempting to suppress on. Please
         adjust which rows you're suppressing on and try and again.")
  }
  
  if(row_index_ordered_suppression) { # if your row_index is ordered, then suppress in that exact order
    # assess whether secondary suppression is necessary. If it is, apply it to our priority row.
    for(col in cols_to_suppress) {
      for(row in row_index) {
        df <- priority_row_suppression_single_column(df,
                                                     col,
                                                     row)
      }
    }
  } else { # if not ordered and any of your priority rows can be used to secondary suppress, find the minimum value
    # assess whether secondary suppression is necessary. If it is, apply it to our priority row.
    for(col in cols_to_suppress) {
      df <- priority_row_suppression_single_column(df,
                                                   col,
                                                   row_index)
    }
  }
  
  # and finally, apply secondary suppression code for columns (so if we set our priority column to an already suppressed row, then we correct for this)
  if(is.null(row_nos_to_suppress)) {row_nos_to_suppress <- 1:nrow(df)} # if NULL, just assume all rows in use
  # and also check that we do indeed require secondary suppression in our given columns
  for(col in cols_to_suppress) {
    if(length(which(df[[col]] == 9999999)) == 1) { # just check that only one piece of suppression has been applied
      df[row_nos_to_suppress,] <- df[row_nos_to_suppress,] %>% 
        dplyr::mutate_at(col, 
                         apply_secondary_suppression_cols,
                         secondary_suppress_0)
    }
  }
  
  
  return(df)
}