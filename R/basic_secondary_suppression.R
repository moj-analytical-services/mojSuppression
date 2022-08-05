# basic function to apply secondary suppression to input columns
#apply secondary suppression to a string of numbers - requires primary suppression to have already been completed
apply_secondary_suppression_cols <- function(
  x,
  secondary_suppress_0 = TRUE # set to FALSE to prevent 0s from taking precendence for suppression (though will be suppressed if it's the only option)
) {
  
  #this section evaluates how many values in our df are equal to "9999999"
  #if exactly one value has been suppressed, then we run the first part of the if statement,
  #otherwise, we just return our original df
  if(length(which(x == 9999999)) == 1) {
    
    # replace 0s if we don't want to secondary suppress on them
    if(!secondary_suppress_0) {
      x[x == 0] <- 3333333
    }
    
    row_to_redact <- which(x == min(x[!is.infinite(x)], na.rm = TRUE))
    set.seed(10) # set seed so we always sample the same vals
    row_to_redact <- ifelse(length(row_to_redact) != 1, sample(row_to_redact, 1), row_to_redact) 
    
    x[row_to_redact] <- 9999999
    
    # return 0s
    if(!secondary_suppress_0) {
      x[x == 3333333] <- 0
    }
    
  }
  
  return(x)
}

# function to apply either primary or secondary suppression to a given set of row index numbers
apply_suppression_rows <- function(
  df, # specify dataframe to suppress on
  cols_to_suppress, # columns to suppress on. These should be presented like so: c("col1", "col2", ...)
  row_nos_to_suppress, #rows to be suppressed along. These should be numeric values: c(1:3), for example. 
  #If you want to suppress all rows, simply run from 1:length(df)
  suppression_thres, # this is the threshold for suppression. Values equal to or below this value will be suppressed
  suppress_0 = TRUE, # whether to suppress 0s or not. TRUE is the default and will suppress all 0s
  inc_secondary_suppression = TRUE, # flag to select whether primary or secondary suppression is applied. Secondary supp is the default.
  inc_primary_suppression = TRUE, # opt to remove primary suppression (secondary in turn won't work if no values == 9999999).
  secondary_suppress_0 = TRUE # set to FALSE to prevent 0s from taking precendence for suppression (though will be suppressed if it's the only option)
) { 
  #error if no or invalid row numbers are entered
  if(length(row_nos_to_suppress) == 0|!class(row_nos_to_suppress) %in% c("numeric", "integer")) {
    stop(print("Invalid or no row numbers given to suppress on. Please ensure that these are corrected or specified before continuing."))
  }
  
  #error if invalid suppression threshold entered
  if(class(suppression_thres) != "numeric"|suppression_thres == Inf|suppression_thres == -Inf) {
    stop("Please enter a valid suppression threshold")
  }
  
  #loop around the rows requiring suppression
  for(i in row_nos_to_suppress) { # it'd be considerably more efficient not to loop...
    #isolate the row to suppress
    row_to_supp <- df[i,] %>% 
      dplyr::select(cols_to_suppress) %>% 
      unlist()
    
    if(inc_primary_suppression) {
      #apply primary suppression
      if(suppress_0 == TRUE) {
        #suppress 0s
        row_to_supp[row_to_supp <= suppression_thres] = 9999999
      } else {
        #leave 0s unsuppressed
        row_to_supp[row_to_supp <= suppression_thres & row_to_supp > 0] = 9999999
      }
    }
    
    if(inc_secondary_suppression) { #if including secondary suppression, run code below
      if(length(which(row_to_supp == 9999999)) == 1) {
        
        # replace 0s if we don't want to secondary suppress on them
        if(!secondary_suppress_0) {
          row_to_supp[row_to_supp == 0] <- 3333333
        }
        
        row_to_redact <- which(row_to_supp == min(row_to_supp[!is.infinite(row_to_supp)], na.rm = TRUE))
        set.seed(10) # set seed so we always sample the same vals
        row_to_redact <- ifelse(length(row_to_redact) != 1, sample(row_to_redact, 1), row_to_redact) 
        
        row_to_supp[row_to_redact] <- 9999999
        
        # replace 0s if we don't want to secondary suppress on them
        if(!secondary_suppress_0) {
          row_to_supp[row_to_supp == 3333333] <- 0
        }
      }
    }

    #add our previous df back onto the original
    df[i, stringr::str_which(colnames(df), stringr::str_c(cols_to_suppress, collapse = "|"))] <- row_to_supp %>% as.list()
    
  }
  return(df)
}