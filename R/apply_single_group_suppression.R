#' Suppress an input dataframe (with some additional options)
#'
#' @description
#'
#' This fuction offers a way for users to suppression rows, columns or both rows and columns simultaneously.
#' For a more streamlined version (without some features, but easier to use), please see `simple_suppression`.
#'
#' @details
#' The `suppress_single_group()` function can be used to apply both primary or 
#' secondary suppression across a selection of either rows or columns,
#' suppressing all values below a specified suppression threshold.
#' This funtion expects that the input be your final dataframe that you wish to suppress on.
#' Please filter down your existing work, should it not meet this criteria.
#' 
#' The `suppress_single_group()` function offers more functionality than the alternative, `simple_suppression`, at the
#' cost of being slightly less intuitive and more daunting for new users.
#' If you need any of the following rules, please use this function, otherwise you may be better served by the `simple_suppression` function:
#' * total suppression - i.e. where total suppression nodes sum to <= suppression threshold and more suppression is necessary
#' * priority suppression - where you want certain rows to take precedence for your secondary suppression
#' * pseudo suppression - where you have columns you don't want to be actively suppressed, but if their values fall below your suppression threshold other columns need suppression
#' 
#' @param df Specify dataframe to suppress on
#' @param where_to_suppress Which orientation you'd like to suppress on. For both row and column suppression, please use: c("row", "col")
#' @param cols_to_suppress Columns to suppress on. These should be presented in the following format: `c("col1", "col2", ...)`
#' If no value is entered, all columns will automatically be selected for suppression. 
#' If a non-numeric column is selected, the code will forcefully stop the process
#' @param row_nos_to_suppress Rows to be suppressed along. These should be numeric values: `c(1:3)`, for example.
#' If no value is entered, all rows will be automatically selected for suppression.
#' @param suppression_thres This is your threshold value to determine which values should be suppressed. 
#' Values equal to, or below this value will be suppressed.
#' @param suppress_0 Whether to suppress 0s or not. TRUE is the default and will suppress all 0s
#' @param inc_secondary_suppression Flag to select whether only primary or secondary suppression is applied. Secondary suppression is the default.
#' @param suppression_output_value Set a value to output where suppression has occurred. It is advised you set this to a numeric value (or leave at the default)
#' while working with your data in R, as setting the output to `~` will set your column to a character class.
#' @param secondary_suppress_0 Specify whether 0 can be used for secondary suppression or not. If TRUE, 0s will be suppressed when secondary suppression is performed
#' 
#' @include utils.R
#' @include primary_suppression.R
#' @include basic_secondary_suppression.R
#' @include cross_ref_suppression.R
#' @include priority_suppression.R
#' @include pseudo_suppression.R
#' @include total_suppressors.R
#' 
#' 
#' @importFrom magrittr "%>%"
#' @importFrom utils "head"
#' 
#' @export
#' @examples
#' # Create our test df
#' bme <- c(25, 1, 1, 10)
#' white <- c(10, 6, 10, 1)
#' other <- c(1, 5, 10, 10)
#' test <- c(5, 6, 4, 3)
#' 
#' df <- data.frame(bme = bme,
#'                  white = white,
#'                  other = other,
#'                  test = test)
#' 
#' # simple example for suppressing across all rows and columns
#' mojSuppression::suppress_single_group(
#'     df = df,
#'     where_to_suppress = c('col', 'row'),
#'     suppression_thres = 2
#' )
#' 



# function to apply both primary or secondary suppression across a selection of either rows, cols, or both
# this should be used on standard dfs where data is already in its final form 
# if your data is in a longer format (column headers are in a single column, for example), please see the function below
suppress_single_group <- function(
  df, # specify dataframe to suppress on
  where_to_suppress = c("col"), # which orientation to suppress on. For both row and column supp: c("row", "col")
  cols_to_suppress = NULL, # columns to suppress on. These should be presented like so: c("col1", "col2", ...)
  # leave this blank if you're only suppressing on rows, or enter a value to ensure you only suppress across specific row/col combos
  row_nos_to_suppress = NULL, #rows to be suppressed along. These should be numeric values: c(1:3), for example. 
  #If you want to suppress all rows, simply run from 1:length(df)
  # leave this blank if you're only suppressing on cols
  suppression_thres, # this is the threshold for suppression. Values equal to or below this value will be suppressed
  suppress_0 = TRUE, # whether to suppress 0s or not. TRUE is the default and will suppress all 0s
  inc_secondary_suppression = TRUE, # flag to select whether primary or secondary suppression is applied. Secondary supp is the default.
  suppression_output_value = 9999999, #set a value to output where suppression has occurred
  subset_df_along_row_and_col_arguments = TRUE, # TRUE by default. This essentially states that you want your "cols_to_suppress" and "row_nos_to_suppress" to be the only combinations suppressed
  # setting this to FALSE means that the code will look at the whole of a row or column that's supplied above, but will only suppress these rows/cols.
  # setting this to TRUE is the inverse. A column can only be suppressed using rows you've selected above.
  columns_to_pseudo_suppress = NULL, # if there are any columns that you don't want to have suppression markings within, but if there value falls below your threshold, require secondary suppression in another
  # column, then please input their column names here. If there's any confusion on this, please refer to the examples below.
  rows_to_pseudo_suppress = NULL,
  pseudo_suppression_threshold = NULL, # this is automatically set to the suppression threshold if no value is entered
  pseudo_suppress_0 = NULL, # this value gives you control over whether 0s should be pseudo suppressed (assuming you're using this functionality). Leaving this as NULL copies your input for suppress_0
  priority_row_suppression = NULL, # specify a row number to allow it make it the priority for secondary suppression. The designated row does not need to fall in your main suppression area 
  # (not within cols_to_suppress and rows_to_suppress). Please only enter a single value for this. This is considered secondary suppression for argument purposes.
  ordered_priority_suppression = FALSE, # set to TRUE if you'd like your priority suppression to follow the hierachy established in either the priority_row_suppression or priority_col_suppression arguments
  # (i.e. for row priority, entering c(7, 2) will mean that row 7 is suppressed first and then row 2 is used if 7 was already suppressed)
  suppression_pre_applied = FALSE, # stops primary suppression. This is intended for use in the tidy suppression function, so please avoid using it here.
  total_suppression = FALSE, # set to TRUE if you don't want your suppression totals for rows or columns to fall below your suppression threshold. i.e. if you suppression two 1s in a column, this will suppress a third value for you
  indirect_suppression = FALSE, # need notes here...
  secondary_suppress_0 = TRUE # set to FALSE to prevent 0s from taking precendence for suppression (though will be suppressed if it's the only option)
  # this argument currently has no impact on the cross suppression used throughout. If you opt to suppress on both rows and cols, some 0s may be suppressed in the process
) { 
  
  if(rlang::is_missing(suppression_thres)) {
    stop("No suppression threshold has been set. Please add a value for suppression_thres when initialising the function.")
  }
  if(rlang::is_missing(cols_to_suppress)) {
    print("No columns specified for suppression - all columns will be used.")
  }
  if(rlang::is_missing(row_nos_to_suppress)) {
    print("No rows specified for suppression - all rows will be used.")
  }
  
  # if no cols are entered for suppression, select all numeric cols
  if(is.null(cols_to_suppress)) {
    cols_to_suppress <- df %>% 
      dplyr::select_if(is.numeric) %>% 
      colnames()
  }
  # if no rows are entered for suppression, select all available rows
  if(is.null(row_nos_to_suppress)) {
    row_nos_to_suppress <- 1:nrow(df)
  }
  
  # find the row index to use for our priority row suppression (if applicable)
  if(is.null(priority_row_suppression)) {
    row_index_to_use <- NULL
  } else {
    row_index_to_use <- priority_row_suppression-(min(row_nos_to_suppress)-1)
  }
  
  # if no pseudo supp value is entered, set this to our supp threshold
  if(is.null(pseudo_suppression_threshold)) pseudo_suppression_threshold <- suppression_thres
  
  # split out our original, unsuppressed df to use later in this process
  unsuppressed_df <- df
  
  # initially, work out whether we're pseudo suppressing 0
  if(is.null(pseudo_suppress_0)) {
    pseudo_suppress_0 <- suppress_0
  }
  
  # initially, subset df if required (so we only suppress across selected row/col combos)
  df_to_suppress <- df
  
  if(subset_df_along_row_and_col_arguments) {
    df_to_suppress <- df_to_suppress[,cols_to_suppress] # subset cols (keep only numeric cols...)
    if(!is.null(row_nos_to_suppress)) { # subset rows only if we're working with rows
      df_to_suppress <- df_to_suppress[row_nos_to_suppress,]
    }
  }
  
  # ensure we don't run into any errors...
  df_to_suppress <- df_to_suppress %>% 
    dplyr::mutate_if(is.numeric, as.double) %>% 
    dplyr::as_tibble()
  # stop the code from running if we run into mismatches between data types
  if(!all(sapply(df_to_suppress, typeof) == "double")) {
    stop("Not all of our subset df's columns are doubles. Please review the data being input before continuing.")
  }
  
  # apply pseudo suppression to columns/rows if required
  if(!is.null(columns_to_pseudo_suppress)) { # i.e. not null
    # this needs to go first as it doesn't take df_to_suppress as an arg
    df_to_suppress <- custom_pseudo_supp(
      df,
      subset_df = df_to_suppress,
      pseudo_col_to_supp = 'total_undeclared'
    )
    
    df_to_suppress <- pseudo_suppress_columns(df,
                                              subset_df = df_to_suppress,
                                              row_nos_to_suppress = row_nos_to_suppress,
                                              columns_to_pseudo_suppress = columns_to_pseudo_suppress,
                                              pseudo_suppression_threshold = pseudo_suppression_threshold,
                                              pseudo_suppress_0,
                                              secondary_suppress_0
    )
    
  }
  
  # apply pseudo suppression to columns/rows if required
  if(!is.null(rows_to_pseudo_suppress)) { # i.e. not null
    df_to_suppress <- pseudo_suppress_rows(df,
                                           subset_df = df_to_suppress,
                                           cols_to_suppress = cols_to_suppress,
                                           rows_to_pseudo_suppress = rows_to_pseudo_suppress,
                                           pseudo_suppression_threshold = pseudo_suppression_threshold,
                                           pseudo_suppress_0)
  }
  
  
  # Apply specific suppression to required locations, based on how whether we have "row", "col" or both
  if("col" %in% where_to_suppress) { # if we have column suppression that needs applying, always apply primary
    if(!suppression_pre_applied) {
      #apply primary suppression and then use these results to apply secondary suppression
      df_to_suppress <- apply_primary_suppression_cols(df_to_suppress, 
                                                       cols_to_suppress,
                                                       suppression_thres,
                                                       suppress_0)
    }
  }
  
  # also check if the user has specified that suppression totals should exceed the threshold, and that ncol > 2
  if((total_suppression | indirect_suppression) & length(cols_to_suppress) > 2) {
    df_to_suppress <- suppress_col_total_below_supp_thres(
      df_to_suppress,
      unsuppressed_df,
      suppression_thres,
      cols_to_suppress = cols_to_suppress, # set cols to suppress on
      rows_to_suppress = row_nos_to_suppress, # set rows to suppress on
      subset_df_along_row_and_col_arguments,
      priority_rows_to_suppress = priority_row_suppression,
      secondary_suppress_0 = secondary_suppress_0,
      running_total_supp = TRUE,
      total_suppression = total_suppression,
      indirect_suppression = indirect_suppression
    )
    # apply row total suppression
    # df_to_suppress <- suppress_row_total_below_supp_thres(
    #   df_to_suppress = df_to_suppress,
    #   unsuppressed_df = unsuppressed_df,
    #   suppression_threshold = suppression_thres,
    #   cols_to_suppress = cols_to_suppress, # set cols to suppress on
    #   rows_to_suppress = row_nos_to_suppress, # set rows to suppress on
    #   subset_df_along_row_and_col_arguments,
    #   secondary_suppress_0 = secondary_suppress_0,
    #   running_total_supp = TRUE
    # )
    
  }
  
  if(
    all("col" == where_to_suppress) & # if we're including secondary suppression, and this is only on columns, apply regular column secondary supp
    inc_secondary_suppression & 
    is.null(priority_row_suppression)
  ) {
    if(is_list(df_to_suppress)) df_to_suppress <- df_to_suppress[[1]]
    df_to_suppress <- df_to_suppress %>% 
      dplyr::mutate_at(cols_to_suppress, 
                       ~apply_secondary_suppression_cols(.x,
                                                         secondary_suppress_0))
  } else if(
    all("row" == where_to_suppress) # if we're only including row suppression, apply primary and secondary suppression according to arguments
  ){
    if(is_list(df_to_suppress)) df_to_suppress <- df_to_suppress[[1]]
    df_to_suppress <- apply_suppression_rows(df_to_suppress, 
                                             cols_to_suppress,
                                             row_nos_to_suppress,
                                             suppression_thres,
                                             suppress_0,
                                             inc_secondary_suppression,
                                             inc_primary_suppression = !suppression_pre_applied,  # invert input
                                             secondary_suppress_0)
  } else if(
    all(c("col", "row") %in% where_to_suppress & inc_secondary_suppression) # otherwise, if we're suppressing both rows and columns, we need to apply some fancier suppression (function family cross-referenced suppression)
    # this looks to check where previous suppression has occurred and attempts to put any required secondary suppression in corresponding gaps, so we don't oversuppress
  ){
    # primary suppression applied in an earlier stage, so just fill in any gaps created
    df_to_suppress <- apply_cross_referenced_suppression(
      df_to_suppress,
      columns_to_suppress = cols_to_suppress,
      priority_row_suppression = row_index_to_use,
      ordered_priority_suppression = ordered_priority_suppression,
      secondary_suppress_0 = secondary_suppress_0,
      total_suppression = total_suppression,
      indirect_suppression = indirect_suppression,
      unsuppressed_df = unsuppressed_df,
      suppression_thres = suppression_thres,
      rows_nos_to_suppress = row_nos_to_suppress # include all rows
    )
  }
  
  # and finally, apply priority row suppression where required...
  if(!is.null(priority_row_suppression) & 
     inc_secondary_suppression == "TRUE" & 
     !all(c("col", "row") %in% where_to_suppress)) { # only run when either only row or only column suppression has been selected
    df_to_suppress <- apply_priority_row_suppression(
      df = df_to_suppress,
      cols_to_suppress = cols_to_suppress,
      row_nos_to_suppress = row_nos_to_suppress,
      row_index = row_index_to_use,
      row_index_ordered_suppression = ordered_priority_suppression
    )
  }
  
  # finally, run a check to ensure that our suppression totals exceeds the suppression threshold (assuming the argument is set to TRUE)
  # this is to ensure that users of the final output can't decipher how many people exist across multiple 
  if(total_suppression | indirect_suppression) {
    if(all("col" == where_to_suppress)) {
      df_to_suppress <- suppress_col_total_below_supp_thres(
        df_to_suppress,
        unsuppressed_df,
        suppression_thres,
        cols_to_suppress = cols_to_suppress, # set cols to suppress on
        rows_to_suppress = row_nos_to_suppress, # set rows to suppress on
        subset_df_along_row_and_col_arguments,
        priority_rows_to_suppress = priority_row_suppression,
        secondary_suppress_0 = secondary_suppress_0,
        indirect_suppression = indirect_suppression
      )
    } else if(all("row" == where_to_suppress)) {
      df_to_suppress <- suppress_row_total_below_supp_thres(
        df_to_suppress,
        unsuppressed_df,
        suppression_thres,
        cols_to_suppress = cols_to_suppress, # set cols to suppress on
        rows_to_suppress = row_nos_to_suppress, # set rows to suppress on
        subset_df_along_row_and_col_arguments,
        indirect_suppression = indirect_suppression
      )
    }
  }
  
  # unlist if required
  if(is_list(df_to_suppress)) df_to_suppress <- df_to_suppress[[1]]
  
  # bind our subset values back onto the original (if required)
  if(subset_df_along_row_and_col_arguments) {
    # subset our df accordingly
    df[row_nos_to_suppress,cols_to_suppress] <- df_to_suppress
  }
    
  #set value to output where suppression has occurred (default is to leave it as is)
  df <- df %>% 
    dplyr::mutate_at(cols_to_suppress,
              ~ifelse(. == 9999999, suppression_output_value, .))
  
  return(df)
}