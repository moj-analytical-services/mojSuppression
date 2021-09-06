#' this is the simplified version of suppress_single_group
#' the main aim is to remove some of the potentially superfluous features and streamline the suppression function and
#' total argument list for users

# function to apply both primary or secondary suppression across a selection of either rows, cols, or both
# this should be used on standard dfs where data is already in its final form 
# if your data is in a longer format (column headers are in a single column, for example), please see the function below

#' The `simple_suppression()` function can be used to apply both primary or 
#' secondary suppression across a selection of either rows, cols, or both,
#' suppressing all values below a specified suppression threshold.
#' This funtion expects that the input be your final dataframe that you wish to suppress on.
#' Please filter down your existing work, should it not meet this criteria.
#' 
#' The `simple_suppression()` function applies suppression to either rows, columns or both.
#' This is the simplified version of suppress_single_group.
#' The main aim is to remove some of the potentially superfluous features and streamline argument list for users
#' If you need any of the following rules, please see the `suppress_single_group()` function instead:
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
#' mojSuppression::simple_suppression(
#'     df = df,
#'     where_to_suppress = c('col', 'row'),
#'     suppression_thres = 2
#' )
#' 

simple_suppression <- function(
  df,
  where_to_suppress = c("col"),
  cols_to_suppress = NULL,
  row_nos_to_suppress = NULL,
  suppression_thres,
  suppress_0 = TRUE, 
  inc_secondary_suppression = TRUE,
  suppression_output_value = 9999999,
  secondary_suppress_0 = TRUE
) {
  
  # set defaults for various removed features
  columns_to_pseudo_suppress = NULL
  rows_to_pseudo_suppress = NULL
  pseudo_suppression_threshold = NULL
  pseudo_suppress_0 = NULL
  priority_row_suppression = NULL
  ordered_priority_suppression = FALSE
  suppression_pre_applied = FALSE
  suppression_total_suppression = FALSE
  subset_df_along_row_and_col_arguments = TRUE 
  
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
  
  if(
    all("col" == where_to_suppress) & # if we're including secondary suppression, and this is only on columns, apply regular column secondary supp
    inc_secondary_suppression & 
    is.null(priority_row_suppression)
  ) {
    df_to_suppress <- df_to_suppress %>% 
      dplyr::mutate_at(cols_to_suppress, 
                       ~apply_secondary_suppression_cols(.x,
                                                         secondary_suppress_0))
  } else if(
    all("row" == where_to_suppress) # if we're only including row suppression, apply primary and secondary suppression according to arguments
  ){
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
    df_to_suppress <- apply_cross_referenced_suppression(df_to_suppress,
                                                         columns_to_suppress = cols_to_suppress,
                                                         priority_row_suppression = row_index_to_use,
                                                         ordered_priority_suppression = ordered_priority_suppression,
                                                         secondary_suppress_0 = secondary_suppress_0)
  }
  
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