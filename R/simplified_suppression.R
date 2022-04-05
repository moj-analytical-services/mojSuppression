#' Suppress an input dataframe
#'
#' @description
#'
#' This is the simplified version of suppress_single_group. The main aim is to remove some of the potentially 
#' superfluous features and streamline the suppression function and reduce the total argument list for users.
#' This fuction offers a way for users to suppression rows, columns or both rows and columns simultaneously.
#'
#' @details
#' The `simple_suppression()` function can be used to apply both primary or 
#' secondary suppression across a selection of either rows, columns or both simultaneously,
#' suppressing all values below a specified suppression threshold.
#' This funtion expects that the input be your final dataframe that you wish to suppress on.
#' Please filter down your existing work, should it not meet this criteria.
#' 
#' This is the simplified version of suppress_single_group.
#' The main aim is to remove some of the potentially superfluous features and streamline the argument list for users.
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
#' @include apply_single_group_suppression.R
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
  suppress_0 = FALSE, 
  inc_secondary_suppression = TRUE,
  suppression_output_value = 9999999,
  secondary_suppress_0 = TRUE
) {
  
  # set defaults for various removed features
  subset_df_along_row_and_col_arguments = TRUE 
  columns_to_pseudo_suppress = NULL
  rows_to_pseudo_suppress = NULL
  pseudo_suppression_threshold = NULL
  pseudo_suppress_0 = NULL
  priority_row_suppression = NULL
  ordered_priority_suppression = FALSE
  suppression_pre_applied = FALSE
  total_suppression = FALSE
  
  # use our single group suppression and fill in some
  # arguments automatically
  df <- suppress_single_group(
    df,
    where_to_suppress = where_to_suppress,
    cols_to_suppress = NULL,
    row_nos_to_suppress = NULL,
    suppression_thres,
    suppress_0 = TRUE,
    inc_secondary_suppression = TRUE,
    suppression_output_value = suppression_output_value,
    subset_df_along_row_and_col_arguments = subset_df_along_row_and_col_arguments,
    columns_to_pseudo_suppress = columns_to_pseudo_suppress,
    rows_to_pseudo_suppress = rows_to_pseudo_suppress,
    pseudo_suppression_threshold = pseudo_suppression_threshold,
    pseudo_suppress_0 = pseudo_suppress_0,
    priority_row_suppression = priority_row_suppression,
    ordered_priority_suppression = ordered_priority_suppression,
    suppression_pre_applied = suppression_pre_applied,
    total_suppression = total_suppression,
    indirect_suppression = FALSE,
    secondary_suppress_0 = TRUE
  )
  
  return(df)
}
