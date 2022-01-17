#' Suppress across multiple categories/groups within a single dataframe (with some additional options)
#'
#' @description
#'
#' This function allows users to suppress across multiple groups in a single data.
#' If you find yourself with a df that contains a column with multiple groups that you'd like to individually suppress on
#' you should use this function.
#' As an example, if you had a table with a region column (LONDON, MIDLANDS, etc) and you needed to suppress on each
#' region individually, you could use this function to shortcut the process.
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
#' @export


# function to apply both primary or secondary suppression across a selection of either rows, cols, or both for multiple groups in a single column
# as opposed to the "suppress_single_group" function, this should be used for "tidy data" (data with few columns, but many rows)
# more info on tidy data can be found here - https://cran.r-project.org/web/packages/tidyr/vignettes/tidy-data.html
# this allows the user to suppress before putting their data into its final format
# if required, multiple grouping columns can be selected to suppress along. 
# See the examples section for more info
suppress_tidy_data <- function(df, # specify dataframe to suppress on
                               where_to_suppress = c("col"), # which orientation to suppress on. For both row and column supp: c("row", "col")
                               cols_to_suppress, # columns to suppress on. These should be presented like so: c("col1", "col2", ...)
                               groups_to_suppress_across, # groups to suppress across. 
                               suppression_pre_applied = FALSE, # if this function is being used after suppression has already been applied, skip on primary suppression
                               # this should primarily be used in conjunction with suppress_single_group on a df that's been binded together after initial suppression has been applied
                               suppression_thres, # this is the threshold for suppression. Values equal to or below this value will be suppressed
                               suppress_0 = TRUE, # whether to suppress 0s or not. TRUE is the default and will suppress all 0s
                               inc_secondary_suppression = TRUE, # flag to select whether primary or secondary suppression is applied. Secondary supp is the default.
                               suppression_output_value = 9999999, #set a value to output where suppression has occurred
                               total_suppression = FALSE, # set to TRUE if you don't want your suppression totals for rows or columns to fall below your suppression threshold. i.e. if you suppression two 1s in a column, this will suppress a third value for you
                               indirect_suppression = FALSE,
                               secondary_suppress_0 = TRUE # set to FALSE to prevent 0s from taking precendence for suppression (though will be suppressed if it's the only option)
                               # this argument currently has no impact on the cross suppression used throughout. If you opt to suppress on both rows and cols, some 0s may be suppressed in the process
                               
) {
  
  #create a new "concat" field which combines all of our groups to suppress on
  concat_vals <- groups_to_suppress_across %>% 
    purrr::map(~df[[.x]])
  df <- df %>% 
    dplyr::mutate(concat = reduce(concat_vals, stringr::str_c))
  
  #find the groups we need to suppress across
  suppression_groups <- unique(df[["concat"]])
  # loop around our suppression groups and apply suppression. Then, bind this all back together
  if(any("row" %in% where_to_suppress)) { suppress_rows <- TRUE } else { suppress_rows <- FALSE }
  suppressed_df <- suppression_groups %>% 
    purrr::map_df(~ filter_supp_function(df, "concat", .,
                                  where_to_suppress,
                                  cols_to_suppress,
                                  suppress_rows,
                                  suppression_thres,
                                  suppress_0,
                                  inc_secondary_suppression,
                                  suppression_pre_applied,
                                  total_suppression,
                                  indirect_suppression,
                                  secondary_suppress_0 = secondary_suppress_0
    ))
  
  #remove our new concat column
  suppressed_df <- suppressed_df %>% dplyr::select(-concat)
  
  #set value to output where suppression has occurred (default is to leave it as is)
  suppressed_df <- suppressed_df %>%
    dplyr::mutate_at(cols_to_suppress,
              ~ifelse(. == 9999999, suppression_output_value, .))
  
  return(suppressed_df)
}

# intermediate function to allow us to suppress across multiple groups
# the arguments listed below are used within our final function
filter_supp_function <- function(
  df, filter_col, filter_var, 
  where_to_suppress,
  cols_to_suppress,
  suppress_rows,
  suppression_thres,
  suppress_0 = TRUE,
  inc_secondary_suppression = TRUE,
  suppression_pre_applied = FALSE,
  total_suppression = FALSE,
  indirect_suppression = FALSE,
  secondary_suppress_0 = TRUE
){
  df <- df %>% dplyr::filter_at(filter_col,
                         all_vars(. == filter_var))
  if(suppress_rows) { row_nos_to_suppress <- 1:nrow(df) } else { row_nos_to_suppress <- NULL }
  print(paste0("Suppressing on ", filter_var))
  if(nrow(df) <= 1) {
    stop("Warning, you're attempting to tidy suppress on a df that when filtered has a row length of one. Please check to ensure
         that your backing data is all correct before continuing.")
  }
  df <- df %>% 
    suppress_single_group(., 
                          where_to_suppress = where_to_suppress,
                          cols_to_suppress = cols_to_suppress,
                          row_nos_to_suppress = row_nos_to_suppress,
                          suppression_thres = suppression_thres,
                          suppress_0 = suppress_0,
                          inc_secondary_suppression = inc_secondary_suppression,
                          suppression_pre_applied = suppression_pre_applied,
                          total_suppression = total_suppression,
                          indirect_suppression = indirect_suppression,
                          secondary_suppress_0 = secondary_suppress_0)
  
  return(df)
}
