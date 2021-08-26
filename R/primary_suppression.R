# basic function to apply primary suppression to input columns
apply_primary_suppression_cols <- function(
  df, # specify dataframe to suppress on
  column_name, # columns to suppress on. These should be presented like so: c("col1", "col2", ...)
  suppression_thres, # this is the threshold for suppression. Values equal to or below this value will be suppressed
  suppress_0 = TRUE # whether to suppress 0s or not. TRUE is the default and will suppress all 0s
) {
  
  #error if invalid suppression threshold entered
  if(class(suppression_thres) != "numeric"|suppression_thres == Inf|suppression_thres == -Inf) {
    stop("Please enter a valid suppression threshold")
  }
  
  if(suppress_0 == TRUE) {
    #suppress 0s
    df %>% dplyr::mutate_at(column_name,
                     .funs = list(~ifelse(. <=suppression_thres, 9999999, .)))
  } else {
    #leave 0s unsuppressed
    df %>% dplyr::mutate_at(column_name,
                     .funs = list(~ifelse(. <=suppression_thres & . > 0, 9999999, .)))
  }
  
}
