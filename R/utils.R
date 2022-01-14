# helper function to evaluate lists without matching tibbles/dfs
is_list <- function(object) {
  # is a list object, but is not a df
  return(is.list(object) & !is.data.frame(object))
}

# find our "min-value pair". 
# This is effectively the lowest 2/3 values in a single row 
# (or column if used for the row inverted code) from our available list of values
find_min_value_pair <- function(matrix_to_use,
                                value_pair_len = value_pair_len) {
  # so we get the best pairs, convert any suppressed values back to 
  
  # intiialise our min_value_pairs vector
  min_value_pairs <- numeric()
  for(i in 1:nrow(matrix_to_use)) {
    # find our vector of values to check for min-value pairs
    vec <- matrix_to_use[i,]
    # find all permutations of values and calc minimum value
    perm_set <- gtools::permutations(length(vec), value_pair_len, vec, set = FALSE)
    min_value_pairs <- append(min_value_pairs, unique(min(rowSums(perm_set), na.rm=TRUE)))
  }
  # rename vals so we can find the correct row/col combos
  names(min_value_pairs) <- 1:nrow(matrix_to_use)
  # find min value pairs in our list
  min_value_pairs <- min_value_pairs[min_value_pairs == min(min_value_pairs)]
  
  # if the above still leaves us with multiple options, randomly pick between the two (this essentially signals that the columns we're checking are identical)
  if(length(min_value_pairs)>1) {
    set.seed(55)
    min_value_pairs <- min_value_pairs[sample(1:length(min_value_pairs), 1)]
  }
  return(min_value_pairs)
}

# a flexible version of the above
# this allows us to evaluate what our optimal suppression pairing looks like
# the flexible pairing isn't always optimal, hence why this is a separate func
find_flexible_min_value_pair <- function(matrix) {
  
  # create a seeded sample function...
  seeded_sample <- function(...) {
    set.seed(15)
    sample(...)
  }
  
  second_val_check <- function(min_val_cols) {
    if(!is.null(nrow(min_val_cols))) {
      min_val_cols[seeded_sample(1:nrow(min_val_cols), 1),]
    } else {
      min_val_cols
    }
  }
  
  min_val_cols <- apply(matrix, 2, min)[apply(matrix, 2, min) == min(apply(matrix, 2, min))]
  # need to evaluate len
  if(length(min_val_cols)==1) {
    # we already have val 1...
    first_val <- which(unique(min_val_cols) == matrix, arr.ind=TRUE)
    # look again for the second min value...
    remaining_col_mins <- apply(matrix, 2, min)[apply(matrix, 2, min) > min_val_cols]
    remaining_col_mins <- remaining_col_mins[remaining_col_mins==min(remaining_col_mins)]
    min_val_cols <- which(unique(remaining_col_mins) == matrix, arr.ind=TRUE)
    min_val_cols <- min_val_cols[min_val_cols[,"col"] != first_val[,2],]
    second_val <- second_val_check(min_val_cols)
  } else {
    # if len larger than 1:
    min_val_cols <- which(unique(min_val_cols) == matrix, arr.ind=TRUE)
    first_val <- min_val_cols[seeded_sample(1:nrow(min_val_cols), 1),]
    # update min_val_cols
    min_val_cols <- min_val_cols[min_val_cols[,2]!=first_val[2],]
    second_val <- second_val_check(min_val_cols)
  }
  
  return(rbind(first_val, matrix(second_val, ncol = 2)))
}
