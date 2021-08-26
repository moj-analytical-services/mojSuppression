# find our "min-value pair". This is effectively the lowest 2/3 values in a single row (or column if used for the row inverted code) from our available list of values
find_min_value_pair <- function(matrix_to_use,
                                value_pair_len = value_pair_len) {
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