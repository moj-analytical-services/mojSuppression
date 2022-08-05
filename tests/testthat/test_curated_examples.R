library(testthat)
sapply(list.files(path="R/", pattern="*.R", full.names = TRUE),source,.GlobalEnv)

#### ======= TEST PSEUDO SUPPRESSION ======= ####
# # Outstanding - row and col pseudo supp
# testthat::test_that('Our custom pseudo supp code for this project is working as expected',
#                     {
#                       
#                       test_func <- tibble(
#                         x = c(3, 3, 3, 3),
#                         y = c(3, 3, 3, 0),
#                         pnts = c(0, 0, 0 ,0)
#                       )
#                       suppression_cols <- c('x', 'y')
#                       
#                       testthat::expect_equal(
#                         pseudo_suppress_rows(
#                           test_func, 
#                           test_func[suppression_cols],
#                           pseudo_col_to_supp = 'pnts'
#                         ),
#                         tibble(x = c(3, 3, 3, 3), y = c(3,3,3,9999999)
#                         )
#                       )
#                       
#                       test_func <- tibble(
#                         x = c(3, 3, 0, 3),
#                         y = c(0, 3, 3, 3),
#                         pnts = c(0, 0, 0 ,0)
#                       )
#                       
#                       suppression_cols <- c('x', 'y')
#                       testthat::expect_equal(
#                         pseudo_suppress_rows(
#                           test_func, 
#                           test_func[suppression_cols],
#                           pseudo_col_to_supp = 'pnts'
#                         ),
#                         tibble(x = c(3, 3, 9999999, 3), y = c(9999999,3,3,3)
#                         )
#                       )
#                     }
# )


#### ====== TEST ROW PRIORITY CODE ===== ####
testthat::test_that("Our priority suppression is working under various different examples", {
  
  a <- tibble(x = c(5, 3, 2, 9, 10, 11),
              y = c(6, 7, 8, 9, 20, 22
              ))
  ## TESTING PRIORITY ROW SUPPRESSION 
  # check that our code correctly errors out where only one column is supplied, but row suppression is requested
  testthat::expect_error(suppress_single_group(a, 
                                               where_to_suppress = c("col", "row"),
                                               cols_to_suppress = c("x"), #if you want all numeric columns to be suppressed, you can copy this line
                                               row_nos_to_suppress = 1:nrow(a),
                                               suppression_thres = 2, #suppress values equal to, or below 2
                                               suppress_0 = FALSE))
  
  # check that our code correctly priority suppresses row 5
  testthat::expect_equal(suppress_single_group(a, 
                                               where_to_suppress = c("col"),
                                               cols_to_suppress = c("x", "y"), #if you want all numeric columns to be suppressed, you can copy this line
                                               row_nos_to_suppress = 1:nrow(a),
                                               suppression_thres = 2, #suppress values equal to, or below 2
                                               suppress_0 = FALSE,
                                               priority_row_suppression = 5),
                         tibble(x = c(5, 3, 9999999, 9, 9999999, 11),
                                y = c(6, 7, 8, 9, 20, 22
                                ))
  )
  
  
  # check our results are correct where we're supplying priority row suppression = 5, and restricting our rows for suppression
  # i.e. the code allows for this
  testthat::expect_equal(
    suppress_single_group(a, 
                          where_to_suppress = c("col", "row"),
                          cols_to_suppress = c("x", "y"), #if you want all numeric columns to be suppressed, you can copy this line
                          row_nos_to_suppress = 3:nrow(a),
                          suppression_thres = 2, #suppress values equal to, or below 2
                          suppress_0 = FALSE,
                          priority_row_suppression = 5),
    tibble(x = c(5, 3, 9999999, 9, 9999999, 11),
           y = c(6, 7, 9999999, 9, 9999999, 22
           ))
  )
  
  # check what happens when we enter a priority row outside of our eligible range
  testthat::expect_error(
    suppress_single_group(a, 
                          where_to_suppress = c("col", "row"),
                          cols_to_suppress = c("x", "y"), #if you want all numeric columns to be suppressed, you can copy this line
                          row_nos_to_suppress = 3:nrow(a),
                          suppression_thres = 2, #suppress values equal to, or below 2
                          suppress_0 = FALSE,
                          priority_row_suppression = c(1))
  )
  # and check what happens when we give it a row that's invalid, and one that is valid... (it should work)
  testthat::expect_equal(
    suppress_single_group(a, 
                          where_to_suppress = c("col", "row"),
                          cols_to_suppress = c("x", "y"), #if you want all numeric columns to be suppressed, you can copy this line
                          row_nos_to_suppress = 3:nrow(a),
                          suppression_thres = 2, #suppress values equal to, or below 2
                          suppress_0 = FALSE,
                          priority_row_suppression = c(1, 5)),
    tibble(x = c(5, 3, 9999999, 9, 9999999, 11),
           y = c(6, 7, 9999999, 9, 9999999, 22
           ))
  )
  
  # check what happens when we give it a multiple valid entries (the lowest value should be suppressed)
  testthat::expect_equal(
    suppress_single_group(a, 
                          where_to_suppress = c("col", "row"),
                          cols_to_suppress = c("x", "y"), #if you want all numeric columns to be suppressed, you can copy this line
                          row_nos_to_suppress = 3:nrow(a),
                          suppression_thres = 2, #suppress values equal to, or below 2
                          suppress_0 = FALSE,
                          priority_row_suppression = c(3, 4, 5)),
    tibble(x = c(5, 3, 9999999, 9999999, 10, 11),
           y = c(6, 7, 9999999, 9999999, 20, 22
           ))
  )
  
  # same as above, but now introducing our ordered priority suppression. We expect row 6 to be suppressed first
  testthat::expect_equal(
    suppress_single_group(a, 
                          where_to_suppress = c("col", "row"),
                          cols_to_suppress = c("x", "y"), #if you want all numeric columns to be suppressed, you can copy this line
                          row_nos_to_suppress = 3:nrow(a),
                          suppression_thres = 2, #suppress values equal to, or below 2
                          suppress_0 = FALSE,
                          priority_row_suppression = c(6, 3, 4, 5),
                          ordered_priority_suppression = TRUE),
    tibble(x = c(5, 3, 9999999, 9, 10, 9999999),
           y = c(6, 7, 9999999, 9, 20, 9999999
           ))
  )
  # now check what happens when the first entry in our priority row supp list has already been suppressed
  testthat::expect_equal(
    suppress_single_group(a, 
                          where_to_suppress = c("col", "row"),
                          cols_to_suppress = c("x", "y"), #if you want all numeric columns to be suppressed, you can copy this line
                          row_nos_to_suppress = 3:nrow(a),
                          suppression_thres = 2, #suppress values equal to, or below 2
                          suppress_0 = FALSE,
                          priority_row_suppression = c(6, 3, 4, 5),
                          ordered_priority_suppression = TRUE),
    tibble(x = c(5, 3, 9999999, 9, 10, 9999999),
           y = c(6, 7, 9999999, 9, 20, 9999999
           ))
  )
  # some other odd entries that could happen
  testthat::expect_equal(
    suppress_single_group(a, 
                          where_to_suppress = c("col", "row"),
                          cols_to_suppress = c("x", "y"), #if you want all numeric columns to be suppressed, you can copy this line
                          row_nos_to_suppress = 3:nrow(a),
                          suppression_thres = 2, #suppress values equal to, or below 2
                          suppress_0 = FALSE,
                          priority_row_suppression = c(3, 6, 4, 6),
                          ordered_priority_suppression = TRUE),
    tibble(x = c(5, 3, 9999999, 9, 10, 9999999),
           y = c(6, 7, 9999999, 9, 20, 9999999
           ))
  )
  
  # here we've just spammed a bunch of 3s... 3 is the first point of suppression, so just check the code correctly suppresses another
  # column
  testthat::expect_equal(
    suppress_single_group(a, 
                          where_to_suppress = c("col", "row"),
                          cols_to_suppress = c("x", "y"), #if you want all numeric columns to be suppressed, you can copy this line
                          row_nos_to_suppress = 3:nrow(a),
                          suppression_thres = 2, #suppress values equal to, or below 2
                          suppress_0 = FALSE,
                          priority_row_suppression = c(3, 3, 3, 3),
                          ordered_priority_suppression = TRUE),
    tibble(x = c(5, 3, 9999999, 9999999, 10, 11),
           y = c(6, 7, 9999999, 9999999, 20, 22
           ))
  )
})

#### ======= CROSS CHECKER TESTING ======= ####
testthat::test_that("Check that our suppression cross checker is working as expected (i.e. it suppresses the correct values, given a variety of examples)", {
  
  # EXAMPLE 1 - NICE SIMPLE 3X4 GRID
  bme <- c(25, 9999999, 10, 9999999)
  white <- c(10, 6, 10, 9999999)
  other <- c(9999999, 5, 10, 10)
  
  cross_df <- tibble(bme = bme,
                     white = white,
                     other = other)
  
  cols_to_supp <- c("bme", "white", "other")
  # normal suppression
  testthat::expect_equal(apply_cross_referenced_suppression(cross_df, 
                                                            cols_to_supp),
                         tibble(bme = c(25, 9999999,10, 9999999),
                                white = c(9999999, 6, 10, 9999999),
                                other = c(9999999, 9999999, 10, 10))
  )
  
  # EXAMPLE 2 - ANOTHER VARIATION OF THE ABOVE EXAMPLE
  bme <- c(25, 4, 10, 9999999)
  white <- c(10, 6, 10, 9999999)
  other <- c(9999999, 5, 10, 10)
  
  cross_df <- tibble(bme = bme,
                     white = white,
                     other = other)
  
  cols_to_supp <- c("bme", "white", "other")
  # normal suppression
  testthat::expect_equal(apply_cross_referenced_suppression(cross_df, 
                                                            cols_to_supp),
                         tibble(bme = c(25, 9999999,10, 9999999),
                                white = c(9999999, 6, 10, 9999999),
                                other = c(9999999, 9999999, 10, 10))
  )
  
  # EXAMPLE 3 - NICE SIMPLE EXAMPLE (Most normal case)
  bme <- c(25, 9999999, 10, 15)
  white <- c(10, 9999999, 6, 9999999)
  other <- c(2, 10, 10, 10)
  
  cross_df <- tibble(bme = bme,
                     white = white,
                     other = other)
  
  cols_to_supp <- c("bme", "white", "other")
  # normal suppression
  testthat::expect_equal(apply_cross_referenced_suppression(cross_df, 
                                                            cols_to_supp),
                         tibble(bme = c(25, 9999999,10, 9999999),
                                white = c(10, 9999999, 6, 9999999),
                                other = c(2, 10, 10, 10))
  )
  
  
  
  
  # EXAMPLE 5 - THE MOST CONFUSING CASE OF SUPPRESSION
  bme <- c(25, 9999999, 9999999, 10)
  white <- c(10, 6, 10, 9999999)
  other <- c(9999999, 5, 10, 10)
  test <- c(5, 6, 1, 2)
  
  cross_df <- tibble(bme = bme,
                     white = white,
                     other = other,
                     test = test)
  cols_to_supp <- c("bme", "white", "other", "test")
  # normal suppression
  testthat::expect_equal(apply_cross_referenced_suppression(cross_df, 
                                                            cols_to_supp),
                         tibble(bme = c(25, 9999999, 9999999, 10),
                                white = c(9999999, 6, 10, 9999999),
                                other = c(9999999, 9999999, 10, 10),
                                test = c(5, 6, 9999999, 9999999))
  )
  
  
  
  bme <- c(25, 5, 9999999, 9999999)
  white <- c(10, 6, 5, 9999999)
  other <- c(5, 5, 2, 9999999)
  test <- c(5, 5, 2, 9999999)
  scale_up <- c(5, 5, 4, 9999999)
  
  cross_df <- tibble(
    bme = bme,
    white = white,
    other = other,
    test = test,
    scaleup = scale_up)
  
  cols_to_supp <- c("bme", "white", "other", "test", "scaleup")
  # normal suppression
  testthat::expect_equal(apply_cross_referenced_suppression(cross_df, 
                                                            cols_to_supp),
                         tibble(bme = c(25, 5, 9999999, 9999999),
                                white = c(10, 6, 9999999, 9999999),
                                other = c(5, 5, 9999999, 9999999),
                                test = c(5, 5, 9999999, 9999999),
                                scaleup = c(5, 5, 9999999, 9999999))
  )
  
  bme <- c(6, 5, 9999999, 9999999)
  white <- c(2, 6, 5, 9999999)
  other <- c(5, 5, 2, 9999999)
  test <- c(5, 5, 2, 9999999)
  scale_up <- c(5, 5, 4, 9999999)
  
  cross_df <- tibble(
    bme = bme,
    white = white,
    other = other,
    test = test,
    scaleup = scale_up)
  
  cols_to_supp <- c("bme", "white", "other", "test", "scaleup")
  # normal suppression
  testthat::expect_equal(apply_cross_referenced_suppression(cross_df, 
                                                            cols_to_supp),
                         tibble(bme = c(6, 5, 9999999, 9999999),
                                white = c(9999999, 6, 5, 9999999),
                                other = c(5, 5, 9999999, 9999999),
                                test = c(5, 5, 9999999, 9999999),
                                scaleup = c(9999999, 5, 4, 9999999))
  )
  ## TESTING CODE W/ PRIORITY SUPPRESSION
  testthat::expect_equal(apply_cross_referenced_suppression(cross_df, 
                                                            cols_to_supp, 
                                                            ordered_priority_suppression = FALSE),
                         tibble(bme = c(6, 5, 9999999, 9999999),
                                white = c(9999999, 6, 5, 9999999),
                                other = c(5, 5, 9999999, 9999999),
                                test = c(5, 5, 9999999, 9999999),
                                scaleup = c(9999999, 5, 4, 9999999))
  )
  
  bme <- c(25, 5, 4, 9999999)
  white <- c(10, 6, 5, 4)
  other <- c(5, 5, 2, 4)
  test <- c(5, 5, 2, 4)
  scale_up <- c(5, 4, 4, 4)
  
  cross_df <- tibble(
    bme = bme,
    white = white,
    other = other,
    test = test,
    scaleup = scale_up)
  
  cols_to_supp <- c("bme", "white", "other", "test", "scaleup")
  # normal suppression - # as random number generator is set to a specific seed, result should always be consistent
  # otherwise we'd expect suppression to alternate between other and test
  testthat::expect_equal(apply_cross_referenced_suppression(cross_df, 
                                                            cols_to_supp),
                         tibble(bme = c(25, 5, 9999999, 9999999),
                                white = c(10, 6, 5, 4),
                                other = c(5, 5, 2, 4),
                                test = c(5, 5, 9999999, 9999999),
                                scaleup = c(5, 4, 4, 4))
  )
  
  ## Check our outputs are correct when priority suppression is implemented
  testthat::expect_equal(apply_cross_referenced_suppression(cross_df, 
                                                            cols_to_supp, 
                                                            priority_row_suppression = c(1, 2, 3), 
                                                            ordered_priority_suppression = TRUE),
                         tibble(bme = c(9999999, 5, 4, 9999999),
                                white = c(10, 6, 5, 4),
                                other = c(5, 5, 2, 4),
                                test = c(9999999, 5, 2, 9999999),
                                scaleup = c(5, 4, 4, 4))
  )
  
  # need to look into this (seems to be oversuppressing)
  testthat::expect_equal(apply_cross_referenced_suppression(cross_df,
                                                            cols_to_supp,
                                                            priority_row_suppression = c(1, 2),
                                                            ordered_priority_suppression = FALSE),
                         tibble(bme = c(25, 9999999, 4, 9999999),
                                white = c(10, 6, 5, 4),
                                other = c(5, 5, 2, 4),
                                test = c(5, 5, 2, 4),
                                scaleup = c(5, 9999999, 4, 9999999))
  )
  
  
  bme <- c(25, 9999999, 9999999, 9999999)
  white <- c(10, 6, 5, 4)
  other <- c(5, 5, 3, 4)
  test <- c(5, 5, 2, 4)
  scale_up <- c(5, 4, 4, 4)
  
  cross_df <- tibble(
    bme = bme,
    white = white,
    other = other,
    test = test,
    scaleup = scale_up)
  
  cols_to_supp <- c("bme", "white", "other", "test", "scaleup")
  # normal suppression
  testthat::expect_equal(
    as_tibble(apply_cross_referenced_suppression(cross_df, 
                                                 cols_to_supp)),
    tibble(bme = c(25, 9999999, 9999999, 9999999),
           white = white,
           other = other,
           test = c(5, 9999999, 9999999, 9999999),
           scaleup = scale_up)
  )
  
  other <- c(5, 5, 1, 4)
  cross_df$other <- other
  
  testthat::expect_equal(
    as_tibble(apply_cross_referenced_suppression(cross_df, 
                                                 cols_to_supp)),
    tibble(bme = c(25, 9999999, 9999999, 9999999),
           white = white,
           other = c(5, 9999999, 9999999, 9999999),
           test = test,
           scaleup = scale_up)
  )
  
  
  bme <- c(25, 5, 5, 5)
  white <- c(10, 6, 5, 4)
  other <- c(5, 5, 2, 4)
  test <- c(5, 5, 2, 4)
  scale_up <- c(5, 4, 4, 4)
  
  cross_df <- tibble(
    bme = bme,
    white = white,
    other = other,
    test = test,
    scaleup = scale_up)
  
  cols_to_supp <- c("bme", "white", "other", "test", "scaleup")
  # normal suppression
  testthat::expect_equal(apply_cross_referenced_suppression(cross_df, 
                                                            cols_to_supp),
                         tibble(bme = c(25, 5, 5, 5),
                                white = c(10, 6, 5, 4),
                                other = c(5, 5, 2, 4),
                                test = c(5, 5, 2, 4),
                                scaleup = c(5, 4, 4, 4))
  )
  
  bme <- c(25, 5, 4, 9999999)
  white <- c(10, 6, 5, 4)
  
  cross_df <- tibble(
    bme = bme,
    white = white)
  
  testthat::expect_equal(
    apply_cross_reference_suppression_to_cols(
      list(cross_df),
      c("bme", "white")
    ),
    list(dplyr::as_tibble(cross_df))
  )
  
  
  ### test our secondary_min_row finder is working as expected
  # more testing required here, ideally
  a <- c(9999999, 33, 2, 6)
  b <- c(55, 9999999, 5, 8)
  c <- c(22, 2, 9999999, 2)
  d <- c(9, 8, 6, 9999999)
  
  df <- tibble(
    a = a,
    b = b,
    c = c,
    d = d)
  
  name_1 <- "colname ~ c; rowref ~ 2"
  name_2 <- "colname ~ c; rowref ~ 4"
  column_data_potential_suppression <- c(2, 2)
  names(column_data_potential_suppression) <- c(name_1, name_2)
  
  testthat::expect_equal(
    column_data_potential_suppression[find_secondary_min_row(
      column_data_potential_suppression,
      df,
      c("a", "b", "c", "d")
    )], column_data_potential_suppression[1]
  )
  
  
  # trial the code where no row has a maximum value
  # in theory, it should randomly select one, as there's no easy way to determine which is the better choice for suppression here (both offer the same end result)
  a <- c(9999999, 6, 2, 6)
  b <- c(55, 9999999, 5, 8)
  c <- c(22, 2, 9999999, 2)
  d <- c(9, 8, 6, 9999999)
  
  df <- tibble(
    a = a,
    b = b,
    c = c,
    d = d)
  
  testthat::expect_equal(
    column_data_potential_suppression[find_secondary_min_row(
      column_data_potential_suppression,
      df,
      c("a", "b", "c", "d")
    )], column_data_potential_suppression[2]
  )
  
  # trial the code whereby we have row 4 with two matching values with row 2, but with the third value being lower
  a <- c(9999999, 6, 2, 6)
  b <- c(55, 9999999, 5, 7)
  c <- c(22, 2, 9999999, 2)
  d <- c(9, 8, 6, 9999999)
  
  df <- tibble(
    a = a,
    b = b,
    c = c,
    d = d)
  
  testthat::expect_equal(
    column_data_potential_suppression[find_secondary_min_row(
      column_data_potential_suppression,
      df,
      c("a", "b", "c", "d")
    )], column_data_potential_suppression[1]
  )
  
  # slightly more complex outputs expected
  a <- c(9999999, 33, 2, 6)
  b <- c(55, 9999999, 5, 8)
  c <- c(22, 2, 9999999, 2)
  d <- c(9, 8, 6, 9999999)
  
  cross_df <- tibble(
    a = a,
    b = b,
    c = c,
    d = d)
  
  
  columns_to_supp <- c("a", "b", "c", "d")
  
  testthat::expect_equal(
    apply_cross_referenced_suppression(cross_df,
                                       columns_to_supp
    ),
    tibble(a = c(9999999, 33, 9999999, 6),
           b = c(55, 9999999, 5, 9999999),
           c = c(22, 9999999, 9999999, 2),
           d = c(9999999, 8, 6, 9999999))
  )
  
  
  
})


# To add ------------------------------------------------------------------

### MIN VALUE PAIR WORK
ma <- tibble(x = c(6,2,3,1,1),
             y = c(2,3,1,5,6),
             z = c(11,5,13,8,4),
             a = c(12,1,7,2,12)) %>% as.matrix()

testthat::expect_equal(
  as.vector(find_flexible_min_value_pair(ma)),
  c(4,3,1,2)
)
test_matrix <- tibble(x = c(4,5,3,2), y = c(1,4,6,2), z = c(5,3,7,3)) %>% as.matrix()
testthat::expect_equal(
  as.vector(find_flexible_min_value_pair(test_matrix)),
  c(1,4,2,1)
)
test_matrix <- tibble(x = c(4,5,3,2), y = c(1,4,6,2), z = c(5,3,7,1)) %>% as.matrix()
testthat::expect_equal(
  as.vector(find_flexible_min_value_pair(test_matrix)),
  c(1,4,2,3)
)


t <- data.frame(
  a = c(4, 26, 0, 4),
  b = c(4, 1, 0, 1),
  c = c(7, 9, 1, 2)
)

testthat::expect_equal(
  suppress_single_group(
    df = t,
    c("col", "row"),
    suppression_thres = 2,
    suppress_0 = FALSE,
    suppression_total_suppression = TRUE,
    secondary_suppress_0 = FALSE
  ),
  data.frame(
    a = c(4, 26, 0, 4),
    b = c(9999999, 9999999, 9999999, 9999999),
    c = c(9999999, 9999999, 9999999, 9999999)
  )
)

t <- data.frame(
  a = c(4, 26, 3, 4),
  b = c(4, 1, 0, 1),
  c = c(7, 9, 1, 2)
)

testthat::expect_equal(
  suppress_single_group(
    df = t,
    c("col", "row"),
    suppression_thres = 2,
    suppress_0 = FALSE,
    suppression_total_suppression = FALSE,
    secondary_suppress_0 = FALSE
  ),
  data.frame(
    a = c(4, 9999999, 9999999, 4),
    b = c(4, 9999999, 0, 9999999),
    c = c(7, 9, 9999999, 9999999)
  )
)

# turn all things in broken examples into tests...
testthat::expect_equal(
  suppress_single_group(
    df = t,
    c("col", "row"),
    suppression_thres = 2,
    suppress_0 = FALSE,
    suppression_total_suppression = FALSE,
    secondary_suppress_0 = TRUE
  ),
  data.frame(
    a = c(4, 26, 3, 4),
    b = c(4, 9999999, 9999999, 9999999),
    c = c(7, 9999999, 9999999, 9999999)
  )
)

# turn all things in broken examples into tests...
testthat::expect_equal(
  suppress_single_group(
    df = t,
    c("col", "row"),
    suppression_thres = 2,
    suppress_0 = FALSE,
    suppression_total_suppression = TRUE,
    secondary_suppress_0 = TRUE
  ),
  data.frame(
    a = c(4, 26, 3, 4),
    b = c(9999999, 9999999, 9999999, 9999999),
    c = c(9999999, 9999999, 9999999, 9999999)
  )
)


t <- data.frame(
  a = c(26, 0, 4),
  b = c(2, 0, 1),
  c = c(9, 1, 2)
)

testthat::expect_equal(
  suppress_single_group(
    df = t,
    c("col", "row"),
    suppression_thres = 2,
    suppress_0 = FALSE,
    suppression_total_suppression = TRUE,
    secondary_suppress_0 = FALSE
  ),
  data.frame(
    a = c(26, 0, 4),
    b = rep(9999999, 3),
    c = rep(9999999, 3)
  )
)

t[2,1] <- 3 # converting an existing 0 to 3 changes how the input is manipulated (as it should...)

testthat::expect_equal(
  suppress_single_group(
    df = t,
    c("col", "row"),
    suppression_thres = 2,
    suppress_0 = FALSE,
    suppression_total_suppression = TRUE,
    secondary_suppress_0 = FALSE
  ),
  data.frame(
    a = c(9999999, 9999999, 4),
    b = c(9999999, 0, 9999999),
    c = c(9, 9999999, 9999999)
  )
)

### Some simpler examples to check!
a <- tibble(
  x = c(1, 0, 1, 3),
  y = c(2, 2, 7, 9),
  z = c(2, 5, 8, 2)
)

b <- tibble(
  x = c(1, 3, 1, 5),
  y = c(2, 2, 7, 9),
  z = c(2, 5, 8, 2)
)

testthat::expect_equal(
  suppress_single_group(a,
                        where_to_suppress = c("col", "row"),
                        cols_to_suppress =  c("x", "y", "z"), #if you want all numeric columns to be suppressed, you can copy this line
                        row_nos_to_suppress = 1:4,
                        suppression_thres = 2, #suppress values equal to, or below 2
                        suppress_0 = FALSE,
                        suppression_total_suppression = FALSE,
                        secondary_suppress_0 = FALSE),
  dplyr::tibble(x = c(9999999, 0, 9999999, 9999999),
                y = c(9999999, 9999999, 9999999, 9),
                z = c(9999999, 9999999, 8, 9999999))
)


testthat::expect_equal(
  suppress_single_group(b,
                        where_to_suppress = c("col", "row"),
                        cols_to_suppress =  c("x", "y", "z"), #if you want all numeric columns to be suppressed, you can copy this line
                        row_nos_to_suppress = 1:4,
                        suppression_thres = 2, #suppress values equal to, or below 2
                        suppress_0 = FALSE,
                        suppression_total_suppression = FALSE,
                        secondary_suppress_0 = FALSE),
  dplyr::tibble(x = c(9999999, 9999999, 9999999, 9999999),
                y = c(9999999, 9999999, 9999999, 9),
                z = c(9999999, 5, 8, 9999999))
)

b[2,1] <- 6

testthat::expect_equal(
  suppress_single_group(b,
                        where_to_suppress = c("col", "row"),
                        cols_to_suppress =  c("x", "y", "z"), #if you want all numeric columns to be suppressed, you can copy this line
                        row_nos_to_suppress = 1:4,
                        suppression_thres = 2, #suppress values equal to, or below 2
                        suppress_0 = FALSE,
                        suppression_total_suppression = FALSE,
                        secondary_suppress_0 = FALSE),
  dplyr::tibble(x = c(9999999, 6, 9999999, 9999999),
                y = c(9999999, 9999999, 9999999, 9),
                z = c(9999999, 9999999, 8, 9999999))
)


suppress_single_group(b,
                      where_to_suppress = c("col", "row"),
                      cols_to_suppress =  c("x", "y", "z"), #if you want all numeric columns to be suppressed, you can copy this line
                      row_nos_to_suppress = 1:4,
                      suppression_thres = 2, #suppress values equal to, or below 2
                      suppress_0 = FALSE,
                      suppression_total_suppression = TRUE,
                      secondary_suppress_0 = FALSE)

# A more complex example...
a <- tibble(
  x = c(1, 3, 1, 5, 1, 6),
  y = c(2, 2, 7, 9, 7, 10),
  z = c(2, 5, 8, 2, 2, 11),
  a = c(5, 6, 2, 4, 1, 10),
  b = c(5, 5, 5, 5, 5, 5)
)

testthat::expect_equal(
  suppress_single_group(
    df = a,
    c("col", "row"),
    suppression_thres = 2,
    suppress_0 = FALSE,
    suppression_total_suppression = FALSE,
    secondary_suppress_0 = FALSE
  ),
  dplyr::tibble(
    x = c(9999999, 9999999, 9999999, 5, 9999999, 6),
    y = c(9999999, 9999999, 7, 9, 7, 10),
    z = c(9999999, 5, 8, 9999999, 9999999, 11),
    a = c(5, 6, 9999999, 9999999, 9999999, 10),
    b = c(5, 5, 5, 5, 5, 5)
  )
)



# a more real world style example
test <- tibble(
  a = c(1, 10, 15, 100, 100, 165, 1,0),
  b = c(1, 4, 8, 20, 100, 200, 0,0),
  c = c(1, 5, 10,45,100,100,1,0)
)

testthat::expect_equal(
  suppress_single_group(
    df = test,
    c("col", "row"),
    suppression_thres = 2,
    suppress_0 = FALSE,
    suppression_total_suppression = TRUE,
    secondary_suppress_0 = FALSE
  ),
  dplyr::tibble(
    a = c(9999999, 9999999, 15, 100, 100, 165, 9999999,0),
    b = c(9999999, 9999999, 8, 20, 100, 200, 0,0),
    c = c(9999999, 9999999, 10,45,100,100,9999999,0)
  )
)

testthat::expect_equal(
  suppress_single_group(
    df = test,
    c("col", "row"),
    suppression_thres = 2,
    suppress_0 = FALSE,
    suppression_total_suppression = TRUE,
    secondary_suppress_0 = FALSE,
    priority_row_suppression = 3:8
  ),
  dplyr::tibble(
    a = c(9999999, 10, 9999999, 100, 100, 165, 9999999,0),
    b = c(9999999, 4, 9999999, 20, 100, 200, 0,0),
    c = c(9999999, 5, 9999999,45,100,100,9999999,0)
  )
)


### Examples for our indirect suppression code...
example <- dplyr::tibble(
  a = rep(10, 5),
  b = c(5,11,15,7,13),
  c = c(5,8,5,8,5)
)

main_solution <- 
  dplyr::tibble(
    a = rep(10, 5),
    b = c(9999999,11,9999999,7,9999999),
    c = c(9999999,8,9999999,8,9999999)
  )

secondary_solution <- 
  dplyr::tibble(
    a = c(10, 10, 9999999, 10, 9999999),
    b = c(9999999,11,15,9999999,13),
    c = c(9999999,8,9999999,9999999,9999999)
  )

# these should all produce our simple output...
testthat::expect_equal(
  suppress_single_group(
    df = example,
    c("col", "row"),
    suppression_thres = 5,
    total_suppression = FALSE
  ),
  main_solution
)

testthat::expect_equal(
  suppress_single_group(
    df = example,
    c("col", "row"),
    suppression_thres = 6,
    total_suppression = FALSE,
    indirect_suppression = FALSE
  ),
  main_solution
)

testthat::expect_equal(
  suppress_single_group(
    df = example,
    c("col", "row"),
    suppression_thres = 6, # 6 here, so our indirect supp shouldn't trigger...
    total_suppression = FALSE,
    indirect_suppression = TRUE
  ),
  main_solution
)

# these should trigger our indirect suppression and be sent down a slightly different path
testthat::expect_equal(
  suppress_single_group(
    df = example,
    c("col", "row"),
    suppression_thres = 5,
    total_suppression = FALSE,
    indirect_suppression = TRUE
  ),
  secondary_solution
)