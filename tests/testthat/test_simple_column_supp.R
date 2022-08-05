#### ======= SIMPLE COLUMN SUPPRESSION ======= ####

testthat::test_that("Our suppression function for basic dataframes is working as expected (suppress_single_group)", {
  
  #create the first dummy df
  #setup practice dfs
  set.seed(10)
  dummy_df <- dplyr::tibble(locations = c(rep("Mars", 5), rep("Earth", 5), rep("10SC", 5)),
                     grades = rep(c("Starting", "Novice", "Apprentice", "Senior", "Manager"), 3),
                     resignations = sample(0:20, size = 15, replace = T),
                     deaths = sample(0:10, size = 15, replace = T)
  )
  
  #and a second with a slightly different structure
  set.seed(10)
  dummy_df2 <- dplyr::tibble(race = c("asian", "black", "other", "mixed", "white"),
                      grade_1 = sample(0:5, size = 5, replace = T),
                      grade_2 = sample(0:5, size = 5, replace = T),
                      grade_3 = sample(0:5, size = 5, replace = T),
                      grade_4 = sample(0:5, size = 5, replace = T)) %>% 
    janitor::adorn_totals(c("col", "row"))
  
  
  #### ====== COLUMN AND ROW SUPPRESSION CHECKS 
  #check that column suppression is working correctly (both primary and secondary...) 
  # PRIMARY SUPPRESSION ONLY
  testthat::expect_equal(
    sum(suppress_single_group(dummy_df2,
                              cols_to_suppress = dummy_df2 %>% select_if(is.numeric) %>% colnames(),
                              suppression_thres = 2,
                              suppress_0 = F,
                              inc_secondary_suppression = FALSE) %>% 
          pull(grade_3) == 9999999), 
    sum(dummy_df2$grade_3 <= 2)
  )
  
  testthat::expect_true(
    length(which(suppress_single_group(dummy_df2,
                                       where_to_suppress = "row",
                                       row_nos_to_suppress = 1:length(dummy_df2),
                                       cols_to_suppress = dummy_df2 %>% select_if(is.numeric) %>% colnames(),
                                       suppression_thres = 1,
                                       suppress_0 = TRUE,
                                       inc_secondary_suppression = F)[1,2:6] %>% unlist() == 9999999)) == 1
  )
  
  # SECONDARY SUPPRESSION
  # (Test the same code as in the primary suppression, but this time checking for at least 2 values of 9999999)
  expect_equal(sum(suppress_single_group(dummy_df2,
                                           cols_to_suppress = dummy_df2 %>% select_if(is.numeric) %>% colnames(),
                                           suppression_thres = 2,
                                           suppress_0 = F,
                                           inc_secondary_suppression = TRUE) %>% 
                       pull(grade_3) == 9999999) >= 2, TRUE)
  
  testthat::expect_equal(sum(suppress_single_group(dummy_df,
                                                   cols_to_suppress = dummy_df %>% select_if(is.numeric) %>% colnames(),
                                                   suppression_thres = 1,
                                                   suppress_0 = T) %>% 
                               pull(resignations) == 9999999) >= 2, TRUE)
  
  testthat::expect_equal(sum(suppress_single_group(dummy_df2,
                                           cols_to_suppress = dummy_df2 %>% select_if(is.numeric) %>% colnames(),
                                           suppression_thres = 1,
                                           suppress_0 = T) %>% 
                       pull(grade_2) == 9999999) >= 2, TRUE)
  
  
  testthat::expect_true(
    length(which(
      suppress_single_group(dummy_df2,
                            where_to_suppress = "row",
                            row_nos_to_suppress = 1:length(dummy_df2),
                            cols_to_suppress = dummy_df2 %>% select_if(is.numeric) %>% colnames(),
                            suppression_thres = 1,
                            suppress_0 = TRUE)[1,2:6] %>% unlist() == 9999999)) >= 2
  )
  
  
  testing_example3 <- data.frame(
    a = c(4, 4, 4),
    b = c(2, 3, 1),
    c = c(3, 1, 2)
  )
  
  solution <- data.frame(
    a = c(4, 4, 4),
    b = c("-", "-", "-"),
    c = c("-", "-", "-")
  )
  
  testthat::expect_equal(
    suppress_single_group(
      df = testing_example3,
      c("col", "row"),
      suppression_thres = 2,
      suppress_0=FALSE,
      suppression_output_value = "-",
      secondary_suppress_0=FALSE
    ),
    solution
  )
  
})
