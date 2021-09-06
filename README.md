# mojSuppression

> Apply row, column and row/column suppression to dataframes. 

## Overview

This package contains three main functions: `simple_suppression`, `suppress_single_group` (both for suppressing a single dataframe) and `suppress_tidy_data` (for suppressing on multiple dfs, bound together).

Installation:
```r
# install.packages('devtools') # run if you don't have devtools installed
devtools::install_github('moj-analytical-services/mojSuppression')
```

If there are features you'd like implemented or if any bugs are found, please submit an `Issue`.

Suppression functions available:
  - [simple_suppression](#simple_suppression)
  - [suppress_single_group](#banner)
  - [suppress_tidy_data](#radio-button)

### simple_suppression
Simple suppression function without any additional bells and whistles. 

This function accepts an input dataframe, alongside some basic arguments and outputs a suppressed dataframe. See `?mojSuppression::simple_suppression` for more details.

**Example:**
```
bme <- c(25, 1, 2, 10)
white <- c(10, 6, 10, 2)
other <- c(1, 5, 10, 10)
test <- c(5, 6, 1, 2)

test_df <- data.frame(bme = bme,
                      white = white,
                      other = other,
                      test = test)

# suppress only columns
mojSuppression::simple_suppression(
  df = test_df,
  where_to_suppress = c('col', 'row'),
  cols_to_suppress = c("bme", "white", "other", "test"), # you can leave this blank if you wish to suppress all cols
  row_nos_to_suppress = 1:nrow(test_df), # you can leave this blank if you wish to supp all rows
  suppression_thres = 2,
  suppression_output_value = "~"
)
# suppress both rows and columns
mojSuppression::simple_suppression(
  df = test_df,
  where_to_suppress = c('col', 'row'),
  cols_to_suppress = c("bme", "white", "other", "test"), # you can leave this blank if you wish to suppress all cols
  row_nos_to_suppress = 1:nrow(test_df), # you can leave this blank if you wish to supp all rows
  suppression_thres = 2,
  suppression_output_value = "~"
)
```
