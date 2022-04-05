# mojSuppression

> Apply row, column and row/column suppression to dataframes. 

## Overview

This package contains three main functions: `simple_suppression`, `suppress_single_group` (both for suppressing a single dataframe) and `suppress_tidy_data` (for suppressing on multiple dfs, bound together).

Installation:
```r
if(!"devtools" %in% installed.packages()[, "Package"]) install.packages("devtools")  # run if you don't have devtools installed
devtools::install_github('moj-analytical-services/mojSuppression')
```

If there are features you'd like implemented or if any bugs are found, please submit an `Issue`.

## Disclaimer on methodology

The main idea underpinning the row+col evaluation within this code is the idea of branching. 
In short, where it's unclear what the next suppression step should be (i.e. we have multiple possible avenues we could go down), we will create and queue new branches to be evaluated later.

The longer version is:
As we run the code, new items will be added to the list as 'suppression branches'.
These branches are simply instances where multiple decisions on what to suppress can be made (i.e. we have three 3s we can in theory suppress)
but it's not initially obvious which to suppress. If you suppress manually, you'll come across plenty of examples where you have to take a minute
to think about what the correct suppression step would be. This code "thinks" by branching and evaluating multiple possible solutions,
allowing us to check all of these solutions and find the winner.

The obvious flaw here is that this all comes at the cost of performance. In general, loops will be quick enough that you don't notice this. However,
if you have a dataframe of 500+ cells, you'll quickly start creating branches into oblivion. Smaller dataframes (30ish rows and below) will easily run in < 1 second.

The code allows you to limit the total number of branches evaluated to mitigate this looping hell. The output will always be a valid solution, but may not be the optimal
solution.

## Navigation

Suppression functions available:
  - [simple_suppression](#simple_suppression)
  - [suppress_single_group](#banner)
  - [suppress_tidy_data](#suppress_tidy_data)

<hr>

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

### suppress_tidy_data


This function simply extends the single group suppression functions, allowing the user to suppress across multiple groups simultaneously.

So, say you had a dataframe that looked like this:

![Screenshot 2022-04-05 at 17 35 30](https://user-images.githubusercontent.com/45356472/161803015-51c7c20a-5b20-49ec-a051-35a0f76251ae.png)

Here, `LONDON` and `WALES` should be suppressed **independently** of one another. 
That is, secondary suppression should be applied to both `LONDON` and `WALES`, as opposed to the dataframe as a whole.


The `suppress_tidy_data` function resolves this issue by breaking the dataframe up into individual parts and then performing suppression across each section.

In code, this looks like:
```
# Create df
location <- c("LONDON", "LONDON", "LONDON", "LONDON", "LONDON", "WALES", "WALES", "WALES", "WALES")
col1 <- c(1, 75, 1, 5, 1, 6, 55, 1, 12)

new_table <- dplyr::tibble(location = location,
              headcount = col1)
              
# apply tidy suppression
mojSuppression::suppress_tidy_data(
  df=new_table,
  groups_to_suppress_across = c("location"), # this can be more than one column
  where_to_suppress="col",
  suppression_thres = 2
)
```

With the output being:
![Screenshot 2022-04-05 at 17 42 16](https://user-images.githubusercontent.com/45356472/161804202-f5af00dd-0a88-4335-8f11-82034b0d02b2.png)
