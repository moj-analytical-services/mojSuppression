# moj-suppression

> Apply row, column and row/column suppression to dataframes. 

## Overview

This package contains three main functions: `simple_suppression`, `suppress_single_group` (both for suppressing a single dataframe) and `suppress_tidy_data` (for suppressing on multiple dfs, bound together).

Installation (as of Sept 2021):
```r
remotes::install_github('moj-analytical-services/moj-suppression')
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
