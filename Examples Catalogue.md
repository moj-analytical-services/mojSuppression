# Examples Catalogue

> Please run any code examples from here locally

The arguments section uses the following dataframe, so please load this into memory before continuing:
**Load our df:**
```
# Create our test df
bme <- c(25, 1, 1, 10)
white <- c(5, 6, 5, 1)
other <- c(1, 7, 4, 3)
test <- c(5, 6, 4, 3)

trial_df <- data.frame(bme = bme,
                 white = white,
                 other = other,
                 test = test)
```

## Contents

<hr>

## Code Behaviour

The code has some in-built behaviours to try to ensure that suppressed outputs are as good as they can be. 
If you get an output that deviates from any of these, please contact the maintainer of this repo.

### Minimise Suppression Nodes and Suppression Total
To ensure that we don't oversuppress, the code will evaluate all potential suppression options and choose the output that minimises the
total number of suppression nodes (places where suppression has taken place) on display, and secondarily the result that minimises your suppression total.

**To understand node minimisation, please see the following example:**
```
# EXAMPLE BEHAVIOUR
# Create our test df
bme <- c(25, 1, 1, 1)
white <- c(10, 6, 10, 1)
other <- c(1, 5, 10, 10)
test <- c(3,3,3,3)

df <- data.frame(bme = bme,
                 white = white,
                 other = other,
                 test = test)

# suppress our df without any bells and whistles
mojSuppression::apply_suppression_to_df(
  df = df,
  where_to_suppress = c('col', 'row'),
  suppression_thres = 2
)
```
The original df looks like:

![Screenshot 2021-09-13 at 19 08 56](https://user-images.githubusercontent.com/45356472/133134723-3accf43b-6c96-4783-aa00-0d085cfbc224.png)


and our output after suppression is:

![Screenshot 2021-09-13 at 19 03 25](https://user-images.githubusercontent.com/45356472/133134063-21a23c4c-02fb-4d8f-ba44-fcb17bc0d3ec.png)
This results in 8 total suppression nodes and a suppression total of 31.


Alternatively, you can imagine we could have suppressed our dataframe like so:

![Screenshot 2021-09-13 at 19 07 16](https://user-images.githubusercontent.com/45356472/133134522-1eae0592-22b2-4fe7-aa6a-e96dde8edcb3.png)
This results in 9 suppression nodes and a suppression total of 27.

Here, the code chooses an output that minimises our suppression nodes first and the suppression total second.

Please do verify these results for yourself in excel.

### Randomly Selecting between Indistinguishable Suppression Nodes


<hr>


### where_to_suppress
```
# suppress across both rows and columns
mojSuppression::apply_suppression_to_df(
  df = trial_df,
  where_to_suppress = c('col', 'row'),
  suppression_thres = 2
)
# suppress across only columns
mojSuppression::apply_suppression_to_df(
  df = trial_df,
  where_to_suppress = c('col'),
  suppression_thres = 2
)
```

### suppression_thres
```
# tweak to adjust which values should be primary suppressed
mojSuppression::apply_suppression_to_df(
  df = trial_df,
  where_to_suppress = c('col', 'row'),
  suppression_thres = 5
)
```

### cols_to_suppress
> Only suppress specific columns - please feed in column names
```
# as you can see from below, the 1 in column other has been ignored
mojSuppression::apply_suppression_to_df(
  df = trial_df,
  where_to_suppress = c('col', 'row'),
  suppression_thres = 2,
  cols_to_suppress = c('bme', 'white', 'test')
)
```

### row_nos_to_suppress
> Suppress specific rows - please feed in row indexes
```
# as you can see from below, the 1 in column other has been ignored
mojSuppression::apply_suppression_to_df(
  df = trial_df,
  where_to_suppress = c('col', 'row'),
  suppression_thres = 2,
  row_nos_to_suppress = 2:4
)
```

### suppress_0
> Whether to suppress 0s in your dataframe - default set to FALSE

```
# Create our test trial_df
bme <- c(25, 1, 0, 10)
white <- c(10, 0, 10, 1)
other <- c(1, 5, 10, 10)
test <- c(5, 6, 4, 3)

df <- data.frame(bme = bme,
                 white = white,
                 other = other,
                 test = test)
# 0s are now suppressed
mojSuppression::apply_suppression_to_df(
  df = trial_df,
  where_to_suppress = c('col'),
  suppression_thres = 2,
  suppress_0 = TRUE
)
```

