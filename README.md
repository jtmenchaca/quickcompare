
<!-- README.md is generated from README.Rmd. Please edit that file -->

# quickstats

<!-- badges: start -->
<!-- badges: end -->

The goal of quickstats is to provide simple functionality to
statistically compare two groups and output the results in a
publication-ready table.

## Installation

You can install the development version of quickstats from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jtmenchaca/quickstats")
```

## Examples

``` r
library(quickstats)
library(dplyr, quietly = T)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
## basic example code
data(iris)
summary = iris |>
  summarize_cols_by_group(
    group_col = "Species",
    continuous_cols = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
    rounding_digits = 1
  )


data(mtcars)
summary = mtcars |>
  summarize_cols_by_group(
    group_col = "cyl",
    binary_or_cat_cols = c("vs", "am", "gear", "carb"),
    continuous_cols = c("mpg", "disp", "hp", "drat", "wt", "qsec")
  )
```

By default, the binary/categorical columns are compared across groups
using the Fisher’s Exact Test. If the data is too large to be run
efficiently using the standard Fisher’s Exact Test, comparisons will be
made using the Monte Carlo test for Fisher’s.

``` r
mtcars |>
  summarize_cols_by_group(
    group_col = "cyl",
    binary_or_cat_cols = c("gear", "carb")
  )
#> # A tibble: 12 × 6
#>    Characteristic   `Cyl - 4`  `Cyl - 6`  `Cyl - 8`   `p-value` statistical_test
#>    <chr>            <chr>      <chr>      <chr>       <chr>     <chr>           
#>  1 "Total Count, n" "11"       "7"        "14"        ""        ""              
#>  2 "Gear, n (%)"    ""         ""         ""          ""        ""              
#>  3 "     3"         "1 (9.1)"  "2 (28.6)" "12 (85.7)" "<.01"    "Fisher's Exact"
#>  4 "     4"         "8 (72.7)" "4 (57.1)" "0 (0)"     "<.01"    "Fisher's Exact"
#>  5 "     5"         "2 (18.2)" "1 (14.3)" "2 (14.3)"  "1"       "Fisher's Exact"
#>  6 "Carb, n (%)"    ""         ""         ""          ""        ""              
#>  7 "     1"         "5 (45.5)" "2 (28.6)" "0 (0)"     ".01"     "Fisher's Exact"
#>  8 "     2"         "6 (54.5)" "0 (0)"    "4 (28.6)"  ".05"     "Fisher's Exact"
#>  9 "     3"         "0 (0)"    "0 (0)"    "3 (21.4)"  ".22"     "Fisher's Exact"
#> 10 "     4"         "0 (0)"    "4 (57.1)" "6 (42.9)"  ".01"     "Fisher's Exact"
#> 11 "     6"         "0 (0)"    "1 (14.3)" "0 (0)"     ".23"     "Fisher's Exact"
#> 12 "     8"         "0 (0)"    "0 (0)"    "1 (7.1)"   "1"       "Fisher's Exact"
```

Continuous columns are compared between two groups by the Student’s
t-Test if the column has a normal distribution or the Wilcoxon
signed-rank test if the distribution is non-normal. The Shapiro–Wilk
test is used to evaluate normality.

When compared between three groups, the ANOVA test is used to compare
continuous columns.

``` r
mtcars |> 
  filter(cyl != 6) |> 
  summarize_cols_by_group(
    group_col = "cyl", 
    continuous_cols = c("mpg", "disp", "hp")
  )
#> # A tibble: 4 × 5
#>   Characteristic     `Cyl - 4`           `Cyl - 8`            `p-value` statis…¹
#>   <chr>              <chr>               <chr>                <chr>     <chr>   
#> 1 Total Count, n     11                  14                   ""        ""      
#> 2 Mpg, mean (SD)     26.7 (4.5)          15.1 (2.6)           "<.01"    "Studen…
#> 3 Disp, median [IQR] 108.0 [78.8, 120.7] 350.5 [301.8, 390.0] "<.01"    "Wilcox…
#> 4 Hp, mean (SD)      82.6 (20.9)         209.2 (51.0)         "<.01"    "Studen…
#> # … with abbreviated variable name ¹​statistical_test
```

Use the `remove_group_col_NA` and `cols_to_remove_NA` to remove rows
with missing values in specific columns.

Sometimes, you want to use a subset of your dataset to compare across
groups for your binary/categorical data. To specify the column you would
like to use as a subset, use the `binary_or_cat_cols_subpop` argument.

If the `binary_or_cat_cols_subpop` argument is provided, it must be the
same length as the `binary_or_cat_cols` argument where each value in
`binary_or_cat_cols` aligns by index to the value in the
`binary_or_cat_cols_subgroup`. Columns that should use the general
population should have a value of the empty string ““.

For now, any columns specified in `binary_or_cat_cols_subpop` should be
binary columns, where a value of `1` will be used to identify the
appropriate subpopulation.

Note: I know the `binary_or_cat_cols_subpop` is not intuitive at the
moment. It will be changed to be more user-friendly in the future!

I hope this can be helpful!

Best, J.T. Menchaca
