
<!-- README.md is generated from README.Rmd. Please edit that file -->

# quickstats

<!-- badges: start -->
<!-- badges: end -->

The goal of quickstats is to simply and statistically compare columns by
group and output the results in a publication-ready table.

## Installation

You can install the development version of quickstats from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jtmenchaca/quickstats")
```

## Create a comparison

``` r
library(quickstats)
library(dplyr, quietly = T, warn.conflicts = F)
library(palmerpenguins, quietly = T, warn.conflicts = F)

data(penguins, package = "palmerpenguins")
penguins_data = penguins |>
  filter(species != "Gentoo") # Simplifying example for only 2 groups

summary = penguins_data |> 
  compare_cols_by_group(
    group_col = "species", 
    binary_or_cat_cols = c("island", "sex", "year"), 
    continuous_cols = c("bill_length_mm", "bill_depth_mm", 
                        "flipper_length_mm", "body_mass_g")
  )
```

By default, the binary/categorical columns are compared across groups
using the Chi square test or the Fisher’s Exact test, depending on the
count of results. If there are no observations for a given subgroup, no
p-value is provided (consider comparing across sub-groups that all have
at least one observation in a separate analysis).

``` r
penguins_data |> 
  compare_cols_by_group(
    group_col = "species", 
    binary_or_cat_cols = c("island", "sex", "year")
  )
#> # A tibble: 13 × 5
#>    Characteristic   `Species - Adelie` `Species - Chinstrap` `p-value` statist…¹
#>    <chr>            <chr>              <chr>                 <chr>     <chr>    
#>  1 "Total Count, n" "152"              "68"                  ""        ""       
#>  2 "Island, n (%)"  ""                 ""                    ""        ""       
#>  3 "     Biscoe"    "44 (28.9)"        "0 (0)"               ""        "n/a"    
#>  4 "     Dream"     "56 (36.8)"        "68 (100)"            "<.01"    "Chi-squ…
#>  5 "     Torgersen" "52 (34.2)"        "0 (0)"               ""        "n/a"    
#>  6 "Sex, n (%)"     ""                 ""                    ""        ""       
#>  7 "     female"    "73 (48)"          "34 (50)"             ".9"      "Chi-squ…
#>  8 "     male"      "73 (48)"          "34 (50)"             ".9"      "Chi-squ…
#>  9 "     Missing"   "6 (3.9)"          "0 (0)"               ""        "n/a"    
#> 10 "Year, n (%)"    ""                 ""                    ""        ""       
#> 11 "     2007"      "50 (32.9)"        "26 (38.2)"           ".54"     "Chi-squ…
#> 12 "     2008"      "50 (32.9)"        "18 (26.5)"           ".43"     "Chi-squ…
#> 13 "     2009"      "52 (34.2)"        "24 (35.3)"           "1"       "Chi-squ…
#> # … with abbreviated variable name ¹​statistical_test
```

Continuous columns are compared between two groups by either 1) the
Student’s t-test if the column has a normal distribution or 2) the
Wilcoxon signed-rank test if the distribution is non-normal. The
Shapiro–Wilk test is used to evaluate normality.

When comparing between three groups, the ANOVA test is used to compare
continuous columns.

If a continuous variable has a non-normal distribution by the
Shaprio-Wilk test, it is summarized using IQR.

``` r
penguins_data |> 
  compare_cols_by_group(
    group_col = "species", 
    continuous_cols = c("bill_length_mm", "bill_depth_mm", 
                        "flipper_length_mm", "body_mass_g")
  )
#> # A tibble: 9 × 5
#>   Characteristic                 `Species - Adelie` Species - …¹ p-val…² stati…³
#>   <chr>                          <chr>              <chr>        <chr>   <chr>  
#> 1 "Total Count, n"               152                68           ""      ""     
#> 2 "Bill Length Mm, median [IQR]" 38.8 [36.8, 40.8]  49.5 [46.3,… "<.01"  "Wilco…
#> 3 "     Missing, n (%)"          1 (0.7)            0 (0)        ""      "n/a"  
#> 4 "Bill Depth Mm, mean (SD)"     18.3 (1.2)         18.4 (1.1)   ".66"   "Stude…
#> 5 "     Missing, n (%)"          1 (0.7)            0 (0)        ""      "n/a"  
#> 6 "Flipper Length Mm, mean (SD)" 190.0 (6.5)        195.8 (7.1)  "<.01"  "Stude…
#> 7 "     Missing, n (%)"          1 (0.7)            0 (0)        ""      "n/a"  
#> 8 "Body Mass G, mean (SD)"       3,700.7 (458.6)    3,733.1 (38… ".59"   "Stude…
#> 9 "     Missing, n (%)"          1 (0.7)            0 (0)        ""      "n/a"  
#> # … with abbreviated variable names ¹​`Species - Chinstrap`, ²​`p-value`,
#> #   ³​statistical_test
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

## Save your results to Excel

Neatly translate your formatted results to an XLSX file with the helper
function `save_comparison_to_xlsx`.

    #> # A tibble: 21 × 5
    #>    Characteristic   `Species - Adelie` `Species - Chinstrap` `p-value` statist…¹
    #>    <chr>            <chr>              <chr>                 <chr>     <chr>    
    #>  1 "Total Count, n" "152"              "68"                  ""        ""       
    #>  2 "Island, n (%)"  ""                 ""                    ""        ""       
    #>  3 "     Biscoe"    "44 (28.9)"        "0 (0)"               ""        "n/a"    
    #>  4 "     Dream"     "56 (36.8)"        "68 (100)"            "<.01"    "Chi-squ…
    #>  5 "     Torgersen" "52 (34.2)"        "0 (0)"               ""        "n/a"    
    #>  6 "Sex, n (%)"     ""                 ""                    ""        ""       
    #>  7 "     female"    "73 (48)"          "34 (50)"             ".9"      "Chi-squ…
    #>  8 "     male"      "73 (48)"          "34 (50)"             ".9"      "Chi-squ…
    #>  9 "     Missing"   "6 (3.9)"          "0 (0)"               ""        "n/a"    
    #> 10 "Year, n (%)"    ""                 ""                    ""        ""       
    #> # … with 11 more rows, and abbreviated variable name ¹​statistical_test

It should leave you with a tidy XLSX file with something that looks like
the following:

![A tidy XLSX table](man/figures/README-example-xlsx.png)

*J.T. Menchaca*
