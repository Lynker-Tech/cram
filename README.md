
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cram

<!-- badges: start -->

[![Dependencies](https://img.shields.io/badge/dependencies-6/30-orange?style=flat)](#)
<!-- badges: end -->

The goal of cram is to process output from the Lynker CRAM model.

## Installation

You can install the development version of cram from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("Lynker-Tech/cram")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(cram)

base_folder  <-  "D:/downloads/runs/2049 v0.469 0.13/"
# base_folder       <-  "C:/Users/angus/OneDrive/Desktop/runs/2049 v0.469 0.13"

## basic example code
models       <-  cram::parse_directory(base_folder)
models 
#>   base_year model_version                         base_folder
#> 1      2049   v0.469 0.13 D:/downloads/runs/2049 v0.469 0.13/
```

Now that we’ve located the model directory, we can use the
`process_cram()` function to process the output sheets within the model
directory

``` r
cram1 <- cram::process_cram(
            model_directory = models[1, ],
            return_wide     = TRUE
          )
#> Warning: One or more parsing issues, see `problems()` for details
#> One or more parsing issues, see `problems()` for details

head(cram1, 15)
#> # A tibble: 15 × 763
#>    model_year model_ve…¹ model…² year  qm    step  start_date end_date   inflo…³
#>    <chr>      <chr>      <chr>   <chr> <chr> <chr> <date>     <date>       <dbl>
#>  1 2049       v0.469 0.… 000.AR… 1948  1     0     1947-10-01 1947-10-08       0
#>  2 2049       v0.469 0.… 000.AR… 1948  2     0     1947-10-09 1947-10-16      44
#>  3 2049       v0.469 0.… 000.AR… 1948  3     0     1947-10-17 1947-10-24     134
#>  4 2049       v0.469 0.… 000.AR… 1948  4     0     1947-10-25 1947-10-31      27
#>  5 2049       v0.469 0.… 000.AR… 1948  5     0     1947-11-01 1947-11-08      15
#>  6 2049       v0.469 0.… 000.AR… 1948  6     0     1947-11-09 1947-11-16       0
#>  7 2049       v0.469 0.… 000.AR… 1948  7     0     1947-11-17 1947-11-23       0
#>  8 2049       v0.469 0.… 000.AR… 1948  8     0     1947-11-24 1947-11-30       0
#>  9 2049       v0.469 0.… 000.AR… 1948  9     0     1947-12-01 1947-12-08       0
#> 10 2049       v0.469 0.… 000.AR… 1948  10    0     1947-12-09 1947-12-16       0
#> 11 2049       v0.469 0.… 000.AR… 1948  11    0     1947-12-17 1947-12-24       0
#> 12 2049       v0.469 0.… 000.AR… 1948  12    0     1947-12-25 1947-12-31       0
#> 13 2049       v0.469 0.… 000.AR… 1948  13    0     1948-01-01 1948-01-08       0
#> 14 2049       v0.469 0.… 000.AR… 1948  14    0     1948-01-09 1948-01-16       0
#> 15 2049       v0.469 0.… 000.AR… 1948  15    0     1948-01-17 1948-01-24       0
#> # … with 754 more variables: inflow_10_flow <dbl>, link_575_flow <dbl>,
#> #   link_572_flow <dbl>, decree_22_flow <dbl>, dataobject_120_flow <dbl>,
#> #   dataobject_35_flow <dbl>, dataobject_36_flow <dbl>,
#> #   dataobject_114_flow <dbl>, dataobject_28_flow <dbl>,
#> #   dataobject_29_flow <dbl>, link_710_flow <dbl>, link_712_flow <dbl>,
#> #   link_711_flow <dbl>, link_713_flow <dbl>, demand_17_flow <dbl>,
#> #   demand_2_flow <dbl>, demand_52_flow <dbl>, link_971_flow <dbl>, …
#> # ℹ Use `colnames()` to see all variable names
```

<br>

We can also build a lookup table for the output sheets in the model
directory using the `lookup_table()` function

``` r
lookup_df <- cram::lookup_table(
            model_directory = models[1, ]
          )
#> Warning: One or more parsing issues, see `problems()` for details
#> One or more parsing issues, see `problems()` for details

head(lookup_df, 15)
#>          output_sheet                 model_scenario           orig_name
#> X1...1       ARKANSAS 000.ARWM 2022-08-03 2045 0.469                Step
#> X2...2       ARKANSAS 000.ARWM 2022-08-03 2045 0.469                Step
#> X3...3       ARKANSAS 000.ARWM 2022-08-03 2045 0.469                Step
#> X4...4       ARKANSAS 000.ARWM 2022-08-03 2045 0.469       Inflow_9_Flow
#> X5...5       ARKANSAS 000.ARWM 2022-08-03 2045 0.469      Inflow_10_Flow
#> X6...6       ARKANSAS 000.ARWM 2022-08-03 2045 0.469       Link_575_Flow
#> X7...7       ARKANSAS 000.ARWM 2022-08-03 2045 0.469       Link_572_Flow
#> X8...8       ARKANSAS 000.ARWM 2022-08-03 2045 0.469      Decree_22_Flow
#> X9...9       ARKANSAS 000.ARWM 2022-08-03 2045 0.469 DataObject_120_Flow
#> X10...10     ARKANSAS 000.ARWM 2022-08-03 2045 0.469  DataObject_35_Flow
#> X11...11     ARKANSAS 000.ARWM 2022-08-03 2045 0.469  DataObject_36_Flow
#> X12...12     ARKANSAS 000.ARWM 2022-08-03 2045 0.469 DataObject_114_Flow
#> X13...13     ARKANSAS 000.ARWM 2022-08-03 2045 0.469  DataObject_28_Flow
#> X14...14     ARKANSAS 000.ARWM 2022-08-03 2045 0.469  DataObject_29_Flow
#> X15...15     ARKANSAS 000.ARWM 2022-08-03 2045 0.469       Link_710_Flow
#>                         name                                     desc parameter
#> X1...1                  step                                    Major      Time
#> X2...2                  step                                    Minor      Time
#> X3...3                  step                                     <NA> Operation
#> X4...4         inflow_9_flow                          Aurora HS yield      Flow
#> X5...5        inflow_10_flow          Aurora's 1/2 Busk-Ivanhoe Yield      Flow
#> X6...6         link_575_flow           Aurora's Twin Lakes allocation      Flow
#> X7...7         link_572_flow  Aurora's Upper Ranch Diversions to Twin      Flow
#> X8...8        decree_22_flow            2500 AF B-I lease from Pueblo      Flow
#> X9...9   dataobject_120_flow       Aurora's yield from Highline Canal      Flow
#> X10...10  dataobject_35_flow              Aurora's RIG Headgate Yield      Flow
#> X11...11  dataobject_36_flow    Aurora's Rocky Ford II headgate yield      Flow
#> X12...12 dataobject_114_flow      Aurora Colo Canal yield (at Lake M)      Flow
#> X13...13  dataobject_28_flow     Aurora's upper basin exchange decree      Flow
#> X14...14  dataobject_29_flow Aurora's Colo Canal upper basin exchange      Flow
#> X15...15       link_710_flow      PBWW contract exchange to Turquoise      Flow
```

<br>

We can identify the variables in one of processed CRAM output sheets

``` r
output_variables <- find_variables(
                        df   = cram1
                      )

head(output_variables, 15)
#> # A tibble: 15 × 2
#>    names               title_names        
#>    <chr>               <chr>              
#>  1 model_year          Model Year         
#>  2 model_version       Model Version      
#>  3 model_scenario      Model Scenario     
#>  4 year                Year               
#>  5 qm                  Qm                 
#>  6 step                Step               
#>  7 start_date          Start Date         
#>  8 end_date            End Date           
#>  9 inflow_9_flow       Inflow 9 Flow      
#> 10 inflow_10_flow      Inflow 10 Flow     
#> 11 link_575_flow       Link 575 Flow      
#> 12 link_572_flow       Link 572 Flow      
#> 13 decree_22_flow      Decree 22 Flow     
#> 14 dataobject_120_flow Dataobject 120 Flow
#> 15 dataobject_35_flow  Dataobject 35 Flow
```

Identify available variables using `find_variables()`, and use
`plot_cram()` to plot a set of variables for multiple model scenarios
after processing CRAM output sheets using `process_cram()`

``` r
# Plot specific variables for CRAM model runs
cram_plot <- plot_cram(
                  cram_df   = cram1,
                  plot_vars = output_variables$names[17:19],
                  start     = "2004-01-01",
                  end       = "2007-01-01",
                  wrap      = FALSE
                )
#> Plotting...

cram_plot
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />
