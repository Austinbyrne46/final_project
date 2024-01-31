# Purpose

The purpose of this readme is to provide a breakdown of my thought
process while completing this volatility modeling project. In this
readme I will explain what and why I am doing what I am doing to provide
a clear structure for this project.

``` r
rm(list = ls()) # Clean your environment:
gc() # garbage collection - It can be useful to call gc after a large object has been removed, as this may prompt R to return memory to the operating system.
```

    ##          used (Mb) gc trigger (Mb) max used (Mb)
    ## Ncells 467544 25.0    1005647 53.8   660385 35.3
    ## Vcells 865053  6.6    8388608 64.0  1769691 13.6

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
list.files('code/', full.names = T, recursive = T) %>% .[grepl('.R', .)] %>% as.list() %>% walk(~source(.))
```

# Loading relevant packages

Here I am loading the relevant packages that I will be using throughout
this analysis.

``` r
#Loading all relevant packages. 
#install.packages("pacman")
pacman::p_load(tidyverse)
pacman::p_load(readxl)
pacman::p_load(readr)
pacman::p_load(ggplot2)
pacman::p_load(gt)
pacman::p_load(dplyr)
pacman::p_load("MTS", "robustbase")
pacman::p_load("devtools", "rugarch", "rmgarch", 
    "forecast", "tbl2xts", "lubridate", "PerformanceAnalytics", 
    "ggthemes")
library(MTS)
library(robustbase)
library(gt)
library(tidyverse)
library(dplyr)
library(lubridate)
library(zoo)
library(tidyr)
library(tseries)
library(PortfolioAnalytics)
library(ggplot2)
library(RcppRoll)
library(tbl2xts)
library(fmxdat)
library(rmsfuns)
library(PerformanceAnalytics)
```

# Loading relevant data

The following data is loaded into R. I have loaded LCL_Indicies and
LCL_Stock_Returns data sets, however, I am unsure of which data set I
will use for this volatility analysis. This will be decided when doing
some further analysis below.

The LCL_Stock_Returns data set is giving me issues when publishing to
GitHub that is why i have commented it out here. This makes no
difference to my project as I dont end up using that data set in any
analysis.

``` r
LCL_Indices <- readRDS("C:/Users/austi/OneDrive/Desktop/Masters/Financial Econometrics/Project/DCC_GARCH_volatility_model/data/LCL_Indices (1).rds")
#LCL_Stock_Returns <- readRDS("C:/Users/austi/OneDrive/Desktop/Masters/Financial Econometrics/Project/DCC_GARCH_volatility_model/data/LCL_Stock_Returns (1).rds")
```

# Data

## Data overview

The two data sets that I am using in the analysis is that of the
LCL_Indices data set, which contains information on the returns for
differing indexes such as the SWIX, ALSI, Capped SWIX, Top 40, SWIX Top
40, Capped SWIX Top 40, CAPI, Capped Top 40, Industrials, Financials,
Resources, albi and jsapy. The second data set that I am evaluating is
the LCL_Stock_Returns data set. Which also provides the returns for
differing stock in differing indexes.

## Checking for missing values

Before continuing with this analysis it is imperative that I evaluate
the data at hand. Firstly I want to look at whether there are any
missing values in the two data sets that I have imported into R. If so I
need to find an appropriate way of dealing with these missing values.

``` r
#Checking how many total observations we have
nrow(LCL_Indices)
```

    ## [1] 4281

``` r
# For LCL_Indices data set
sum(is.na(LCL_Indices))
```

    ## [1] 602

``` r
# For LCL_Stock_Returns data set
#sum(is.na(LCL_Stock_Returns))
```

From the above code, it is evident that the LCL_Indices data set has 602
missing values and the LCL_Stock_Returns data set has 2 400 432 missing
values. Due to the large amount of missing values in the
LCL_Stock_Returns data set I will focus my analysis on the LCL_Indices
data set. Even though this data set has fewer missing values, I still
need to deal with them appropriately.

Given that we have 4281 total observations is not drastically
substantial, I will therefore remove all rows containing NA’s. This
should still leave us with a sufficient amount of observations to
conduct our volatility analysis.

``` r
#Creating a new cleaned data set that has no NA's 
LCL_Indices_Cleaned <- na.omit(LCL_Indices)

#Evaluating whether the NA's have been removed successfully
sum(is.na(LCL_Indices_Cleaned))
```

    ## [1] 0

``` r
#Evaluating the amount of data still available
nrow(LCL_Indices_Cleaned)
```

    ## [1] 3679

The NA’s have now successfully been removed and we can continue with the
analysis.

# Methodology

The main aim of this analysis is to evaluate the relationship between
different indices and evaluate how they move with respect to each other.
This information can be vitally important in portfolio creation as one
can better diversify their portfolio and reduce risk exposure.

## Evaluating correlation

When attempting to use a GARCH model to evaluate the relationship
between different time series, as in this project, it is important to do
a correlation analysis. The purpose of this correlation analysis is to
identify any multicolinearity issue and for me to better understand the
market dynamics. Additionally when running the GARCH models we may run
into issues with the cholseky decompostion if we use variables that are
too highly correlated.

### Reshaping the data

Here I am reshaping the cleaned data from a long format to a wide format
using the spread function, to make it easier for me to do the
correlation analysis.

``` r
# Reshaping the LCL_Indices_Cleaned data to a wide format
LCL_Indices_wide <- LCL_Indices_Cleaned %>%
  select(date, Name, Returns) %>%
  spread(Name, Returns)
```

### Calculating the correlation matrix

In this code chunk I run the correlation analysis using the cor
function. Here I exclude the date column because it is not needed when
running the correlation matrix. In this next code chunk I will attempt
to write code that will identify highly correlated index pairs. Then I
will be able to make a better decision on which index pairs to evaluate
in the volatility model.
