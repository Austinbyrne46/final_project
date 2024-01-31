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
    ## Ncells 467401 25.0    1005238 53.7   660385 35.3
    ## Vcells 863688  6.6    8388608 64.0  1769691 13.6

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
