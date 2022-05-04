Exercise 1
================

## load packages

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✔ ggplot2 3.3.5     ✔ purrr   0.3.4
    ## ✔ tibble  3.1.6     ✔ dplyr   1.0.9
    ## ✔ tidyr   1.2.0     ✔ stringr 1.4.0
    ## ✔ readr   2.1.2     ✔ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(dplyr)
```

## 1. Downloaded Contact informations from Linkedin

## 2. Import data into R

``` r
library(readr)
connections <- read_csv("Connections.csv")
```

    ## Rows: 1316 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (6): First Name, Last Name, Email Address, Company, Position, Connected On
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
View(connections)
```

## 3. Count contacts by current employer

``` r
connections %>% 
  count(Company) %>% 
  arrange(-n)
```

    ## # A tibble: 1,078 × 2
    ##    Company                                                 n
    ##    <chr>                                               <int>
    ##  1 Sollio Agriculture                                     40
    ##  2 <NA>                                                   28
    ##  3 Syngenta                                               14
    ##  4 La Coop fédérée                                        10
    ##  5 McGill University - Desautels Faculty of Management     7
    ##  6 BOMBARDIER                                              6
    ##  7 EY                                                      6
    ##  8 InVivo                                                  5
    ##  9 Nutrien                                                 5
    ## 10 SGS                                                     5
    ## # … with 1,068 more rows

## 3. Count contacts Total

``` r
connections %>% 
  count()
```

    ## # A tibble: 1 × 1
    ##       n
    ##   <int>
    ## 1  1316

## 4. Create node list

We will deal with First Name and Company columns add a column with the
values from the row ids

``` r
connections <- connections %>% rowid_to_column("Id") 
```
