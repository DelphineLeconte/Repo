Exercise 2
================

## Load libraries

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
library(tidygraph)
```

    ## 
    ## Attaching package: 'tidygraph'

    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

``` r
library(ggraph)
library(readr)
```

## Load nodes table

``` r
nodes <- read_csv("Ex2 - NODES.csv")
```

    ## New names:
    ## Rows: 10 Columns: 2
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (1): NAME dbl (1): ...1
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## • `` -> `...1`

``` r
head(nodes)
```

    ## # A tibble: 6 × 2
    ##    ...1 NAME 
    ##   <dbl> <chr>
    ## 1     1 A    
    ## 2     2 B    
    ## 3     3 C    
    ## 4     4 D    
    ## 5     5 One  
    ## 6     6 Two

## Load edges table

``` r
edges <- read_csv("Ex2 - EDGES.csv")
```

    ## New names:
    ## Rows: 17 Columns: 3
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (2): FROM, TO dbl (1): ...1
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## • `` -> `...1`

``` r
head(edges)
```

    ## # A tibble: 6 × 3
    ##    ...1 FROM  TO   
    ##   <dbl> <chr> <chr>
    ## 1     1 One   Two  
    ## 2     2 Two   A    
    ## 3     3 A     B    
    ## 4     4 A     C    
    ## 5     5 B     C    
    ## 6     6 B     Three
