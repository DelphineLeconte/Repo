Exercise 1
================

## Load packages

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
library(igraph)
```

    ## 
    ## Attaching package: 'igraph'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     as_data_frame, groups, union

    ## The following objects are masked from 'package:purrr':
    ## 
    ##     compose, simplify

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     crossing

    ## The following object is masked from 'package:tibble':
    ## 
    ##     as_data_frame

    ## The following objects are masked from 'package:stats':
    ## 
    ##     decompose, spectrum

    ## The following object is masked from 'package:base':
    ## 
    ##     union

## 1. Downloaded Contact informations from Linkedin

## 2. Import data into R

``` r
library(readr)
connections <- read_csv("Connections.csv")
```

    ## Rows: 1316 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (6): FirstName, LastName, EmailAddress, Company, Position, ConnectedOn
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
head(connections)
```

    ## # A tibble: 6 × 6
    ##   FirstName LastName                 EmailAddress   Company Position ConnectedOn
    ##   <chr>     <chr>                    <chr>          <chr>   <chr>    <chr>      
    ## 1 Haider    Abbas, PhD               <NA>           Confid… Applied… 04-May-22  
    ## 2 Norvan    Gharabegi, P.Eng., MASc. <NA>           GlobVi… Operati… 03-May-22  
    ## 3 Jessica   Jonas                    <NA>           Sanofi  Marketi… 03-May-22  
    ## 4 Husham    Hajhamid                 <NA>           Autode… Product… 02-May-22  
    ## 5 Vitor     Nobre Freire             <NA>           Golden… Busines… 02-May-22  
    ## 6 Alejandra Rodríguez Vidrio         ale.rdz.vidri… Invest… Intern,… 02-May-22

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

## 4. Create list of nodes

inspired by

``` r
connections = connections %>%  unite(name, c("FirstName","LastName" ))
nodes = connections %>% select(c("name", "Company"))
nodes = nodes %>% rowid_to_column("id")
nodes %>% head(10)
```

    ## # A tibble: 10 × 3
    ##       id name                            Company                                
    ##    <int> <chr>                           <chr>                                  
    ##  1     1 Haider_Abbas, PhD               Confidential                           
    ##  2     2 Norvan_Gharabegi, P.Eng., MASc. GlobVision                             
    ##  3     3 Jessica_Jonas                   Sanofi                                 
    ##  4     4 Husham_Hajhamid                 Autodesk                               
    ##  5     5 Vitor_Nobre Freire              Golden Equity Properties               
    ##  6     6 Alejandra_Rodríguez Vidrio      Investissements PSP                    
    ##  7     7 Stephani_Arulanandam            BMO Capital Markets                    
    ##  8     8 Michael_Bourkas                 McGill Not-For-Profit Consulting (MNFP…
    ##  9     9 Michael_Chuang                  KOLABLE                                
    ## 10    10 Poorva_Jalan                    Pratt & Whitney Canada

## 5. Create list of edges

``` r
edges = connections %>% select(c(name, Company)) %>% left_join(nodes %>% select(c(id,name)), by = c("name"="name"))
edges = edges %>% left_join(edges, by = "Company", keep=FALSE) %>% select(c("id.x", "id.y", "Company")) %>% filter(id.x!=id.y)

colnames(edges) = c("x", "y", "Company")
edges %>% head(10)
```

    ## # A tibble: 10 × 3
    ##        x     y Company                                 
    ##    <int> <int> <chr>                                   
    ##  1     3  1306 Sanofi                                  
    ##  2     4    40 Autodesk                                
    ##  3     4   123 Autodesk                                
    ##  4     8   100 McGill Not-For-Profit Consulting (MNFPC)
    ##  5    10    22 Pratt & Whitney Canada                  
    ##  6    12   249 Trouw Nutrition                         
    ##  7    14    53 Abbott                                  
    ##  8    16   367 Self-employed                           
    ##  9    16   866 Self-employed                           
    ## 10    16   933 Self-employed

## 6. Plot

``` r
linkedIn_graph = graph_from_data_frame(d=edges,vertices=NULL ,directed=FALSE)
linkedIn_graph
```

    ## IGRAPH dde7f5b UN-- 331 20044 -- 
    ## + attr: name (v/c), Company (e/c)
    ## + edges from dde7f5b (vertex names):
    ##  [1] 3 --1306 4 --40   4 --123  8 --100  10--22   12--249  14--53   16--367 
    ##  [9] 16--866  16--933  17--80   17--96   19--70   19--151  19--164  19--211 
    ## [17] 19--219  19--227  19--234  19--253  19--255  19--277  19--281  19--292 
    ## [25] 19--293  19--301  19--335  19--336  19--338  19--340  19--342  19--343 
    ## [33] 19--344  19--346  19--347  19--358  19--419  19--422  19--436  19--452 
    ## [41] 19--464  19--470  19--471  19--472  19--475  19--477  19--485  19--490 
    ## [49] 19--496  19--516  19--589  21--66   10--22   24--35   24--79   26--63  
    ## [57] 26--128  24--35   35--79   37--101  37--118  37--1083 37--1149 37--1188
    ## + ... omitted several edges

``` r
#did not achieve to plot
```
