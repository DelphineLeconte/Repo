Exercise 3
================

## Install packages

to read parquet file + race + gender

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

``` r
library(tidygraph)
```

    ## 
    ## Attaching package: 'tidygraph'

    ## The following object is masked from 'package:igraph':
    ## 
    ##     groups

    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

``` r
library(ggraph)
library(readr)
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:igraph':
    ## 
    ##     %--%, union

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(arrow)
```

    ## 
    ## Attaching package: 'arrow'

    ## The following object is masked from 'package:lubridate':
    ## 
    ##     duration

    ## The following object is masked from 'package:utils':
    ## 
    ##     timestamp

``` r
library(gender)
library(wru)
library(ggplot2)
library(gridExtra)
```

    ## 
    ## Attaching package: 'gridExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

``` r
library(grid)
```

## Load data

``` r
applications <- read_parquet(paste0("app_data_sample.parquet"))
edges <- read_csv("edges_sample.csv")
```

    ## Rows: 32906 Columns: 4
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl  (3): application_number, ego_examiner_id, alter_examiner_id
    ## date (1): advice_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
head(edges)
```

    ## # A tibble: 6 × 4
    ##   application_number advice_date ego_examiner_id alter_examiner_id
    ##                <dbl> <date>                <dbl>             <dbl>
    ## 1            9402488 2008-11-17            84356             66266
    ## 2            9402488 2008-11-17            84356             63519
    ## 3            9402488 2008-11-17            84356             98531
    ## 4            9445135 2008-08-21            92953             71313
    ## 5            9445135 2008-08-21            92953             93865
    ## 6            9445135 2008-08-21            92953             91818

``` r
head(applications)
```

    ## # A tibble: 6 × 16
    ##   application_number filing_date examiner_name_last examiner_name_first
    ##   <chr>              <date>      <chr>              <chr>              
    ## 1 08284457           2000-01-26  HOWARD             JACQUELINE         
    ## 2 08413193           2000-10-11  YILDIRIM           BEKIR              
    ## 3 08531853           2000-05-17  HAMILTON           CYNTHIA            
    ## 4 08637752           2001-07-20  MOSHER             MARY               
    ## 5 08682726           2000-04-10  BARR               MICHAEL            
    ## 6 08687412           2000-04-28  GRAY               LINDA              
    ## # … with 12 more variables: examiner_name_middle <chr>, examiner_id <dbl>,
    ## #   examiner_art_unit <dbl>, uspc_class <chr>, uspc_subclass <chr>,
    ## #   patent_number <chr>, patent_issue_date <date>, abandon_date <date>,
    ## #   disposal_type <chr>, appl_status_code <dbl>, appl_status_date <chr>,
    ## #   tc <dbl>

## Select subgroups

``` r
group1 = applications[substr(applications$examiner_art_unit, 1,3)==164,]
group2 = applications[substr(applications$examiner_art_unit, 1,3)==173,]

group1
```

    ## # A tibble: 93,342 × 16
    ##    application_number filing_date examiner_name_last examiner_name_first
    ##    <chr>              <date>      <chr>              <chr>              
    ##  1 08637752           2001-07-20  MOSHER             MARY               
    ##  2 08765941           2000-06-23  FORD               VANESSA            
    ##  3 08901519           2000-09-26  DENT               ALANA              
    ##  4 09000004           2001-05-02  SAUNDERS           DAVID              
    ##  5 09011027           2000-05-01  LANDSMAN           ROBERT             
    ##  6 09077252           2000-05-20  PADMANABHAN        KARTIC             
    ##  7 09077740           2000-01-12  NOLAN              PATRICK            
    ##  8 09091815           2000-04-13  NICKOL             GARY               
    ##  9 09117087           2000-07-28  SALIMI             ALI                
    ## 10 09129758           2010-09-15  PAK                MICHAEL            
    ## # … with 93,332 more rows, and 12 more variables: examiner_name_middle <chr>,
    ## #   examiner_id <dbl>, examiner_art_unit <dbl>, uspc_class <chr>,
    ## #   uspc_subclass <chr>, patent_number <chr>, patent_issue_date <date>,
    ## #   abandon_date <date>, disposal_type <chr>, appl_status_code <dbl>,
    ## #   appl_status_date <chr>, tc <dbl>

## create new dataframe with only those subgroups (faster to process data afterwards)

``` r
bind <- rbind(group1, group2)
glimpse(bind)
```

    ## Rows: 158,146
    ## Columns: 16
    ## $ application_number   <chr> "08637752", "08765941", "08901519", "09000004", "…
    ## $ filing_date          <date> 2001-07-20, 2000-06-23, 2000-09-26, 2001-05-02, …
    ## $ examiner_name_last   <chr> "MOSHER", "FORD", "DENT", "SAUNDERS", "LANDSMAN",…
    ## $ examiner_name_first  <chr> "MARY", "VANESSA", "ALANA", "DAVID", "ROBERT", "K…
    ## $ examiner_name_middle <chr> NA, "L", "HARRIS", "A", "S", NA, "J", "B", "REZA"…
    ## $ examiner_id          <dbl> 73788, 97543, 92931, 64507, 98520, 64900, 97461, …
    ## $ examiner_art_unit    <dbl> 1648, 1645, 1642, 1644, 1647, 1641, 1644, 1642, 1…
    ## $ uspc_class           <chr> "530", "424", "435", "435", "424", "435", "435", …
    ## $ uspc_subclass        <chr> "388300", "001210", "007230", "007210", "085200",…
    ## $ patent_number        <chr> "6927281", NA, "6261766", "6780603", "6387364", "…
    ## $ patent_issue_date    <date> 2005-08-09, NA, 2001-07-17, 2004-08-24, 2002-05-…
    ## $ abandon_date         <date> NA, 2001-08-22, NA, NA, NA, NA, 2001-11-05, 2001…
    ## $ disposal_type        <chr> "ISS", "ABN", "ISS", "ISS", "ISS", "ISS", "ABN", …
    ## $ appl_status_code     <dbl> 250, 161, 250, 250, 150, 150, 161, 161, 150, 161,…
    ## $ appl_status_date     <chr> "07sep2009 00:00:00", "03apr2002 00:00:00", "17au…
    ## $ tc                   <dbl> 1600, 1600, 1600, 1600, 1600, 1600, 1600, 1600, 1…

## Get gender for examiners

get names without repetition:

``` r
examiner_names <- bind %>% 
  distinct(examiner_name_first)
```

Attach a gender and probability to each name and put the results into
the table `examiner_names_gender`

``` r
# get a table of names and gender
examiner_names_gender <- examiner_names %>% 
  do(results = gender(.$examiner_name_first, method = "ssa")) %>% 
  unnest(cols = c(results), keep_empty = TRUE) %>% 
  select(
    examiner_name_first = name,
    gender,
    proportion_female
  )
```

``` r
# remove extra colums from the gender table
examiner_names_gender <- examiner_names_gender %>% 
  select(examiner_name_first, gender)
# joining gender back to the dataset
applications <- applications %>% 
  left_join(examiner_names_gender, by = "examiner_name_first")
# cleaning up
rm(examiner_names)
rm(examiner_names_gender)
gc()
```

    ##            used  (Mb) gc trigger  (Mb) max used  (Mb)
    ## Ncells  4791171 255.9    8512654 454.7  5122365 273.6
    ## Vcells 55182554 421.1   97379206 743.0 87511352 667.7

## Add race

``` r
examiner_surnames <- bind %>% 
  select(surname = examiner_name_last) %>% 
  distinct()
examiner_surnames
```

    ## # A tibble: 388 × 1
    ##    surname    
    ##    <chr>      
    ##  1 MOSHER     
    ##  2 FORD       
    ##  3 DENT       
    ##  4 SAUNDERS   
    ##  5 LANDSMAN   
    ##  6 PADMANABHAN
    ##  7 NOLAN      
    ##  8 NICKOL     
    ##  9 SALIMI     
    ## 10 PAK        
    ## # … with 378 more rows

``` r
examiner_race <- predict_race(voter.file = examiner_surnames, surname.only = T) %>% 
  as_tibble()
```

    ## [1] "Proceeding with surname-only predictions..."

    ## Warning in merge_surnames(voter.file): Probabilities were imputed for 48
    ## surnames that could not be matched to Census list.

``` r
examiner_race
```

    ## # A tibble: 388 × 6
    ##    surname     pred.whi pred.bla pred.his pred.asi pred.oth
    ##    <chr>          <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
    ##  1 MOSHER        0.947  0.00410    0.0241  0.00640  0.0185 
    ##  2 FORD          0.620  0.32       0.0237  0.0045   0.0313 
    ##  3 DENT          0.574  0.374      0.02    0.0052   0.0262 
    ##  4 SAUNDERS      0.674  0.261      0.0262  0.00570  0.0329 
    ##  5 LANDSMAN      0.944  0.0161     0.0185  0.00525  0.0165 
    ##  6 PADMANABHAN   0.0225 0.000600   0.01    0.95     0.0169 
    ##  7 NOLAN         0.868  0.0744     0.0291  0.0058   0.0222 
    ##  8 NICKOL        0.964  0.0089     0.0143  0.00417  0.00833
    ##  9 SALIMI        0.730  0          0.041   0.0964   0.132  
    ## 10 PAK           0.0378 0.00110    0.0088  0.920    0.0321 
    ## # … with 378 more rows

Pick the race category that has the highest probability for each last
name and then join the table back to the main applications table.

``` r
examiner_race <- examiner_race %>% 
  mutate(max_race_p = pmax(pred.asi, pred.bla, pred.his, pred.oth, pred.whi)) %>% 
  mutate(race = case_when(
    max_race_p == pred.asi ~ "Asian",
    max_race_p == pred.bla ~ "black",
    max_race_p == pred.his ~ "Hispanic",
    max_race_p == pred.oth ~ "other",
    max_race_p == pred.whi ~ "white",
    TRUE ~ NA_character_
  ))
examiner_race
```

    ## # A tibble: 388 × 8
    ##    surname     pred.whi pred.bla pred.his pred.asi pred.oth max_race_p race 
    ##    <chr>          <dbl>    <dbl>    <dbl>    <dbl>    <dbl>      <dbl> <chr>
    ##  1 MOSHER        0.947  0.00410    0.0241  0.00640  0.0185       0.947 white
    ##  2 FORD          0.620  0.32       0.0237  0.0045   0.0313       0.620 white
    ##  3 DENT          0.574  0.374      0.02    0.0052   0.0262       0.574 white
    ##  4 SAUNDERS      0.674  0.261      0.0262  0.00570  0.0329       0.674 white
    ##  5 LANDSMAN      0.944  0.0161     0.0185  0.00525  0.0165       0.944 white
    ##  6 PADMANABHAN   0.0225 0.000600   0.01    0.95     0.0169       0.95  Asian
    ##  7 NOLAN         0.868  0.0744     0.0291  0.0058   0.0222       0.868 white
    ##  8 NICKOL        0.964  0.0089     0.0143  0.00417  0.00833      0.964 white
    ##  9 SALIMI        0.730  0          0.041   0.0964   0.132        0.730 white
    ## 10 PAK           0.0378 0.00110    0.0088  0.920    0.0321       0.920 Asian
    ## # … with 378 more rows

Join the data back to the applications table.

``` r
# removing extra columns
examiner_race <- examiner_race %>% 
  select(surname,race)
applications <- applications %>% 
  left_join(examiner_race, by = c("examiner_name_last" = "surname"))
rm(examiner_race)
rm(examiner_surnames)
gc()
```

    ##            used  (Mb) gc trigger  (Mb) max used  (Mb)
    ## Ncells  5131398 274.1    8512654 454.7  5842624 312.1
    ## Vcells 58869527 449.2   97379206 743.0 96004336 732.5

## Add tenure

``` r
examiner_dates <- applications %>% 
  select(examiner_id, filing_date, appl_status_date) 
```

Make dates format consistent (create new variables `start_date` and
\`end_date’)

``` r
examiner_dates <- examiner_dates %>% 
  mutate(start_date = ymd(filing_date), end_date = as_date(dmy_hms(appl_status_date)))
```

Identify the earliest and the latest date for each examiner and
calculate the difference in days, which is their tenure in the
organization.

``` r
examiner_dates <- examiner_dates %>% 
  group_by(examiner_id) %>% 
  summarise(
    earliest_date = min(start_date, na.rm = TRUE), 
    latest_date = max(end_date, na.rm = TRUE),
    tenure_days = interval(earliest_date, latest_date) %/% days(1)
    ) %>% 
  filter(year(latest_date)<2018)
```

Joining back to the applications data.

``` r
applications <- applications %>% 
  left_join(examiner_dates, by = "examiner_id")
rm(examiner_dates)
gc()
```

    ##            used  (Mb) gc trigger   (Mb)  max used   (Mb)
    ## Ncells  5144887 274.8   15413586  823.2  15413586  823.2
    ## Vcells 71246992 543.6  140402056 1071.2 139824606 1066.8
