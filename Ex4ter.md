Exercise 4
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
applications <- read_parquet("app_data_sample.parquet")
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

## Add attributes

``` r
examiner_names <- applications %>% 
  distinct(examiner_name_first)
# get a table of names and gender
examiner_names_gender <- examiner_names %>% 
  do(results = gender(.$examiner_name_first, method = "ssa")) %>% 
  unnest(cols = c(results), keep_empty = TRUE) %>% 
  select(
    examiner_name_first = name,
    gender,
    proportion_female
  )
examiner_names_gender
```

    ## # A tibble: 1,822 × 3
    ##    examiner_name_first gender proportion_female
    ##    <chr>               <chr>              <dbl>
    ##  1 AARON               male              0.0082
    ##  2 ABDEL               male              0     
    ##  3 ABDOU               male              0     
    ##  4 ABDUL               male              0     
    ##  5 ABDULHAKIM          male              0     
    ##  6 ABDULLAH            male              0     
    ##  7 ABDULLAHI           male              0     
    ##  8 ABIGAIL             female            0.998 
    ##  9 ABIMBOLA            female            0.944 
    ## 10 ABRAHAM             male              0.0031
    ## # … with 1,812 more rows

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
    ## Ncells  4787815 255.7    8218738 439.0  4807820 256.8
    ## Vcells 50013286 381.6   93604824 714.2 80329066 612.9

``` r
examiner_surnames <- applications %>% 
  select(surname = examiner_name_last) %>% 
  distinct()
examiner_race <- predict_race(voter.file = examiner_surnames, surname.only = T) %>% 
  as_tibble()
```

    ## [1] "Proceeding with surname-only predictions..."

    ## Warning in merge_surnames(voter.file): Probabilities were imputed for 698
    ## surnames that could not be matched to Census list.

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
    ## Ncells  5127539 273.9    8218738 439.0  5769753 308.2
    ## Vcells 53699305 409.7   93604824 714.2 92470234 705.5

``` r
examiner_dates <- applications %>% 
  select(examiner_id, filing_date, appl_status_date) 
examiner_dates <- examiner_dates %>% 
  mutate(start_date = ymd(filing_date), end_date = as_date(dmy_hms(appl_status_date)))

examiner_dates <- examiner_dates %>% 
  group_by(examiner_id) %>% 
  summarise(
    earliest_date = min(start_date, na.rm = TRUE), 
    latest_date = max(end_date, na.rm = TRUE),
    tenure_days = interval(earliest_date, latest_date) %/% days(1)
    ) %>% 
  filter(year(latest_date)<2018)


applications <- applications %>% 
  left_join(examiner_dates, by = "examiner_id")
rm(examiner_dates)
gc()
```

    ##            used  (Mb) gc trigger   (Mb)  max used   (Mb)
    ## Ncells  5142026 274.7   14572047  778.3  14572047  778.3
    ## Vcells 66078900 504.2  134966945 1029.8 134656556 1027.4

## 1. Create application processing time variable

``` r
# compute the final decision date as either abandon date or patent issue date
application_dates <- applications %>% 
    mutate(decision_date = coalesce(abandon_date,patent_issue_date)) %>%
    select(application_number,filing_date, abandon_date, patent_issue_date, decision_date, examiner_id, examiner_art_unit, gender, race, tenure_days) %>%
    filter(!is.na(decision_date))

# compute the application processing time as the difference of filing date and decision date
application_dates <- application_dates %>% 
    #mutate(app_proc_time = decision_date - filing_date)
    mutate(app_proc_time = difftime(decision_date, filing_date, units = "days"))


# filter out negative and outlying application processing time
application_dates <- application_dates %>% 
    filter(app_proc_time>ddays(0)) %>% 
    filter(app_proc_time<ddays(10000))
```

## Estimate relationship between centrality and application processing time

``` r
# get the workgroup from art unit as rounding down to digit tenth.
application_dates <- application_dates %>%
  mutate(wg = (application_dates$examiner_art_unit%/%10) * 10)

# Find out which is the dominating workgroup an examiner handled the applications for.
library(plyr)
```

    ## ------------------------------------------------------------------------------

    ## You have loaded plyr after dplyr - this is likely to cause problems.
    ## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
    ## library(plyr); library(dplyr)

    ## ------------------------------------------------------------------------------

    ## 
    ## Attaching package: 'plyr'

    ## The following objects are masked from 'package:tidygraph':
    ## 
    ##     arrange, mutate, rename

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     arrange, count, desc, failwith, id, mutate, rename, summarise,
    ##     summarize

    ## The following object is masked from 'package:purrr':
    ## 
    ##     compact

``` r
library(dplyr)
library(lubridate)
application_dates <- mutate(
  application_dates,
  period = case_when(
    filing_date<ymd("2007-01-01") ~ NA_character_,
    filing_date<ymd("2008-01-01") ~ "t0",
    filing_date<ymd("2009-01-01") ~ "t1",
    filing_date<ymd("2010-01-01") ~ "t2",
    filing_date<ymd("2011-01-01") ~ "t3",
    filing_date<ymd("2012-01-01") ~ "t4",
    filing_date<ymd("2013-01-01") ~ "t5",
    filing_date<ymd("2014-01-01") ~ "t6",
    filing_date<ymd("2015-01-01") ~ "t7",
    filing_date<ymd("2016-01-01") ~ "t8",
    TRUE~ NA_character_)
  )

# get number of applications
library(plyr)
examiner_wg_napp <- ddply(application_dates, .(examiner_id, period, wg), nrow)
names(examiner_wg_napp) <- c("examiner_id","period", "wg", "n_applications")

# assume an examiner belong to the wg he/she most frequently handled applications for, if tie take the greater wg
examiner_wg_napp <- examiner_wg_napp[order(examiner_wg_napp$examiner_id, examiner_wg_napp$period, -(examiner_wg_napp$n_applications), -(examiner_wg_napp$wg)), ] ### sort first
examiner_wg <- examiner_wg_napp [!duplicated(examiner_wg_napp[c(1,2)]),]
examiner_wg <- select(examiner_wg, c("examiner_id","wg","period"))
examiner_wg <- drop_na(examiner_wg)

rm(examiner_wg_napp)




# compute average application processing time

cols <- c("examiner_id","period", "wg", "gender", "race", "tenure_days")

examiners <- application_dates %>%
    group_by(across(all_of(cols))) %>%
    dplyr::summarize(mean_app_proc_time = mean(app_proc_time, na.rm=TRUE), n_app = n()) %>%
    drop_na()
```

    ## `summarise()` has grouped output by 'examiner_id', 'period', 'wg', 'gender',
    ## 'race'. You can override using the `.groups` argument.

``` r
# compute average application processing time

cols <- c("examiner_id","period", "wg", "gender", "race", "tenure_days")

examiners <- application_dates %>%
    group_by(across(all_of(cols))) %>%
    dplyr::summarize(mean_app_proc_time = mean(app_proc_time, na.rm=TRUE), n_app = n()) %>%
    drop_na()
```

    ## `summarise()` has grouped output by 'examiner_id', 'period', 'wg', 'gender',
    ## 'race'. You can override using the `.groups` argument.

``` r
# subset from applications examiners who belong to the two selected work groups
examiner_aus <- examiners %>%
    filter(period == "t1") %>% 
    #filter(wg == 164 | wg == 173) %>%
    select(wg, examiner_id, gender, race, tenure_days, mean_app_proc_time, n_app) %>%
    distinct(examiner_id, .keep_all=TRUE) %>% 
    drop_na() 
```

    ## Adding missing grouping variables: `period`

``` r
# subset from edges examiners who belong to the two selected work groups
edges_aus <- edges %>%
  filter(ego_examiner_id %in% examiner_aus$examiner_id) %>%
  filter(alter_examiner_id %in% examiner_aus$examiner_id) %>%
  drop_na() #585

# merge work group information
network <- left_join(edges_aus, examiner_aus, by = c("ego_examiner_id" = "examiner_id"))
colnames(network)[6] <- "ego_examiner_wg"
colnames(network)[7] <- "ego_examiner_gender"
colnames(network)[8] <- "ego_examiner_race"
colnames(network)[9] <- "ego_examiner_tenure"
colnames(network)[10] <- "ego_examiner_appprooctime"
colnames(network)[11] <- "ego_examiner_napp"
network <- subset(network, select = -c(period))
network <- left_join(network, examiner_aus, by = c("alter_examiner_id" = "examiner_id"))
colnames(network)[12] <- "alter_examiner_wg"
colnames(network)[13] <- "alter_examiner_gender"
colnames(network)[14] <- "alter_examiner_race"
colnames(network)[15] <- "alter_examiner_tenure"
colnames(network)[16] <- "alter_examiner_appprooctime"
colnames(network)[17] <- "alter_examiner_napp"
network <- subset(network, select = -c(period))

# create edge list
edge_list <- select(network, c("ego_examiner_id","alter_examiner_id"))

# create node list
ego <- select(network, c("ego_examiner_id","ego_examiner_wg")) %>%
    dplyr::rename(id=ego_examiner_id, wg=ego_examiner_wg)
alter <- select(network, c("alter_examiner_id","alter_examiner_wg")) %>%
    dplyr::rename(id=alter_examiner_id, wg=alter_examiner_wg)
nodes <- rbind(ego, alter) %>%
  select(id) %>%
  distinct() %>%
  drop_na()

advice_net = graph_from_data_frame(d=edge_list, vertices=nodes, directed=TRUE)
advice_net
```

    ## IGRAPH b23e734 DN-- 1519 20803 -- 
    ## + attr: name (v/c)
    ## + edges from b23e734 (vertex names):
    ##  [1] 84356->63519 92953->91818 92953->91818 72253->61519 72253->72253
    ##  [6] 67078->75772 67078->75772 67078->97328 91688->71059 91688->71059
    ## [11] 91688->67669 91688->67669 61797->78036 94270->81337 94270->81337
    ## [16] 94270->66927 94270->66927 94270->66927 94270->66927 73223->92537
    ## [21] 73223->92537 94270->97655 94270->97655 94270->81337 94270->66927
    ## [26] 94270->66927 60128->69459 75772->99395 75772->99395 75772->99395
    ## [31] 75772->99395 75772->99395 75772->99395 75772->99395 75772->99395
    ## [36] 75772->77746 75772->77746 75772->77746 75772->77746 67713->77746
    ## + ... omitted several edges

``` r
# calculate Centrality 
V(advice_net)$dc <- degree(advice_net)
V(advice_net)$bc <- betweenness(advice_net)
V(advice_net)$ec <- evcent(advice_net)$vector
V(advice_net)$cc <- closeness(advice_net)

# combine the centrality scores
centrality <- data.frame(cbind(nodes$id, V(advice_net)$dc, V(advice_net)$bc, V(advice_net)$ec, V(advice_net)$cc)) 
colnames(centrality)[1] <- "examiner_id"
colnames(centrality)[2] <- "degree_centrality"
colnames(centrality)[3] <- "betweenness_centrality"
colnames(centrality)[4] <- "eigenvector_centrality"
colnames(centrality)[5] <- "closeness_centrality"

# merge centrality to applications
examiner_joined <- left_join(examiner_aus, centrality, by = c("examiner_id" = "examiner_id"))
examiner_joined <- examiner_joined %>%
  drop_na(degree_centrality)

head(examiner_joined)
```

    ## # A tibble: 6 × 12
    ## # Groups:   examiner_id, period, wg, gender, race [6]
    ##   period    wg examiner_id gender race  tenure_days mean_app_proc_time n_app
    ##   <chr>  <dbl>       <dbl> <chr>  <chr>       <dbl> <drtn>             <int>
    ## 1 t1      2120       59056 male   Asian        6268 1513.1000 days        10
    ## 2 t1      2180       59056 male   Asian        6268  879.1481 days        54
    ## 3 t1      2160       59141 female Asian        4582 1839.5556 days        18
    ## 4 t1      2160       59181 female black        6331 1324.6038 days        53
    ## 5 t1      1640       59211 male   white        6332 1044.1250 days        56
    ## 6 t1      1730       59227 female white        6349 1270.4176 days        91
    ## # … with 4 more variables: degree_centrality <dbl>,
    ## #   betweenness_centrality <dbl>, eigenvector_centrality <dbl>,
    ## #   closeness_centrality <dbl>

``` r
rm(examiner_wg)
rm(alter)
rm(ego)
rm(examiner_aus)
rm(edges_aus)
rm(edge_list)
rm(edges)
rm(nodes)
rm(centrality)
```

## Lineaf regression

``` r
# run linear regression to estimate the relationship between centrality and app_proc_time
mreg = lm(as.numeric(mean_app_proc_time)~degree_centrality+betweenness_centrality+eigenvector_centrality+closeness_centrality,
          data=examiner_joined)
summary(mreg)
```

    ## 
    ## Call:
    ## lm(formula = as.numeric(mean_app_proc_time) ~ degree_centrality + 
    ##     betweenness_centrality + eigenvector_centrality + closeness_centrality, 
    ##     data = examiner_joined)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1088.24  -354.91    21.97   304.69  1873.88 
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)             1.357e+03  2.298e+01  59.058  < 2e-16 ***
    ## degree_centrality      -9.361e-01  2.681e-01  -3.492 0.000495 ***
    ## betweenness_centrality -6.844e-03  1.189e-02  -0.576 0.564833    
    ## eigenvector_centrality  5.455e+01  3.839e+02   0.142 0.887022    
    ## closeness_centrality   -4.913e+01  3.806e+01  -1.291 0.196954    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 509.1 on 1350 degrees of freedom
    ##   (843 observations deleted due to missingness)
    ## Multiple R-squared:  0.01142,    Adjusted R-squared:  0.008492 
    ## F-statistic: 3.899 on 4 and 1350 DF,  p-value: 0.003739

``` r
#### linear regression - selected work group 2450 & 2480
examiner_joined_2wg <- examiner_joined %>%
filter(wg == 2450 | wg == 2480)

mreg2 = lm(as.numeric(mean_app_proc_time)~degree_centrality+betweenness_centrality+eigenvector_centrality+closeness_centrality,
          data=examiner_joined_2wg)
summary(mreg2)
```

    ## 
    ## Call:
    ## lm(formula = as.numeric(mean_app_proc_time) ~ degree_centrality + 
    ##     betweenness_centrality + eigenvector_centrality + closeness_centrality, 
    ##     data = examiner_joined_2wg)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -926.83 -202.41   45.55  251.46  900.41 
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)             1.500e+03  7.486e+01  20.032   <2e-16 ***
    ## degree_centrality      -2.117e+00  1.188e+00  -1.783   0.0787 .  
    ## betweenness_centrality  5.354e-03  3.695e-02   0.145   0.8852    
    ## eigenvector_centrality -8.213e+06  1.535e+07  -0.535   0.5943    
    ## closeness_centrality    5.283e+01  1.517e+02   0.348   0.7286    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 423 on 75 degrees of freedom
    ##   (33 observations deleted due to missingness)
    ## Multiple R-squared:  0.05775,    Adjusted R-squared:  0.007495 
    ## F-statistic: 1.149 on 4 and 75 DF,  p-value: 0.3402

``` r
#Overall, the effect of centrality is greater for work groups 2450 and 2480 than in the entire USPTO organization. This is potentially due to the nature of applications that require more communications, collaborations and advice seeking in specific domain subjects. 
```

## Impact of gender

``` r
# male
examiner_joined_m <- examiner_joined %>%
  filter(gender == "male")

mreg3 = lm(as.numeric(mean_app_proc_time)~degree_centrality+betweenness_centrality+eigenvector_centrality+closeness_centrality,
           data=examiner_joined_m)
summary(mreg3)
```

    ## 
    ## Call:
    ## lm(formula = as.numeric(mean_app_proc_time) ~ degree_centrality + 
    ##     betweenness_centrality + eigenvector_centrality + closeness_centrality, 
    ##     data = examiner_joined_m)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1109.05  -346.62    26.79   304.06  1788.31 
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)             1.387e+03  2.684e+01  51.658  < 2e-16 ***
    ## degree_centrality      -1.150e+00  3.214e-01  -3.577 0.000365 ***
    ## betweenness_centrality  1.610e-04  1.342e-02   0.012 0.990432    
    ## eigenvector_centrality  1.997e+02  3.997e+02   0.500 0.617345    
    ## closeness_centrality   -8.984e+01  4.481e+01  -2.005 0.045255 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 513.2 on 969 degrees of freedom
    ##   (605 observations deleted due to missingness)
    ## Multiple R-squared:  0.01605,    Adjusted R-squared:  0.01199 
    ## F-statistic: 3.951 on 4 and 969 DF,  p-value: 0.003461

``` r
# female
examiner_joined_f <- examiner_joined %>%
  filter(gender == "female")

mreg4 = lm(as.numeric(mean_app_proc_time)~degree_centrality+betweenness_centrality+eigenvector_centrality+closeness_centrality,
           data=examiner_joined_f)
summary(mreg4)
```

    ## 
    ## Call:
    ## lm(formula = as.numeric(mean_app_proc_time) ~ degree_centrality + 
    ##     betweenness_centrality + eigenvector_centrality + closeness_centrality, 
    ##     data = examiner_joined_f)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1008.43  -361.46    30.34   307.99  1876.62 
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)             1.277e+03  4.462e+01  28.612   <2e-16 ***
    ## degree_centrality      -3.689e-01  4.849e-01  -0.761    0.447    
    ## betweenness_centrality -3.491e-02  2.606e-02  -1.340    0.181    
    ## eigenvector_centrality -3.238e+03  3.152e+03  -1.027    0.305    
    ## closeness_centrality    6.634e+01  7.223e+01   0.918    0.359    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 496.4 on 376 degrees of freedom
    ##   (238 observations deleted due to missingness)
    ## Multiple R-squared:  0.01609,    Adjusted R-squared:  0.005622 
    ## F-statistic: 1.537 on 4 and 376 DF,  p-value: 0.1907

``` r
library(scales) 
```

    ## 
    ## Attaching package: 'scales'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     discard

    ## The following object is masked from 'package:readr':
    ## 
    ##     col_factor

``` r
plot1 <- ggplot(examiner_joined, aes(gender)) + 
          geom_bar(aes(y = (..count..)/sum(..count..))) + 
          scale_y_continuous(labels=scales::percent) +
          ylab("Relative Frequencies") +
          ggtitle("Gender distribution for USPTO")

plot1
```

![](Ex4ter_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
#Visializing the gender distribution and application processing time by gender for USPTO.

plot2 <- ggplot(examiner_joined, aes(gender,mean_app_proc_time)) + 
          geom_bar(posititon="dodge", stat="summary", fun="mean") + 
          ylab("Mean App Proc Time (Days)") +
          #ylim(0,0.65) +
          ggtitle("App Proc Time for USPTO")
```

    ## Warning: Ignoring unknown parameters: posititon

``` r
## Warning: Ignoring unknown parameters: posititon
grid.arrange(plot1,plot2,ncol=2, widths=c(1,1))
```

    ## Don't know how to automatically pick scale for object of type difftime. Defaulting to continuous.

![](Ex4ter_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->
