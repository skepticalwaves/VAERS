VAERS Analysis
================
Skeptical Waves
2021-12-12

## Setup

The following file must be downloaded and unzipped into the `csvs/`
directory. We can’t download it automatically because captchas.

<https://vaers.hhs.gov/eSubDownload/index.jsp?fn=AllVAERSDataCSVS.zip>

``` r
library(tidyverse)
library(ggplot2)
library(magrittr)
library(summarytools)
library(kableExtra)
```

### Load VAERS Events

``` r
# Define all the various ways NA was defined (not exhaustive)
na_list = c('', 'NA', 'unknown',  'UKN', 'unk', 'Unknown', 'UNKNOWN', 'n\\a', 'n/a', 'N/A', 'N\\A')

# Define specifications to load columns
parsing_spec = cols(
  VAERS_ID = col_character(),
  RECVDATE = col_datetime(format = "%m/%d/%Y"),
  DATEDIED = col_datetime(format = "%m/%d/%Y"),
  RPT_DATE  = col_datetime(format = "%m/%d/%Y"),
  VAX_DATE = col_datetime(format = "%m/%d/%Y"),
  ONSET_DATE = col_datetime(format = "%m/%d/%Y"),
  TODAYS_DATE = col_datetime(format = "%m/%d/%Y"),
  DIED = col_character(),
  ER_VISIT = col_character(),
  X_STAY = col_character()
)

VAERSDATA2021 <- read_csv("csvs/2021VAERSDATA.csv",
                          col_types = parsing_spec,
                          na = na_list)

VAERSDATA2020 <- read_csv("csvs/2020VAERSDATA.csv",
                          col_types = parsing_spec,
                          na = na_list)

VAERSDATANONDOMESTIC <- read_csv("csvs/NonDomesticVAERSDATA.csv",
                                 col_types = parsing_spec,
                                 na = na_list)
```

    ## Warning: One or more parsing issues, see `problems()` for details

``` r
VAERSVAX2021 <- read_csv("csvs/2021VAERSVAX.csv",
                         col_types = parsing_spec,
                         na = na_list)
```

    ## Warning: The following named parsers don't match the column names: RECVDATE,
    ## DATEDIED, RPT_DATE, VAX_DATE, ONSET_DATE, TODAYS_DATE, DIED, ER_VISIT, X_STAY

``` r
VAERSVAX2020 <- read_csv("csvs/2020VAERSVAX.csv",
                         col_types = parsing_spec,
                         na = na_list)
```

    ## Warning: The following named parsers don't match the column names: RECVDATE,
    ## DATEDIED, RPT_DATE, VAX_DATE, ONSET_DATE, TODAYS_DATE, DIED, ER_VISIT, X_STAY

``` r
VAERSVAXNONDOMESTIC <- read_csv("csvs/NonDomesticVAERSVAX.csv",
                                col_types = parsing_spec,
                                na = na_list)
```

    ## Warning: The following named parsers don't match the column names: RECVDATE,
    ## DATEDIED, RPT_DATE, VAX_DATE, ONSET_DATE, TODAYS_DATE, DIED, ER_VISIT, X_STAY

``` r
# Merge
VAERSDATA = rbind(VAERSDATA2021, VAERSDATA2020, VAERSDATANONDOMESTIC)
VAERSVAX = rbind(VAERSVAX2021, VAERSVAX2020, VAERSVAXNONDOMESTIC)

# Final joined dataframe
VAERS = VAERSDATA %>% left_join(VAERSVAX, by = "VAERS_ID")


# Cleanup
rm(
  VAERSDATA2021,
  VAERSDATA2020,
  VAERSDATANONDOMESTIC,
  VAERSVAX2021,
  VAERSVAX2020,
  VAERSVAXNONDOMESTIC,
  VAERSDATA,
  VAERSVAX
)
```

### Filtering

Extract only COVID-19 events, with a start date of 2020-03-27.

``` r
VAERSCOVID19 = VAERS  %>% filter(VAX_TYPE == "COVID19") %>% filter(VAX_DATE >= "2020-12-01")
```

Cleaning up the text:

``` r
VAERSCOVID19 %<>% mutate(VAX_LOT = str_replace_all(VAX_LOT,"[^[:graph:]]", ""))
VAERSCOVID19 %<>% mutate(VAX_LOT = tolower(VAX_LOT))
VAERSCOVID19 %<>% mutate(VAX_LOT = str_replace_all(VAX_LOT, " ", ""))
VAERSCOVID19 %<>% mutate(VAX_LOT = str_replace_all(VAX_LOT, "(moderna|modern|moder|pfizer|pfz|pfr|janssen)", ""))
VAERSCOVID19 %<>% mutate(VAX_LOT = str_replace_all(VAX_LOT, "(lot\\#||lot\\:|lot)", ""))
VAERSCOVID19 %<>% mutate(VAX_LOT = str_replace_all(VAX_LOT, "(^(n|N)o.*|.*unknown.*|.*uknown.*|.*known.*|.*unav.*|.*un.*|.*Unknown.*|.*unk.*|n\\/a|nk|\\?|na)", NA_character_))
VAERSCOVID19 %<>% mutate(VAX_LOT = na_if(VAX_LOT, ""))
#VAERSCOVID19 %<>% drop_na(VAX_LOT)
```

## Figures

### VAERS Reports over time by Manufacturer, by Lot

``` r
ggplot(
  VAERSCOVID19 %>% group_by(VAX_DATE, VAX_MANU) %>% count(VAX_LOT),
  aes(x = VAX_DATE,
      y = n,
      color = VAX_MANU)
) +
  geom_point(alpha = 0.9) + facet_wrap(~ VAX_MANU, ncol = 1) + theme(legend.position =
                                                                       "none") + ylab("Number of Adverse Events")
```

![](./figures/reports%20over%20time-1.png)<!-- -->

### Top 50 Lots with Adverse Events

``` r
VAERSCOVID19_LOT_FREQ = VAERSCOVID19 %>% drop_na(VAX_LOT) %>% group_by(VAX_MANU) %>% count(VAX_LOT) %>% ungroup() %>% slice_max(n, n = 50)
ggplot(VAERSCOVID19_LOT_FREQ, aes(y = n, x = reorder(VAX_LOT, -n) , fill=VAX_MANU)) +
  geom_bar(stat = 'identity') +
  guides(x=guide_axis(angle = 45)) + xlab("Lot ID") + ylab("Number of Adverse Events")
```

![](./figures/report%20frequency-1.png)<!-- -->

#### Table of top 50 lots

``` r
knitr::kable(VAERSCOVID19_LOT_FREQ)
```

<table>
<thead>
<tr>
<th style="text-align:left;">
VAX_MANU
</th>
<th style="text-align:left;">
VAX_LOT
</th>
<th style="text-align:right;">
n
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
MODERNA
</td>
<td style="text-align:left;">
026L20A
</td>
<td style="text-align:right;">
4301
</td>
</tr>
<tr>
<td style="text-align:left;">
MODERNA
</td>
<td style="text-align:left;">
039K20A
</td>
<td style="text-align:right;">
4217
</td>
</tr>
<tr>
<td style="text-align:left;">
MODERNA
</td>
<td style="text-align:left;">
011J20A
</td>
<td style="text-align:right;">
3980
</td>
</tr>
<tr>
<td style="text-align:left;">
PFIZER
</td>
<td style="text-align:left;">
EK5730
</td>
<td style="text-align:right;">
3626
</td>
</tr>
<tr>
<td style="text-align:left;">
PFIZER
</td>
<td style="text-align:left;">
EK9231
</td>
<td style="text-align:right;">
3469
</td>
</tr>
<tr>
<td style="text-align:left;">
PFIZER
</td>
<td style="text-align:left;">
EH9899
</td>
<td style="text-align:right;">
3329
</td>
</tr>
<tr>
<td style="text-align:left;">
PFIZER
</td>
<td style="text-align:left;">
ER2613
</td>
<td style="text-align:right;">
3239
</td>
</tr>
<tr>
<td style="text-align:left;">
MODERNA
</td>
<td style="text-align:left;">
013L20A
</td>
<td style="text-align:right;">
3123
</td>
</tr>
<tr>
<td style="text-align:left;">
MODERNA
</td>
<td style="text-align:left;">
025L20A
</td>
<td style="text-align:right;">
3077
</td>
</tr>
<tr>
<td style="text-align:left;">
MODERNA
</td>
<td style="text-align:left;">
012L20A
</td>
<td style="text-align:right;">
2983
</td>
</tr>
<tr>
<td style="text-align:left;">
PFIZER
</td>
<td style="text-align:left;">
EN6201
</td>
<td style="text-align:right;">
2808
</td>
</tr>
<tr>
<td style="text-align:left;">
MODERNA
</td>
<td style="text-align:left;">
029L20A
</td>
<td style="text-align:right;">
2713
</td>
</tr>
<tr>
<td style="text-align:left;">
MODERNA
</td>
<td style="text-align:left;">
011L20A
</td>
<td style="text-align:right;">
2695
</td>
</tr>
<tr>
<td style="text-align:left;">
PFIZER
</td>
<td style="text-align:left;">
EN5318
</td>
<td style="text-align:right;">
2687
</td>
</tr>
<tr>
<td style="text-align:left;">
MODERNA
</td>
<td style="text-align:left;">
041L20A
</td>
<td style="text-align:right;">
2611
</td>
</tr>
<tr>
<td style="text-align:left;">
PFIZER
</td>
<td style="text-align:left;">
EJ1685
</td>
<td style="text-align:right;">
2610
</td>
</tr>
<tr>
<td style="text-align:left;">
PFIZER
</td>
<td style="text-align:left;">
EN6205
</td>
<td style="text-align:right;">
2570
</td>
</tr>
<tr>
<td style="text-align:left;">
PFIZER
</td>
<td style="text-align:left;">
EL1284
</td>
<td style="text-align:right;">
2565
</td>
</tr>
<tr>
<td style="text-align:left;">
MODERNA
</td>
<td style="text-align:left;">
037K20A
</td>
<td style="text-align:right;">
2564
</td>
</tr>
<tr>
<td style="text-align:left;">
PFIZER
</td>
<td style="text-align:left;">
ER8732
</td>
<td style="text-align:right;">
2563
</td>
</tr>
<tr>
<td style="text-align:left;">
MODERNA
</td>
<td style="text-align:left;">
028L20A
</td>
<td style="text-align:right;">
2549
</td>
</tr>
<tr>
<td style="text-align:left;">
PFIZER
</td>
<td style="text-align:left;">
ER8733
</td>
<td style="text-align:right;">
2519
</td>
</tr>
<tr>
<td style="text-align:left;">
PFIZER
</td>
<td style="text-align:left;">
EL3247
</td>
<td style="text-align:right;">
2501
</td>
</tr>
<tr>
<td style="text-align:left;">
PFIZER
</td>
<td style="text-align:left;">
EN6208
</td>
<td style="text-align:right;">
2499
</td>
</tr>
<tr>
<td style="text-align:left;">
MODERNA
</td>
<td style="text-align:left;">
030L20A
</td>
<td style="text-align:right;">
2498
</td>
</tr>
<tr>
<td style="text-align:left;">
PFIZER
</td>
<td style="text-align:left;">
EN6200
</td>
<td style="text-align:right;">
2413
</td>
</tr>
<tr>
<td style="text-align:left;">
PFIZER
</td>
<td style="text-align:left;">
EK9788
</td>
<td style="text-align:right;">
2411
</td>
</tr>
<tr>
<td style="text-align:left;">
PFIZER
</td>
<td style="text-align:left;">
EN6207
</td>
<td style="text-align:right;">
2408
</td>
</tr>
<tr>
<td style="text-align:left;">
PFIZER
</td>
<td style="text-align:left;">
EP6955
</td>
<td style="text-align:right;">
2340
</td>
</tr>
<tr>
<td style="text-align:left;">
PFIZER
</td>
<td style="text-align:left;">
EN6198
</td>
<td style="text-align:right;">
2312
</td>
</tr>
<tr>
<td style="text-align:left;">
PFIZER
</td>
<td style="text-align:left;">
EN6202
</td>
<td style="text-align:right;">
2284
</td>
</tr>
<tr>
<td style="text-align:left;">
PFIZER
</td>
<td style="text-align:left;">
EL1283
</td>
<td style="text-align:right;">
2278
</td>
</tr>
<tr>
<td style="text-align:left;">
MODERNA
</td>
<td style="text-align:left;">
042L20A
</td>
<td style="text-align:right;">
2267
</td>
</tr>
<tr>
<td style="text-align:left;">
PFIZER
</td>
<td style="text-align:left;">
EL3246
</td>
<td style="text-align:right;">
2227
</td>
</tr>
<tr>
<td style="text-align:left;">
MODERNA
</td>
<td style="text-align:left;">
012M20A
</td>
<td style="text-align:right;">
2221
</td>
</tr>
<tr>
<td style="text-align:left;">
PFIZER
</td>
<td style="text-align:left;">
EW0150
</td>
<td style="text-align:right;">
2218
</td>
</tr>
<tr>
<td style="text-align:left;">
MODERNA
</td>
<td style="text-align:left;">
013M20A
</td>
<td style="text-align:right;">
2215
</td>
</tr>
<tr>
<td style="text-align:left;">
MODERNA
</td>
<td style="text-align:left;">
007M20A
</td>
<td style="text-align:right;">
2193
</td>
</tr>
<tr>
<td style="text-align:left;">
PFIZER
</td>
<td style="text-align:left;">
ER8729
</td>
<td style="text-align:right;">
2182
</td>
</tr>
<tr>
<td style="text-align:left;">
PFIZER
</td>
<td style="text-align:left;">
EN6206
</td>
<td style="text-align:right;">
2153
</td>
</tr>
<tr>
<td style="text-align:left;">
PFIZER
</td>
<td style="text-align:left;">
ER8727
</td>
<td style="text-align:right;">
2135
</td>
</tr>
<tr>
<td style="text-align:left;">
MODERNA
</td>
<td style="text-align:left;">
031L20A
</td>
<td style="text-align:right;">
2128
</td>
</tr>
<tr>
<td style="text-align:left;">
PFIZER
</td>
<td style="text-align:left;">
EN6199
</td>
<td style="text-align:right;">
2101
</td>
</tr>
<tr>
<td style="text-align:left;">
PFIZER
</td>
<td style="text-align:left;">
EN6203
</td>
<td style="text-align:right;">
2095
</td>
</tr>
<tr>
<td style="text-align:left;">
MODERNA
</td>
<td style="text-align:left;">
010M20A
</td>
<td style="text-align:right;">
2049
</td>
</tr>
<tr>
<td style="text-align:left;">
MODERNA
</td>
<td style="text-align:left;">
025J20-2A
</td>
<td style="text-align:right;">
2042
</td>
</tr>
<tr>
<td style="text-align:left;">
PFIZER
</td>
<td style="text-align:left;">
EP7534
</td>
<td style="text-align:right;">
2033
</td>
</tr>
<tr>
<td style="text-align:left;">
PFIZER
</td>
<td style="text-align:left;">
EL3249
</td>
<td style="text-align:right;">
2015
</td>
</tr>
<tr>
<td style="text-align:left;">
PFIZER
</td>
<td style="text-align:left;">
EN6204
</td>
<td style="text-align:right;">
2014
</td>
</tr>
<tr>
<td style="text-align:left;">
PFIZER
</td>
<td style="text-align:left;">
EL9262
</td>
<td style="text-align:right;">
1972
</td>
</tr>
</tbody>
</table>
