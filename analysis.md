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
```

### Load VAERS Events

``` r
# Define all the various ways NA was defined (not exhaustive)
na_list = c('', 'NA', 'unknown',  'UKN', 'unk', 'Unknown', 'UNKNOWN', 'n\\a', 'n/a', 'N/A', 'N\\A')

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
ggplot(VAERSCOVID19 %>% drop_na(VAX_LOT) %>% group_by(VAX_MANU) %>% count(VAX_LOT) %>% ungroup() %>% slice_max(n, n = 50),
       aes(y = n, x = reorder(VAX_LOT, -n) , fill=VAX_MANU)) +
  geom_bar(stat = 'identity') +
  guides(x=guide_axis(angle = 45)) + xlab("Lot ID") + ylab("Number of Adverse Events")
```

![](./figures/report%20frequency-1.png)<!-- -->