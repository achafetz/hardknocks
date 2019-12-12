Summarizing Data in R
================
Aaron Chafetz
2019-12-12

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 3.5.3

    ## -- Attaching packages ------------------------------ tidyverse 1.2.1 --

    ## v ggplot2 3.2.0     v purrr   0.3.2
    ## v tibble  2.1.3     v dplyr   0.8.3
    ## v tidyr   0.8.3     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.4.0

    ## Warning: package 'ggplot2' was built under R version 3.5.3

    ## Warning: package 'tibble' was built under R version 3.5.3

    ## Warning: package 'tidyr' was built under R version 3.5.3

    ## Warning: package 'readr' was built under R version 3.5.3

    ## Warning: package 'purrr' was built under R version 3.5.3

    ## Warning: package 'dplyr' was built under R version 3.5.3

    ## Warning: package 'stringr' was built under R version 3.5.3

    ## Warning: package 'forcats' was built under R version 3.5.3

    ## -- Conflicts --------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(ICPIutilities)
```

``` r
#run the below lines of code to save the masked MSD
dataset_url <- "https://media.githubusercontent.com/media/ICPI/TrainingDataset/master/Output/MER_Structured_TRAINING_Datasets_PSNU_IM_FY17-20_20191115_v1_1.txt"
filename <- basename(dataset_url) 

df <- read_msd(dataset_url, save_rds = FALSE)
```

    ## Warning in file.remove(file): cannot remove file 'https://
    ## media.githubusercontent.com/media/ICPI/TrainingDataset/master/Output/
    ## MER_Structured_TRAINING_Datasets_PSNU_IM_FY17-20_20191115_v1_1.txt', reason
    ## 'Invalid argument'

``` r
glimpse(df)
```

    ## Observations: 215,847
    ## Variables: 45
    ## $ region                   <chr> "Milky Way", "Milky Way", "Milky Way"...
    ## $ regionuid                <chr> "JCDsa4GxW5q", "JCDsa4GxW5q", "JCDsa4...
    ## $ operatingunit            <chr> "Saturn", "Saturn", "Saturn", "Saturn...
    ## $ operatingunituid         <chr> "wRhSdzNT5Gl", "wRhSdzNT5Gl", "wRhSdz...
    ## $ countryname              <chr> "Outer Ring", "Outer Ring", "Outer Ri...
    ## $ snu1                     <chr> "Outer Ring II", "Outer Ring II", "Ou...
    ## $ snu1uid                  <chr> "foYAIOUJEgm", "foYAIOUJEgm", "foYAIO...
    ## $ psnu                     <chr> "Dione", "Dione", "Dione", "Dione", "...
    ## $ psnuuid                  <chr> "nqa03O1goVF", "nqa03O1goVF", "nqa03O...
    ## $ snuprioritization        <chr> "1 - Scale-Up: Saturation", "1 - Scal...
    ## $ typemilitary             <chr> "N", "N", "N", "N", "N", "N", "N", "N...
    ## $ dreams                   <chr> "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y...
    ## $ primepartner             <chr> "Sagittarius", "Virgo", "Virgo", "Vir...
    ## $ fundingagency            <chr> "HHS/HRSA", "USAID", "USAID", "USAID"...
    ## $ mech_code                <chr> "01566", "01545", "01545", "01545", "...
    ## $ mech_name                <chr> "Nsh", "Arh", "Arh", "Arh", "Arh", "A...
    ## $ pre_rgnlztn_hq_mech_code <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N...
    ## $ prime_partner_duns       <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N...
    ## $ award_number             <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N...
    ## $ indicator                <chr> "PP_PREV", "OVC_HIVSTAT", "OVC_HIVSTA...
    ## $ numeratordenom           <chr> "N", "N", "N", "N", "N", "N", "N", "N...
    ## $ indicatortype            <chr> "DSD", "DSD", "DSD", "DSD", "DSD", "D...
    ## $ disaggregate             <chr> "Age/Sex", "ReportedStatus", "Reporte...
    ## $ standardizeddisaggregate <chr> "Age/Sex", "ReportedStatus", "Reporte...
    ## $ categoryoptioncomboname  <chr> "10-14, Male", "No HIV Status", "No H...
    ## $ ageasentered             <chr> "10-14", NA, NA, NA, "18-24", "18-24"...
    ## $ trendsfine               <chr> "10-14", NA, NA, NA, "18-24", "18-24"...
    ## $ trendssemifine           <chr> "10-14", NA, NA, NA, "18-24", "18-24"...
    ## $ trendscoarse             <chr> "<15", NA, NA, NA, "18+", "18+", "<18...
    ## $ sex                      <chr> "Male", NA, NA, NA, "Male", "Male", "...
    ## $ statushiv                <chr> NA, "Unknown", "Unknown", NA, NA, NA,...
    ## $ statustb                 <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N...
    ## $ statuscx                 <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N...
    ## $ hiv_treatment_status     <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N...
    ## $ population               <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N...
    ## $ otherdisaggregate        <chr> NA, "No HIV Status", "No HIV Status",...
    ## $ coarsedisaggregate       <chr> "N", "N", "N", "N", "N", "N", "N", "N...
    ## $ modality                 <chr> NA, NA, NA, NA, NA, NA, "N", "N", NA,...
    ## $ fiscal_year              <int> 2017, 2018, 2019, 2019, 2018, 2019, 2...
    ## $ targets                  <dbl> NA, NA, NA, 34060, NA, 1960, NA, NA, ...
    ## $ qtr1                     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, N...
    ## $ qtr2                     <dbl> 170, NA, 30, NA, 3370, NA, 70, 3440, ...
    ## $ qtr3                     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, N...
    ## $ qtr4                     <dbl> 290, 1760, 90, NA, 3510, NA, 190, 520...
    ## $ cumulative               <dbl> 460, 1760, 90, NA, 3510, NA, 190, 520...

``` r
View(df)
```

``` r
df %>% 
  filter(fiscal_year == 2019,
         operatingunit == "Jupiter",
         indicator == "TX_NEW",
         standardizeddisaggregate == "Total Numerator")
```

    ## # A tibble: 18 x 45
    ##    region regionuid operatingunit operatingunituid countryname snu1 
    ##    <chr>  <chr>     <chr>         <chr>            <chr>       <chr>
    ##  1 Milky~ JCDsa4Gx~ Jupiter       SBnIjWsPgKN      Jupiter     Jupi~
    ##  2 Milky~ JCDsa4Gx~ Jupiter       SBnIjWsPgKN      Jupiter     Jupi~
    ##  3 Milky~ JCDsa4Gx~ Jupiter       SBnIjWsPgKN      Jupiter     Jupi~
    ##  4 Milky~ JCDsa4Gx~ Jupiter       SBnIjWsPgKN      Jupiter     Jupi~
    ##  5 Milky~ JCDsa4Gx~ Jupiter       SBnIjWsPgKN      Jupiter     Jupi~
    ##  6 Milky~ JCDsa4Gx~ Jupiter       SBnIjWsPgKN      Jupiter     Jupi~
    ##  7 Milky~ JCDsa4Gx~ Jupiter       SBnIjWsPgKN      Jupiter     Jupi~
    ##  8 Milky~ JCDsa4Gx~ Jupiter       SBnIjWsPgKN      Jupiter     Jupi~
    ##  9 Milky~ JCDsa4Gx~ Jupiter       SBnIjWsPgKN      Jupiter     Jupi~
    ## 10 Milky~ JCDsa4Gx~ Jupiter       SBnIjWsPgKN      Jupiter     Jupi~
    ## 11 Milky~ JCDsa4Gx~ Jupiter       SBnIjWsPgKN      Jupiter     Jupi~
    ## 12 Milky~ JCDsa4Gx~ Jupiter       SBnIjWsPgKN      Jupiter     Jupi~
    ## 13 Milky~ JCDsa4Gx~ Jupiter       SBnIjWsPgKN      Jupiter     Jupi~
    ## 14 Milky~ JCDsa4Gx~ Jupiter       SBnIjWsPgKN      Jupiter     Jupi~
    ## 15 Milky~ JCDsa4Gx~ Jupiter       SBnIjWsPgKN      Jupiter     Jupi~
    ## 16 Milky~ JCDsa4Gx~ Jupiter       SBnIjWsPgKN      Jupiter     Jupi~
    ## 17 Milky~ JCDsa4Gx~ Jupiter       SBnIjWsPgKN      Jupiter     Jupi~
    ## 18 Milky~ JCDsa4Gx~ Jupiter       SBnIjWsPgKN      Jupiter     Jupi~
    ## # ... with 39 more variables: snu1uid <chr>, psnu <chr>, psnuuid <chr>,
    ## #   snuprioritization <chr>, typemilitary <chr>, dreams <chr>,
    ## #   primepartner <chr>, fundingagency <chr>, mech_code <chr>,
    ## #   mech_name <chr>, pre_rgnlztn_hq_mech_code <chr>,
    ## #   prime_partner_duns <chr>, award_number <chr>, indicator <chr>,
    ## #   numeratordenom <chr>, indicatortype <chr>, disaggregate <chr>,
    ## #   standardizeddisaggregate <chr>, categoryoptioncomboname <chr>,
    ## #   ageasentered <chr>, trendsfine <chr>, trendssemifine <chr>,
    ## #   trendscoarse <chr>, sex <chr>, statushiv <chr>, statustb <chr>,
    ## #   statuscx <chr>, hiv_treatment_status <chr>, population <chr>,
    ## #   otherdisaggregate <chr>, coarsedisaggregate <chr>, modality <chr>,
    ## #   fiscal_year <int>, targets <dbl>, qtr1 <dbl>, qtr2 <dbl>, qtr3 <dbl>,
    ## #   qtr4 <dbl>, cumulative <dbl>

``` r
df %>% 
  filter(fiscal_year == 2019,
         operatingunit == "Jupiter",
         indicator == "TX_NEW",
         #standardizeddisaggregate == "Total Numerator"
         ) %>% 
  count(standardizeddisaggregate, trendscoarse, wt = cumulative)
```

    ## # A tibble: 4 x 3
    ##   standardizeddisaggregate          trendscoarse     n
    ##   <chr>                             <chr>        <dbl>
    ## 1 Age/Sex/HIVStatus                 <15            960
    ## 2 Age/Sex/HIVStatus                 15+          19940
    ## 3 PregnantOrBreastfeeding/HIVStatus <NA>           120
    ## 4 Total Numerator                   <NA>         19910

``` r
df %>% 
  filter(fiscal_year == 2019,
         operatingunit == "Jupiter",
         indicator == "TX_NEW",
         standardizeddisaggregate == "Total Numerator"
         ) %>% 
  summarise(cumulative = sum(cumulative, na.rm = TRUE))
```

    ## # A tibble: 1 x 1
    ##   cumulative
    ##        <dbl>
    ## 1      19910

``` r
df %>% 
  filter(fiscal_year == 2019,
         indicator == "TX_NEW",
         standardizeddisaggregate == "Total Numerator"
         ) %>% 
  group_by(operatingunit, psnu, primepartner) %>% 
  summarise(cumulative = sum(cumulative, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(operatingunit, psnu, desc(cumulative)) %>% 
  print(n = Inf)
```

    ## # A tibble: 42 x 4
    ##    operatingunit psnu     primepartner        cumulative
    ##    <chr>         <chr>    <chr>                    <dbl>
    ##  1 Jupiter       Callisto Triangulum Australe       4670
    ##  2 Jupiter       Callisto Canes Venatici              60
    ##  3 Jupiter       Europa   Triangulum Australe       6790
    ##  4 Jupiter       Europa   Canes Venatici             520
    ##  5 Jupiter       Ganymede Triangulum Australe       5210
    ##  6 Jupiter       Ganymede Canes Venatici             150
    ##  7 Jupiter       Himalia  Pegasus                   2160
    ##  8 Jupiter       Himalia  Canes Venatici             240
    ##  9 Jupiter       Himalia  Delphinus                  110
    ## 10 Neptune       Naiad    Boötes                    5380
    ## 11 Neptune       Naiad    Sagittarius               1930
    ## 12 Neptune       Naiad    Dedup                     -100
    ## 13 Neptune       Nereid   Libra                    24540
    ## 14 Neptune       Nereid   Auriga                    7850
    ## 15 Neptune       Nereid   Sagittarius               3080
    ## 16 Neptune       Nereid   Leo                       1140
    ## 17 Neptune       Nereid   Dedup                    -2600
    ## 18 Saturn        Dione    Ursa Major                5050
    ## 19 Saturn        Dione    Auriga                     840
    ## 20 Saturn        Dione    Ophiuchus                  810
    ## 21 Saturn        Dione    Virgo                       30
    ## 22 Saturn        Dione    Canes Venatici              10
    ## 23 Saturn        Mimas    Cepheus                   4080
    ## 24 Saturn        Mimas    Boötes                    3500
    ## 25 Saturn        Mimas    Ophiuchus                 1410
    ## 26 Saturn        Mimas    Triangulum Australe        720
    ## 27 Saturn        Mimas    Eridanus                   610
    ## 28 Saturn        Mimas    Cetus                      480
    ## 29 Saturn        Mimas    Draco                      470
    ## 30 Saturn        Mimas    Aquila                     270
    ## 31 Saturn        Mimas    Virgo                      170
    ## 32 Saturn        Mimas    Canes Venatici              80
    ## 33 Saturn        Mimas    Corvus                      50
    ## 34 Saturn        Thea     Leo                       2940
    ## 35 Saturn        Thea     Auriga                     940
    ## 36 Saturn        Thea     Ophiuchus                  770
    ## 37 Saturn        Thea     Cygnus                     590
    ## 38 Saturn        Thea     Virgo                      500
    ## 39 Saturn        Thea     Orion                      300
    ## 40 Saturn        Thea     Gemini                     140
    ## 41 Saturn        Thea     Canes Venatici             100
    ## 42 Saturn        Thea     Aquila                      40

group\_by\_

``` r
key_ind <- c("operatingunit", "psnu", "primepartner")


df %>% 
  filter(fiscal_year == 2019,
         indicator == "TX_NEW",
         standardizeddisaggregate == "Total Numerator"
         ) %>% 
  group_by_at(key_ind) %>% 
  summarise(cumulative = sum(cumulative, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(operatingunit, psnu, desc(cumulative)) %>% 
  print(n = Inf)
```

    ## # A tibble: 42 x 4
    ##    operatingunit psnu     primepartner        cumulative
    ##    <chr>         <chr>    <chr>                    <dbl>
    ##  1 Jupiter       Callisto Triangulum Australe       4670
    ##  2 Jupiter       Callisto Canes Venatici              60
    ##  3 Jupiter       Europa   Triangulum Australe       6790
    ##  4 Jupiter       Europa   Canes Venatici             520
    ##  5 Jupiter       Ganymede Triangulum Australe       5210
    ##  6 Jupiter       Ganymede Canes Venatici             150
    ##  7 Jupiter       Himalia  Pegasus                   2160
    ##  8 Jupiter       Himalia  Canes Venatici             240
    ##  9 Jupiter       Himalia  Delphinus                  110
    ## 10 Neptune       Naiad    Boötes                    5380
    ## 11 Neptune       Naiad    Sagittarius               1930
    ## 12 Neptune       Naiad    Dedup                     -100
    ## 13 Neptune       Nereid   Libra                    24540
    ## 14 Neptune       Nereid   Auriga                    7850
    ## 15 Neptune       Nereid   Sagittarius               3080
    ## 16 Neptune       Nereid   Leo                       1140
    ## 17 Neptune       Nereid   Dedup                    -2600
    ## 18 Saturn        Dione    Ursa Major                5050
    ## 19 Saturn        Dione    Auriga                     840
    ## 20 Saturn        Dione    Ophiuchus                  810
    ## 21 Saturn        Dione    Virgo                       30
    ## 22 Saturn        Dione    Canes Venatici              10
    ## 23 Saturn        Mimas    Cepheus                   4080
    ## 24 Saturn        Mimas    Boötes                    3500
    ## 25 Saturn        Mimas    Ophiuchus                 1410
    ## 26 Saturn        Mimas    Triangulum Australe        720
    ## 27 Saturn        Mimas    Eridanus                   610
    ## 28 Saturn        Mimas    Cetus                      480
    ## 29 Saturn        Mimas    Draco                      470
    ## 30 Saturn        Mimas    Aquila                     270
    ## 31 Saturn        Mimas    Virgo                      170
    ## 32 Saturn        Mimas    Canes Venatici              80
    ## 33 Saturn        Mimas    Corvus                      50
    ## 34 Saturn        Thea     Leo                       2940
    ## 35 Saturn        Thea     Auriga                     940
    ## 36 Saturn        Thea     Ophiuchus                  770
    ## 37 Saturn        Thea     Cygnus                     590
    ## 38 Saturn        Thea     Virgo                      500
    ## 39 Saturn        Thea     Orion                      300
    ## 40 Saturn        Thea     Gemini                     140
    ## 41 Saturn        Thea     Canes Venatici             100
    ## 42 Saturn        Thea     Aquila                      40

``` r
df %>% 
  filter(fiscal_year == 2019,
         indicator == "TX_NEW",
         standardizeddisaggregate == "Total Numerator"
         ) %>% 
  group_by_if(is.character) %>% 
  summarise(cumulative = sum(cumulative, na.rm = TRUE)) %>% 
  ungroup()
```

    ## # A tibble: 94 x 39
    ##    region regionuid operatingunit operatingunituid countryname snu1 
    ##    <chr>  <chr>     <chr>         <chr>            <chr>       <chr>
    ##  1 Milky~ JCDsa4Gx~ Jupiter       SBnIjWsPgKN      Jupiter     Jupi~
    ##  2 Milky~ JCDsa4Gx~ Jupiter       SBnIjWsPgKN      Jupiter     Jupi~
    ##  3 Milky~ JCDsa4Gx~ Jupiter       SBnIjWsPgKN      Jupiter     Jupi~
    ##  4 Milky~ JCDsa4Gx~ Jupiter       SBnIjWsPgKN      Jupiter     Jupi~
    ##  5 Milky~ JCDsa4Gx~ Jupiter       SBnIjWsPgKN      Jupiter     Jupi~
    ##  6 Milky~ JCDsa4Gx~ Jupiter       SBnIjWsPgKN      Jupiter     Jupi~
    ##  7 Milky~ JCDsa4Gx~ Jupiter       SBnIjWsPgKN      Jupiter     Jupi~
    ##  8 Milky~ JCDsa4Gx~ Jupiter       SBnIjWsPgKN      Jupiter     Jupi~
    ##  9 Milky~ JCDsa4Gx~ Jupiter       SBnIjWsPgKN      Jupiter     Jupi~
    ## 10 Milky~ JCDsa4Gx~ Jupiter       SBnIjWsPgKN      Jupiter     Jupi~
    ## # ... with 84 more rows, and 33 more variables: snu1uid <chr>, psnu <chr>,
    ## #   psnuuid <chr>, snuprioritization <chr>, typemilitary <chr>,
    ## #   dreams <chr>, primepartner <chr>, fundingagency <chr>,
    ## #   mech_code <chr>, mech_name <chr>, pre_rgnlztn_hq_mech_code <chr>,
    ## #   prime_partner_duns <chr>, award_number <chr>, indicator <chr>,
    ## #   numeratordenom <chr>, indicatortype <chr>, disaggregate <chr>,
    ## #   standardizeddisaggregate <chr>, categoryoptioncomboname <chr>,
    ## #   ageasentered <chr>, trendsfine <chr>, trendssemifine <chr>,
    ## #   trendscoarse <chr>, sex <chr>, statushiv <chr>, statustb <chr>,
    ## #   statuscx <chr>, hiv_treatment_status <chr>, population <chr>,
    ## #   otherdisaggregate <chr>, coarsedisaggregate <chr>, modality <chr>,
    ## #   cumulative <dbl>

summarize\_

``` r
df %>% 
  filter(fiscal_year == 2019,
         indicator == "TX_NEW",
         standardizeddisaggregate == "Total Numerator"
         ) %>% 
  group_by_at(key_ind) %>% 
  summarise_at(vars(cumulative), sum, na.rm = TRUE) %>% 
  ungroup()
```

    ## # A tibble: 42 x 4
    ##    operatingunit psnu     primepartner        cumulative
    ##    <chr>         <chr>    <chr>                    <dbl>
    ##  1 Jupiter       Callisto Canes Venatici              60
    ##  2 Jupiter       Callisto Triangulum Australe       4670
    ##  3 Jupiter       Europa   Canes Venatici             520
    ##  4 Jupiter       Europa   Triangulum Australe       6790
    ##  5 Jupiter       Ganymede Canes Venatici             150
    ##  6 Jupiter       Ganymede Triangulum Australe       5210
    ##  7 Jupiter       Himalia  Canes Venatici             240
    ##  8 Jupiter       Himalia  Delphinus                  110
    ##  9 Jupiter       Himalia  Pegasus                   2160
    ## 10 Neptune       Naiad    Boötes                    5380
    ## # ... with 32 more rows

``` r
df %>% 
  filter(fiscal_year == 2019,
         indicator == "TX_NEW",
         standardizeddisaggregate == "Total Numerator"
         ) %>% 
  group_by_at(key_ind) %>% 
  summarise_if(is.double, sum, na.rm = TRUE) %>% 
  ungroup()
```

    ## # A tibble: 42 x 9
    ##    operatingunit psnu  primepartner targets  qtr1  qtr2  qtr3  qtr4
    ##    <chr>         <chr> <chr>          <dbl> <dbl> <dbl> <dbl> <dbl>
    ##  1 Jupiter       Call~ Canes Venat~      20    20    30    10    10
    ##  2 Jupiter       Call~ Triangulum ~    1560  1070  1070   980  1580
    ##  3 Jupiter       Euro~ Canes Venat~     160   120   130   140   140
    ##  4 Jupiter       Euro~ Triangulum ~    2110  1440  1570  1580  2200
    ##  5 Jupiter       Gany~ Canes Venat~      40    50    40    40    40
    ##  6 Jupiter       Gany~ Triangulum ~    2030  1210  1270  1210  1540
    ##  7 Jupiter       Hima~ Canes Venat~     130    50    70    70    70
    ##  8 Jupiter       Hima~ Delphinus         80    10    30    30    50
    ##  9 Jupiter       Hima~ Pegasus         1490   540   530   560   550
    ## 10 Neptune       Naiad Boötes           840  1500  1500  1240  1150
    ## # ... with 32 more rows, and 1 more variable: cumulative <dbl>

Positivity

``` r
df %>% 
  filter(fiscal_year == 2019,
         indicator %in% c("HTS_TST", "HTS_TST_POS"),
         standardizeddisaggregate == "Total Numerator"
         ) %>% 
  group_by(operatingunit, indicator) %>% 
  summarise(cumulative = sum(cumulative, na.rm = TRUE)) %>% 
  ungroup() %>% 
  spread(indicator, cumulative) %>% 
  mutate(positivity = round(HTS_TST_POS/HTS_TST, 3))
```

    ## # A tibble: 3 x 4
    ##   operatingunit HTS_TST HTS_TST_POS positivity
    ##   <chr>           <dbl>       <dbl>      <dbl>
    ## 1 Jupiter        372400       23480      0.063
    ## 2 Neptune        719940       51950      0.072
    ## 3 Saturn        1532620       31360      0.02

Share of Index

``` r
df %>% 
  filter(fiscal_year == 2019,
         operatingunit == "Jupiter",
         indicator == "HTS_TST_POS",
         standardizeddisaggregate == "Modality/Age/Sex/Result") %>% 
  mutate(modality_type = ifelse(str_detect(modality, "Index"), "Index", "Other")) %>% 
  group_by(primepartner, indicator, modality_type) %>% 
  summarise(cumulative = sum(cumulative, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(primepartner) %>% 
  mutate(share_index = cumulative/sum(cumulative, na.rm = TRUE))
```

    ## # A tibble: 11 x 5
    ## # Groups:   primepartner [6]
    ##    primepartner        indicator   modality_type cumulative share_index
    ##    <chr>               <chr>       <chr>              <dbl>       <dbl>
    ##  1 Auriga              HTS_TST_POS Index                880       0.284
    ##  2 Auriga              HTS_TST_POS Other               2220       0.716
    ##  3 Canes Venatici      HTS_TST_POS Index                600       0.297
    ##  4 Canes Venatici      HTS_TST_POS Other               1420       0.703
    ##  5 Capricornus         HTS_TST_POS Other                740       1    
    ##  6 Delphinus           HTS_TST_POS Index                150       0.326
    ##  7 Delphinus           HTS_TST_POS Other                310       0.674
    ##  8 Pegasus             HTS_TST_POS Index                690       0.179
    ##  9 Pegasus             HTS_TST_POS Other               3170       0.821
    ## 10 Triangulum Australe HTS_TST_POS Index               6990       0.364
    ## 11 Triangulum Australe HTS_TST_POS Other              12210       0.636
