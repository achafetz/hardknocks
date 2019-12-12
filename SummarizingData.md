Summarizing Data in R
================
Aaron Chafetz
2019-12-12 \[updated: 2019-12-12\]

Start by loading the packages we need.

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

Access the data

``` r
#run the below lines of code to save the masked MSD
dataset_url <- "https://media.githubusercontent.com/media/ICPI/TrainingDataset/master/Output/MER_Structured_TRAINING_Datasets_PSNU_IM_FY17-20_20191115_v1_1.txt"
filename <- basename(dataset_url) 

df <- read_msd(dataset_url, save_rds = FALSE)
```

To look at the data, we can use `View(df)` but `glimpse()` provides a better over view of the dataset.

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

We're interested in looking at Jupiter's TX\_NEW in 2019.

In Excel, if we filtered the data table (named df), we would get 18 rows that look like this

![image](https://user-images.githubusercontent.com/8933069/70719468-0ddb5d00-1cc0-11ea-86cf-d1e73b230be8.png).

We can do the same thing in R.

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

Filtering gets us part of the way there but we need to summarize or aggregate the filtered rows to get a total.

In Excel, we could write a formula like this:

`=SUMIFS(df[cumulative],df[fiscal_year],2019,df[operatingunit],"Jupiter",df[indicator],"TX_NEW",df[standardizeddisaggregate],"Total Numerator")`

We are telling excel to sum the cumulative column based after it has filtered on year, indicator, etc.

This is extremely similar in R. We can use `count()` with the `wt` argument, specifying that we don't just want a count of rows, but instead want to sum up the `cumulative` column.

``` r
df %>% 
  filter(fiscal_year == 2019,
         operatingunit == "Jupiter",
         indicator == "TX_NEW",
         standardizeddisaggregate == "Total Numerator") %>% 
  count(wt = cumulative)
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <dbl>
    ## 1 19910

If we were interested in comparing operating units, we could take `operatingunit` of the filter and include it in `count()` as a sort of grouping variable.

``` r
df %>% 
  filter(fiscal_year == 2019,
         indicator == "TX_NEW",
         standardizeddisaggregate == "Total Numerator") %>% 
  count(operatingunit, wt = cumulative)
```

    ## # A tibble: 3 x 2
    ##   operatingunit     n
    ##   <chr>         <dbl>
    ## 1 Jupiter       19910
    ## 2 Neptune       41220
    ## 3 Saturn        24900

Starting to think about grouping variable is similar to pivot tables in Excel. The filters are the same as above and then `operatingunit`, our grouping variable is under "Row Labels".

![image](https://user-images.githubusercontent.com/8933069/70720555-0026d700-1cc2-11ea-935b-7ca5a71c49d7.png)

You can group by multiple variables with `count()`. We can add, as an example, in `psnu` as well.

``` r
df %>% 
  filter(fiscal_year == 2019,
         indicator == "TX_NEW",
         standardizeddisaggregate == "Total Numerator") %>% 
  count(operatingunit, psnu, wt = cumulative)
```

    ## # A tibble: 9 x 3
    ##   operatingunit psnu         n
    ##   <chr>         <chr>    <dbl>
    ## 1 Jupiter       Callisto  4730
    ## 2 Jupiter       Europa    7310
    ## 3 Jupiter       Ganymede  5360
    ## 4 Jupiter       Himalia   2510
    ## 5 Neptune       Naiad     7210
    ## 6 Neptune       Nereid   34010
    ## 7 Saturn        Dione     6740
    ## 8 Saturn        Mimas    11840
    ## 9 Saturn        Thea      6320

I find this a good way to look at what disaggregates exist for a variable and do a quick completeness check.

``` r
df %>% 
  filter(fiscal_year == 2019,
         operatingunit == "Jupiter",
         indicator == "TX_NEW") %>% 
  count(standardizeddisaggregate, sex, trendscoarse, wt = cumulative)
```

    ## # A tibble: 7 x 4
    ##   standardizeddisaggregate          sex         trendscoarse     n
    ##   <chr>                             <chr>       <chr>        <dbl>
    ## 1 Age/Sex/HIVStatus                 Female      <15            490
    ## 2 Age/Sex/HIVStatus                 Female      15+          13050
    ## 3 Age/Sex/HIVStatus                 Male        <15            470
    ## 4 Age/Sex/HIVStatus                 Male        15+           6890
    ## 5 Age/Sex/HIVStatus                 Unknown Sex <15              0
    ## 6 PregnantOrBreastfeeding/HIVStatus <NA>        <NA>           120
    ## 7 Total Numerator                   <NA>        <NA>         19910

Alternatively, instead of using `count()` we can use the more robust function, `summarize()` which will allow us to aggregate or weight multiple variables at a time or to create other statistics beyond sum, eg calculating a mean or standard deviation.

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

To add our grouping variable in, we have to add an additional line to identify them and then one more after our summarize function to remove the grouping (so we don't perform any more actions on the dataset when its grouped). I've also added in an extra line at the bottom to sort partners in each operating unit from high to low cumulative value using `arrange()`.

``` r
df %>% 
  filter(fiscal_year == 2019,
         indicator == "TX_NEW",
         standardizeddisaggregate == "Total Numerator"
         ) %>% 
  group_by(operatingunit, primepartner) %>% 
  summarise(cumulative = sum(cumulative, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(operatingunit, primepartner, desc(cumulative))
```

    ## # A tibble: 27 x 3
    ##    operatingunit primepartner        cumulative
    ##    <chr>         <chr>                    <dbl>
    ##  1 Jupiter       Canes Venatici             970
    ##  2 Jupiter       Delphinus                  110
    ##  3 Jupiter       Pegasus                   2160
    ##  4 Jupiter       Triangulum Australe      16670
    ##  5 Neptune       Auriga                    7850
    ##  6 Neptune       Boötes                    5380
    ##  7 Neptune       Dedup                    -2700
    ##  8 Neptune       Leo                       1140
    ##  9 Neptune       Libra                    24540
    ## 10 Neptune       Sagittarius               5010
    ## # ... with 17 more rows

When grouping, there are some extra functions to make out lives easier, The `group_by_at()` function allows us to specify variable in a list and then pass those into the code via `group_by_at()`.

``` r
key_ind <- c("operatingunit", "primepartner")

df %>% 
  filter(fiscal_year == 2019,
         indicator == "TX_NEW",
         standardizeddisaggregate == "Total Numerator"
         ) %>% 
  group_by_at(key_ind) %>% 
  summarise(cumulative = sum(cumulative, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(operatingunit, primepartner, desc(cumulative)) %>% 
  print(n = Inf)
```

    ## # A tibble: 27 x 3
    ##    operatingunit primepartner        cumulative
    ##    <chr>         <chr>                    <dbl>
    ##  1 Jupiter       Canes Venatici             970
    ##  2 Jupiter       Delphinus                  110
    ##  3 Jupiter       Pegasus                   2160
    ##  4 Jupiter       Triangulum Australe      16670
    ##  5 Neptune       Auriga                    7850
    ##  6 Neptune       Boötes                    5380
    ##  7 Neptune       Dedup                    -2700
    ##  8 Neptune       Leo                       1140
    ##  9 Neptune       Libra                    24540
    ## 10 Neptune       Sagittarius               5010
    ## 11 Saturn        Aquila                     310
    ## 12 Saturn        Auriga                    1780
    ## 13 Saturn        Boötes                    3500
    ## 14 Saturn        Canes Venatici             190
    ## 15 Saturn        Cepheus                   4080
    ## 16 Saturn        Cetus                      480
    ## 17 Saturn        Corvus                      50
    ## 18 Saturn        Cygnus                     590
    ## 19 Saturn        Draco                      470
    ## 20 Saturn        Eridanus                   610
    ## 21 Saturn        Gemini                     140
    ## 22 Saturn        Leo                       2940
    ## 23 Saturn        Ophiuchus                 2990
    ## 24 Saturn        Orion                      300
    ## 25 Saturn        Triangulum Australe        720
    ## 26 Saturn        Ursa Major                5050
    ## 27 Saturn        Virgo                      700

We can also use `group_by_if()` to group by characteristics, eg character, numeric, double, etc.

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

The `summarize()` function also has these extensions, `summarize_at()` and `summarize_if()`. With `summarize_at()` you will need to specify which variable(s) you want to aggregate in`vars()`. This extenstion allows you to summarize multiple variable here.

``` r
df %>% 
  filter(fiscal_year == 2019,
         indicator == "TX_NEW",
         standardizeddisaggregate == "Total Numerator"
         ) %>% 
  group_by(operatingunit, primepartner) %>% 
  summarise_at(vars(targets, cumulative), sum, na.rm = TRUE) %>% 
  ungroup()
```

    ## # A tibble: 27 x 4
    ##    operatingunit primepartner        targets cumulative
    ##    <chr>         <chr>                 <dbl>      <dbl>
    ##  1 Jupiter       Canes Venatici          350        970
    ##  2 Jupiter       Delphinus                80        110
    ##  3 Jupiter       Pegasus                1490       2160
    ##  4 Jupiter       Triangulum Australe    5700      16670
    ##  5 Neptune       Auriga                  620       7850
    ##  6 Neptune       Boötes                  840       5380
    ##  7 Neptune       Dedup                     0      -2700
    ##  8 Neptune       Leo                     550       1140
    ##  9 Neptune       Libra                  5210      24540
    ## 10 Neptune       Sagittarius            1500       5010
    ## # ... with 17 more rows

The `summarize_if()` function allows you to group by characteristics, eg character, numeric, double, etc, just like `group_by_if()`. This ability allows you to specify and summarize all our columns that are double, eg `targets, qtr1, qtr2, qtr3, qtr4, cumulative`

``` r
df %>% 
  filter(fiscal_year == 2019,
         indicator == "TX_NEW",
         standardizeddisaggregate == "Total Numerator"
         ) %>% 
  group_by(operatingunit, primepartner) %>% 
  summarise_if(is.double, sum, na.rm = TRUE) %>% 
  ungroup()
```

    ## # A tibble: 27 x 8
    ##    operatingunit primepartner    targets  qtr1  qtr2  qtr3  qtr4 cumulative
    ##    <chr>         <chr>             <dbl> <dbl> <dbl> <dbl> <dbl>      <dbl>
    ##  1 Jupiter       Canes Venatici      350   240   270   260   260        970
    ##  2 Jupiter       Delphinus            80    10    30    30    50        110
    ##  3 Jupiter       Pegasus            1490   540   530   560   550       2160
    ##  4 Jupiter       Triangulum Aus~    5700  3720  3910  3770  5320      16670
    ##  5 Neptune       Auriga              620  1470  1520  2040  2840       7850
    ##  6 Neptune       Boötes              840  1500  1500  1240  1150       5380
    ##  7 Neptune       Dedup                 0  -730  -770  -560  -630      -2700
    ##  8 Neptune       Leo                 550   300   270   240   340       1140
    ##  9 Neptune       Libra              5210  6830  6350  6070  5320      24540
    ## 10 Neptune       Sagittarius        1500  1270  1340  1160  1260       5010
    ## # ... with 17 more rows

Let's move onto a more complete example. We're interested in comparing the positivity of across operating units last year. We need to start by filtering by year, indicator, and disaggregation. We need to group by operating unit and indicator (since we need both HTS\_TST and HTS\_TST\_POS to calculate positivity).

``` r
df_hts <- df %>% 
  filter(fiscal_year == 2019,
         indicator %in% c("HTS_TST", "HTS_TST_POS"),
         standardizeddisaggregate == "Total Numerator"
         ) %>% 
  group_by(operatingunit, indicator) %>% 
  summarise(cumulative = sum(cumulative, na.rm = TRUE)) %>% 
  ungroup()
```

We are introducing a new function, `spread()` which allows us to reshape the dataset wide so we can then calculate (via `mutate()`) the positivity.

``` r
df_hts %>% 
  spread(indicator, cumulative) %>% 
  mutate(positivity = round(HTS_TST_POS/HTS_TST, 3))
```

    ## # A tibble: 3 x 4
    ##   operatingunit HTS_TST HTS_TST_POS positivity
    ##   <chr>           <dbl>       <dbl>      <dbl>
    ## 1 Jupiter        372400       23480      0.063
    ## 2 Neptune        719940       51950      0.072
    ## 3 Saturn        1532620       31360      0.02

We can use `group_by()` for more than aggregation. In the case below, we want to find what share of positive test were from index across partners. We will need to group by primepartner to determine what share is from index and not. Share of Index

We start with out standard filtering, using the Modality/Age/Sex/Result disagg so we can identify index and non-index tests.

``` r
df_hts_pos <- df %>% 
  filter(fiscal_year == 2019,
         operatingunit == "Jupiter",
         indicator == "HTS_TST_POS",
         standardizeddisaggregate == "Modality/Age/Sex/Result")
```

Next we need to adjust the modalities, flagging anything that has Index (either Index or IndexMod) as Index and everything else as other before we aggregate the data.

``` r
(df_hts_pos <- df_hts_pos %>% 
  mutate(modality_type = ifelse(str_detect(modality, "Index"), "Index", "Other")) %>% 
  group_by(primepartner, indicator, modality_type) %>% 
  summarise(cumulative = sum(cumulative, na.rm = TRUE)) %>% 
  ungroup()) 
```

    ## # A tibble: 11 x 4
    ##    primepartner        indicator   modality_type cumulative
    ##    <chr>               <chr>       <chr>              <dbl>
    ##  1 Auriga              HTS_TST_POS Index                880
    ##  2 Auriga              HTS_TST_POS Other               2220
    ##  3 Canes Venatici      HTS_TST_POS Index                600
    ##  4 Canes Venatici      HTS_TST_POS Other               1420
    ##  5 Capricornus         HTS_TST_POS Other                740
    ##  6 Delphinus           HTS_TST_POS Index                150
    ##  7 Delphinus           HTS_TST_POS Other                310
    ##  8 Pegasus             HTS_TST_POS Index                690
    ##  9 Pegasus             HTS_TST_POS Other               3170
    ## 10 Triangulum Australe HTS_TST_POS Index               6990
    ## 11 Triangulum Australe HTS_TST_POS Other              12210

Lastly, we need to group by `primepartner` to create shares so we can have the sum of the shares for each partner sum to 100%.

``` r
df_hts_pos %>% 
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
