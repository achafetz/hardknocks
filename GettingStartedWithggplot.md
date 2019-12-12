Visualizing using ggplot
================
Aaron Chafetz
2019-12-12 \[updated: 2019-12-12\]

``` r
library(ICPIutilities)
library(tidyverse)
library(scales)
library(extrafont)
library(patchwork)
library(gridExtra)
```

``` r
path <- "C:/Users/achafetz/Documents/GitHub/TrainingDataset/Output/MER_Structured_TRAINING_Datasets_PSNU_IM_FY17-20_20191115_v1_1.txt"

df <- read_msd(path, save_rds = FALSE)
```

    ## Warning in file.remove(file): cannot remove file 'C:/
    ## Users/achafetz/Documents/GitHub/TrainingDataset/Output/
    ## MER_Structured_TRAINING_Datasets_PSNU_IM_FY17-20_20191115_v1_1.txt', reason
    ## 'Permission denied'

Share of index pos by funding agency over time
----------------------------------------------

Munge the data

``` r
#filter
  df_hts <- df %>% 
    filter(operatingunit == "Jupiter",
           indicator == "HTS_TST_POS",
           standardizeddisaggregate == "Modality/Age/Sex/Result")

#adjust modality
  df_hts <- df_hts %>% 
    mutate(modality = ifelse(str_detect(modality, "Index"), "Index", "Other"))

#aggregate and reshape
  df_hts <- df_hts %>% 
    group_by(fundingagency, indicator, modality, fiscal_year) %>% 
    summarise_if(is.double, sum, na.rm = TRUE) %>% 
    reshape_msd("long") %>% 
    filter(str_detect(period, "q"))
    
#create share of index
  df_hts <- df_hts %>% 
    group_by(fundingagency, period) %>% 
    mutate(share = val / sum(val, na.rm = TRUE)) %>% 
    ungroup() %>% 
    filter(modality == "Index") %>% 
    arrange(fundingagency, period)
  
  df_hts <- df_hts %>% 
    mutate(period = str_remove(period, "20") %>% toupper)
  
  df_hts <- df_hts %>% 
    mutate(fundingagency = str_remove(fundingagency, "HHS/"),
           fundingagency = factor(fundingagency, c("USAID", "CDC", "DOD")))
  
  df_hts <- df_hts %>% 
    mutate(lab = ifelse(str_detect(period, "Q4"), percent(share, 1), NA))
```

Viz

``` r
(v1 <- df_hts %>% 
  ggplot(aes(x = period, y = val, fill = fundingagency)) +
  geom_hline(aes(yintercept = 0)) +
  geom_col() +
  scale_y_continuous(labels = comma) +
  scale_x_discrete(labels = c("FY17", "", "", "",
                              "FY18", "", "", "",
                              "FY19", "", "", "")) +
  scale_fill_manual(values = c("#002F6C", "#0067B9", "gray60")) +
  facet_grid(fundingagency ~ .) +
  labs(x = NULL, y = NULL,
       title = "Positive Index Tests FY17-FY19",
       caption = "FY19Q4 MSD") +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(family = "Gill Sans MT")))
```

![](GettingStartedWithggplot_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
# ggsave("../Downloads/IndexShare.png",
#        dpi = 300,
#        width = 10,  height = 5.6
#        )
```

``` r
(v2 <- df_hts %>% 
  ggplot(aes(x = period, y = share, color = fundingagency, group = fundingagency)) +
  geom_hline(aes(yintercept = 0)) +
  geom_line(size = 1) +
  geom_point( size = 4) +
  geom_blank(aes(y = share * 1.4)) +
  geom_text(aes(label = lab), family = "Gill Sans MT",
            vjust = -1, na.rm = TRUE) +
  scale_y_continuous(labels = percent_format(1)) +
  scale_x_discrete(labels = c("FY17", "", "", "",
                              "FY18", "", "", "",
                              "FY19", "", "", "")) +
  scale_color_manual(values = c("#002F6C", "#0067B9", "gray60")) +
  facet_grid(fundingagency ~ .) +
  labs(x = NULL, y = NULL,
       title = "Share of Positive Index Tests FY17-FY19",
       caption = "FY19Q4 MSD") +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(family = "Gill Sans MT"),
        strip.text = element_text(face = "bold")))
```

![](GettingStartedWithggplot_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
(v_combo <- grid.arrange(v1, v2, nrow = 1))
```

![](GettingStartedWithggplot_files/figure-markdown_github/unnamed-chunk-5-1.png)

    ## TableGrob (1 x 2) "arrange": 2 grobs
    ##   z     cells    name           grob
    ## 1 1 (1-1,1-1) arrange gtable[layout]
    ## 2 2 (1-1,2-2) arrange gtable[layout]

``` r
# ggsave("../Downloads/IndexCombo.png", plot = v_combo,
#        dpi = 300,
#        width = 10,  height = 5.6)
```
