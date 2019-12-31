### Getting Started

Install/update the following packages that we'll be using:

```{r}
library(devtools)
install_github("ICPI/ICPIutilities")
install.packages("tidyverse")
install.packages("scales")
install.packages("extrafont")
install.packages("gridExtra")
install.packages("patchwork")
```

Download the masked MSD dataset. You'll need to 

```{r}
library(readr)
library(magrittr)

#specify the folder in which you want to save the masked MSD file
folderpath_output <- ""

#run the below lines of code to save the masked MSD
dataset_url <- "https://media.githubusercontent.com/media/ICPI/TrainingDataset/master/Output/MER_Structured_TRAINING_Datasets_PSNU_IM_FY17-20_20191115_v1_1.txt"
filename <- basename(dataset_url) 

read_tsv(dataset_url, col_types = c(.default = "c")) %>%
  write_tsv(file.path(folderpath_output, filename), na = "")
  
```
