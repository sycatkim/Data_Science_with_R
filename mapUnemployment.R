rm(list = ls())
# install.packages("blscrapeR")
library(blscrapeR)
library(dplyr)
library(choroplethr)  

x = get_bls_county() %>% as.tbl
str(x)
x

## blscraeR does a lot of clean up of this file:  https://www.bls.gov/web/metro/laucntycur14.txt


# pipe it! 
x %>% 
  mutate(value = as.numeric(unemployed_rate), region = as.numeric(fips)) %>% 
  select(region, value) %>%
  county_choropleth
