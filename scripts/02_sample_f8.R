#####################################################################################
##
##    File Name:        01_vandy_clean_sample.R
##    Date:             2020-03-20
##    Author:           Daniel Weitzel
##    Purpose:          Clean and sample 5000 stories from Vanderbilt news
##    Date Used:        2020-06-07
##    Data Used:        full_vandy.csv.zip
##    Output File:      (none)
##    Data Output:      Sample data sets for F8 coding
##    Data Webpage:     (none)
##    Log File:         (none)
##    Notes:            
##
#####################################################################################

## Setting working directory
setwd(githubdir)
setwd("notwork_news/")

## Libraries 
library("tidyverse")

## Loading the data 
source("scripts/01_clean_vandy.R")

df_vandy <-
  df_vandy %>% 
  dplyr::select(broadcast_abstract, year, sample_id, time, length, special, n)

## This samples for 1968 to 1977 first. Here we had the specials included. The code oversampled specials due to the longer 
## run time compared to normal news segments. From 1978 on we exclude specials in the sampling. This is done further down.

df_vandy_6877 <- 
  df_vandy %>% 
  dplyr::select(-special) %>% 
  group_by(year) %>% 
  sample_n(100, weight = time, replace = FALSE) %>%
  ungroup() %>% 
  filter(year < 1978)

## Sample for 1978 to 2019 without including any specials 
df_vandy_7819 <- 
  df_vandy %>% 
  filter(year > 1977) %>% 
  filter(special == FALSE) %>% 
  group_by(year) %>% 
  sample_n(100, weight = time, replace = FALSE) %>%
  ungroup() %>% 
  select(-special)

## Smaller subsets of the 1978-2019 data set
df_vandy_7887 <-
  df_vandy_7819 %>% 
  filter(year > 1977 & year < 1988)

df_vandy_8897 <-
  df_vandy_7819 %>% 
  filter(year > 1987 & year < 1998)

df_vandy_9807 <-
  df_vandy_7819 %>% 
  filter(year > 1997 & year < 2008)

df_vandy_0817 <-
  df_vandy_7819 %>% 
  filter(year > 2007 & year < 2018)

df_vandy_1819 <-
  df_vandy_7819 %>% 
  filter(year > 2017)


## Export the data set
#write_csv(df_vandy_7819, "data/sample_questions_7819.csv")

#write_csv(df_vandy_6877, "data/subsets_10years/sample_1_questions_6877.csv")
#write_csv(df_vandy_7887, "data/subsets_10years/sample_2_questions_7887.csv")
#write_csv(df_vandy_8897, "data/subsets_10years/sample_3_questions_8897.csv")
#write_csv(df_vandy_9807, "data/subsets_10years/sample_4_questions_9807.csv")
#write_csv(df_vandy_0817, "data/subsets_10years/sample_5_questions_0817.csv")
#write_csv(df_vandy_1819, "data/subsets_10years/sample_6_questions_1819.csv")

rm(df_vandy, df_vandy_6877, df_vandy_7819, df_vandy_7887, 
   df_vandy_8897, df_vandy_9807, df_vandy_0817, df_vandy_1819)
