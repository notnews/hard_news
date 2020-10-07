#####################################################################################
##
##    File Name:        02_recode.R
##    Date:             2020-03-22
##    Author:           Daniel Weitzel
##    Email:            daniel.weitzel@utexas.edu
##    Purpose:          Clean and recode the Figure Eight coded data
##    Date Used:        2020-03-22
##    Data Used:        sample_coded_1000.zip
##    Output File:      (none)
##    Data Output:      (none)
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
library("rio")
library("lubridate")
library("DescTools")

## Loading the data 
source("scripts/01_clean_vandy.R")
df_f8 <- read_csv("data/coded/sample_coded.csv")

## Key election dates
presidential_election <- seq(1968, 2016, 4)
midterm_election      <- seq(1970, 2018, 4)

## Golden answers - First step of the cleaning process of the F8 coded data is saving the gold standard questions
## in a separate CSV. 
gold_answers <- 
  df_f8 %>% 
  filter(`_golden` == "TRUE") %>% 
  select(broadcast_abstract, does_the_news_deal_with_a_concern_or_event_that_is_politically_consequential_or_not_gold, 
         does_the_news_deal_with_a_local_national_or_international_concern_or_event_gold, id, length, n, time, year) %>% 
  rename(hard_news_gold = does_the_news_deal_with_a_concern_or_event_that_is_politically_consequential_or_not_gold,
         geography_gold = does_the_news_deal_with_a_local_national_or_international_concern_or_event_gold) %>% 
  rename(sample_id = id) %>% 
  group_by(sample_id) %>% 
  unique()

write_csv(gold_answers, "data/sample_questions_gold.csv")

## Cleaning the F8 coded data
## Initial prep of the data, renaming, dropping of columns, generating identifiers
df_f8a <- 
  df_f8 %>% 
  filter(`_golden` == "FALSE") %>% 
  dplyr::select(id, `_unit_id`, `_id`, `_worker_id`, broadcast_abstract,  year,
         does_the_news_deal_with_a_concern_or_event_that_is_politically_consequential_or_not,
         does_the_news_deal_with_a_local_national_or_international_concern_or_event,
         length, n, time, `_golden`) %>% 
  rename(news         = does_the_news_deal_with_a_concern_or_event_that_is_politically_consequential_or_not,
         geography    = does_the_news_deal_with_a_local_national_or_international_concern_or_event,
         sample_id    = id,
         f8_unit_id   = `_unit_id`, 
         f8_id        = `_id`, 
         f8_worker_id = `_worker_id`,
         golden       = `_golden`) %>% 
  group_by(sample_id) %>% 
  mutate(coder = "coder",
         coder_n = row_number(),
         news = str_replace_all(news, "_news", "")) %>% 
  unite(coder, coder, coder_n, remove = FALSE) %>% 
  dplyr::select(-coder_n) %>% 
  dplyr::select(sample_id, f8_unit_id, f8_worker_id, year, broadcast_abstract, 
                coder, geography, news, length, n, time) %>% 
  rename(id = f8_worker_id,
         abstracts_in_year = n,
         abstract_length = length) %>% 
  pivot_wider(id_cols = c(sample_id, f8_unit_id, year, broadcast_abstract, abstract_length, abstracts_in_year, time), 
              values_from = c(id, geography, news), 
              names_from = coder) %>% 
  mutate(news = case_when(news_coder_1 == news_coder_2 & news_coder_2 == news_coder_3 ~ news_coder_1,
                           TRUE ~ NA_character_),
         news_majority = pmap_chr(list(news_coder_1, news_coder_2, news_coder_3), ~ Mode(c(...))),
         geography = case_when(geography_coder_1 == geography_coder_2 & geography_coder_2 == geography_coder_3 ~ geography_coder_1,
                               TRUE ~ NA_character_),
         geography_majority = pmap_chr(list(geography_coder_1, geography_coder_2, geography_coder_3), ~ Mode(c(...))),
         president      = as.numeric(str_detect(broadcast_abstract, "president|President|White House|Nixon|Agnew|Ford|Rockefeller|Carter|Mondale|Reagan|Bush|Quayle|Clinton|Gore|Cheney|Obama|Biden|Trump|Pence")),
         president_name = as.numeric(str_detect(broadcast_abstract, "Nixon|Ford|Carter|Reagan|Bush|Clinton|Obama|Trump")),
         p_nixon        = as.numeric(str_detect(broadcast_abstract, "Nixon")),
         vp_agnew       = as.numeric(str_detect(broadcast_abstract, "Agnew")), 
         p_ford         = as.numeric(str_detect(broadcast_abstract, "Ford")),
         vp_rockef      = as.numeric(str_detect(broadcast_abstract, "Rockefeller")),
         p_carter       = as.numeric(str_detect(broadcast_abstract, "Carter")),
         vp_mondale     = as.numeric(str_detect(broadcast_abstract, "Mondale")),
         p_reagan       = as.numeric(str_detect(broadcast_abstract, "Reagan")),
         p_bush         = as.numeric(str_detect(broadcast_abstract, "Bush")),
         vp_quayle      = as.numeric(str_detect(broadcast_abstract, "Quayle")),
         p_clinton      = as.numeric(str_detect(broadcast_abstract, "Clinton")),
         vp_gore        = as.numeric(str_detect(broadcast_abstract, "Gore")),
         vp_cheney      = as.numeric(str_detect(broadcast_abstract, "Cheney")),
         p_obama        = as.numeric(str_detect(broadcast_abstract, "Obama")),
         vp_biden       = as.numeric(str_detect(broadcast_abstract, "Biden")),
         p_trump        = as.numeric(str_detect(broadcast_abstract, "Trump|DJT")),
         war            = as.numeric(str_detect(broadcast_abstract, "war|War|Vietnam|Viet Nam|Afghanistan|Iraq|Somalia|Balkans|Kosovo|Yugoslavia|Serbia|Bosnia|Syria")),
         economy        = as.numeric(str_detect(broadcast_abstract, "economy|Economy|unemployment|Unemployment|inflation|Inflation|GDP|wages|Wages|tax|Tax|taxes|Taxes")),
         welfare        = as.numeric(str_detect(broadcast_abstract, "welfare|Welfare|Medicare|Medicaid|Health Insurance|Insurance|insurance|health|Health|Obamacare|Affordable Care Act")),
         election       = as.numeric(str_detect(broadcast_abstract, "election|Election")),
         election_date  = ifelse(year %in% presidential_election, "presidential",
                                 ifelse(year %in% midterm_election, "midterm", NA)))
  
## Combining the F8 coded data with channel and other meta information from the original Vanderbilt News data set

## Getting the IDs of the broadcasts we sampled from the original data
sampled_ids <- df_f8$sample_id

## Preparing the Vanderbilt data for merge with the coded F8 data by renaming and reducing the data set.
df_vandy <- 
  df_vandy %>% 
  select(broadcast_abstract, broadcast_time, broadcast_duration, year, program_title) %>% 
  mutate_all(list(~na_if(.,""))) %>% 
  filter(!is.na(broadcast_abstract)) %>% 
  mutate(sample_id     = as.numeric(rownames(.)),
         special       = str_detect(program_title, "special|Special"),
         evening_news  = str_detect(program_title, "Evening"),
         channel       = program_title,
         channel       = str_replace_all(channel, "Special|Evening News", ""),
         channel       = str_squish(channel),
         channel       = str_trim(channel)) %>% 
  filter(sample_id %in% sampled_ids) %>% 
  select(-c(broadcast_abstract, broadcast_duration)) %>% 
  separate(broadcast_time, into = c("start_broadcast_time", "end_broadcast_time"), sep = "-") %>% 
  mutate(start_broadcast_time = hms(start_broadcast_time),
         end_broadcast_time = hms(end_broadcast_time))

# Combining original raw data and coded F8 data to have codes and meta information in one data set
df_f8 <-
  df_f8 %>% 
  left_join(df_vandy, by = "sample_id") %>% 
  select(-f8_unit_id) %>% 
  rename(broadcasts_in_year = abstracts_in_year,
         broadcast_time     = time) #%>% 
  #filter(special==FALSE)
  
#write_csv(df_f8, "data/final_data.csv")
rm(df_vandy, sampled_ids, gold_answers, presidential_election, midterm_election)

