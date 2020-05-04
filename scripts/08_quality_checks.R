#####################################################################################
##
##    File Name:        08_quality_checks.R
##    Date:             2020-03-22
##    Author:           Daniel Weitzel
##    Email:            daniel.weitzel@utexas.edu
##    Purpose:          Check quality of the data
##    Date Used:        2020-05-03
##    Data Used:        
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

## Loading the data 
df_gold    <- read_csv("data/gold_standard/sample_questions_gold.csv")
df_f8_gold <- read_csv("data/coded/sample_coded.csv")
df_f8      <- read_csv("data/final_data.csv")


## Minimum number of gold standard questions coded by one coder 
df_f8_gold %>% 
  dplyr::select(`_worker_id`) %>% 
  group_by(`_worker_id`) %>% 
  add_count()%>% 
  unique %>% 
  ungroup %>% 
  filter(n == min(n))

## Average number correct on gold standard questions 
## Initial prep of the data, renaming, dropping of columns
df_f8_gold <- 
  df_f8_gold  %>% 
  filter(`_golden` == "TRUE") %>% 
  dplyr::select(id, `_unit_id`, `_id`, `_worker_id`,  year,
                does_the_news_deal_with_a_concern_or_event_that_is_politically_consequential_or_not,
                does_the_news_deal_with_a_local_national_or_international_concern_or_event) %>% 
  rename(news         = does_the_news_deal_with_a_concern_or_event_that_is_politically_consequential_or_not,
         geography    = does_the_news_deal_with_a_local_national_or_international_concern_or_event,
         sample_id    = id,
         f8_unit_id   = `_unit_id`, 
         f8_id        = `_id`, 
         f8_worker_id = `_worker_id`) %>% 
  group_by(sample_id) %>% 
  mutate(coder = "coder",
         coder_n = row_number(),
         news = str_replace_all(news, "_news", "")) %>% 
  unite(coder, coder, coder_n, remove = FALSE) %>% 
  dplyr::select(-coder_n)

df_gold <-
  df_gold %>% 
  select(sample_id, hard_news_gold, geography_gold)

df_f8_gold <-
  df_f8_gold %>% 
  left_join(df_gold, by = c("sample_id")) %>% 
  mutate(hard_correct = ifelse(hard_news_gold == "hard_news" & news == "hard", TRUE, FALSE),
         geo_correct  = ifelse(geography_gold == geography, TRUE, FALSE))

## Hard news correct
mean(df_f8_gold$hard_correct)
sd(df_f8_gold$hard_correct)

## Geography correct
mean(df_f8_gold$geo_correct)
sd(df_f8_gold$geo_correct)


## Number of majority codings for news (at least 2 coders agree)
df_f8_news <-
  df_f8 %>% 
  filter(special == "FALSE") %>% 
  filter(channel %in% c("ABC", "CBS", "NBC")) %>% 
  dplyr::select(sample_id, starts_with("news_coder_"), starts_with("news")) %>% 
  filter(!is.na(news_majority))

## Number of majority codings for geography (at least 2 coders agree)
df_f8_geo <-
  df_f8 %>% 
  filter(special == "FALSE") %>% 
  filter(channel %in% c("ABC", "CBS", "NBC")) %>% 
  dplyr::select(sample_id, starts_with("geography_coder_"), starts_with("geography")) %>% 
  filter(is.na(geography_majority))


## Coder quality checks 
df_f8_sample <-
  df_f8 %>% 
  select(sample_id:geography_majority) %>% 
  select(-f8_unit_id) %>% 
  rename(broadcasts_in_year = abstracts_in_year,
         broadcast_time     = time) %>% 
  select(sample_id, year, broadcast_abstract, abstract_length, broadcasts_in_year, broadcast_time)
# write_csv(df_f8_sample, "data/sample_questions.csv")



## Golden answers 
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
# write_csv(gold_answers, "data/sample_questions_gold.csv")

## Number of gold standard questions per worker 
golden_numbers <- 
  df_f8 %>% 
  dplyr::select(`_worker_id`, `_golden`) %>% 
  rename(worker_id = `_worker_id`,
         golden    = `_golden`) %>% 
  mutate(golden = ifelse(golden == TRUE, "golden", "notgolden")) %>% 
  group_by(worker_id, golden) %>% 
  add_count() %>% 
  unique %>% 
  pivot_wider(id_cols = worker_id, names_from = golden, values_from = n) %>% 
  drop_na() %>% 
  mutate(all = golden + notgolden,
         share_golden = golden/all)

summary(golden_numbers$share_golden)
sd(golden_numbers$share_golden)

# Share of golden answers is between 15% and 94%. The mean is 44.17% and the SD is 15.8
summary(golden_numbers$all)
sd(golden_numbers$all)
#The number of questions coded by a given individual is between 12 and 347. The mean is 56.55 statements with a standard deviation of 49

## Data quality check
### Checking the coder quality
df_geography <-
  df_f8 %>% 
  ungroup() %>% 
  dplyr::select(geography_coder_1, geography_coder_2, geography_coder_3) %>% 
  mutate_all(funs(ifelse(.=="not_clear", NA, .))) %>%
  mutate_all(funs(ifelse(.=="international", 1, .))) %>% 
  mutate_all(funs(ifelse(.=="national", 2, .))) %>% 
  mutate_all(funs(ifelse(.=="local", 3, .))) %>% 
  mutate(geography_coder_1 = as.numeric(geography_coder_1), 
         geography_coder_2 = as.numeric(geography_coder_2), 
         geography_coder_3 = as.numeric(geography_coder_3),
         same = ifelse(geography_coder_1 == geography_coder_2 & geography_coder_2 == geography_coder_3 | geography_coder_1 == geography_coder_2 | geography_coder_1 == geography_coder_3 | geography_coder_3 == geography_coder_2  , 1, 0))


# Absolute agreement between raters
#agree(as.matrix(df_geography))
#table(df_geography$same)
# Percentage agreement (Tolerance=0)
# Subjects = 5200 
# Raters = 3 
# %-agree = 50.1 

# Fleiss' kappa 
#kappam.fleiss(df_geography)
#kappam.fleiss(df_geography, exact=TRUE)
# value is 0.384 and hence in the "fair agreement" range (0.21 – 0.40)

### Examining data quality for news type coding
df_news <-
  df_f8 %>% 
  ungroup() %>% 
  dplyr::select(news_coder_1, news_coder_2, news_coder_3) %>% 
  mutate_all(funs(ifelse(.=="not_clear", NA, .)))

# Absolute agreement between raters
#agree(cbind(df_news))
#   %-agree = 80.3

# Fleiss' kappa 
#kappam.fleiss(df_news)
#kappam.fleiss(df_news, exact=TRUE)
# Kappa = 0.296 and hence in the "fair agreement" range (0.21 – 0.40)

#table(df_news$news_coder_1)
#table(df_news$news_coder_2)
#table(df_news$news_coder_3)

#matrix_news <- as.matrix(df_news)
#kripp.alpha(matrix_news, method=c("ordinal"))
#  alpha = 0.295 

rm(df_geography, df_news)

## Some overview
## Years
#table(df_f8$year)
## Hard vs soft news 
#table(df_f8$news, useNA = 'always')
#table(df_f8$news_majority, useNA = 'always')
## Geography 
#table(df_f8$geography, useNA = 'always')    
#table(df_f8$geography_majority, useNA = 'always')  


