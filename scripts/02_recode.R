#####################################################################################
##
##    File Name:        02_recode_quality.R
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
library("tidycomm")
library("lubridate")
library("irr")
library("DescTools")

## Loading the data 
df_vandy <- import("data/full_vandy.csv.zip")
df_f8    <- read_csv("data/coded/sample_coded.csv")

## key dates
presidential_election <- seq(1968, 2016, 4)
midterm_election      <- seq(1970, 2018, 4)

## Golden answers 
# gold_answers <- 
#   df_f8 %>% 
#    filter(`_golden` == "TRUE") %>% 
#    select(broadcast_abstract, does_the_news_deal_with_a_concern_or_event_that_is_politically_consequential_or_not_gold, 
#            does_the_news_deal_with_a_local_national_or_international_concern_or_event_gold, id, length, n, time, year) %>% 
#    rename(hard_news_gold = does_the_news_deal_with_a_concern_or_event_that_is_politically_consequential_or_not_gold,
#           geography_gold = does_the_news_deal_with_a_local_national_or_international_concern_or_event_gold) %>% 
#    rename(sample_id = id) %>% 
#    group_by(sample_id) %>% 
#    unique()
# write_csv(gold_answers, "data/sample_questions_gold.csv")
#
## Number of gold standard questions per worker 
# golden_numbers <- 
#   df_f8 %>% 
#   dplyr::select(`_worker_id`, `_golden`) %>% 
#   rename(worker_id = `_worker_id`,
#          golden    = `_golden`) %>% 
#   mutate(golden = ifelse(golden == TRUE, "golden", "notgolden")) %>% 
#   group_by(worker_id, golden) %>% 
#   add_count() %>% 
#   unique %>% 
#   pivot_wider(id_cols = worker_id, names_from = golden, values_from = n) %>% 
#   drop_na() %>% 
#   mutate(all = golden + notgolden,
#          share_golden = golden/all)
# 
# summary(golden_numbers$share_golden)
# sd(golden_numbers$share_golden)
# Share of golden answers is between 15% and 94%. The mean is 44.17% and the SD is 15.8
# 
# summary(golden_numbers$all)
# sd(golden_numbers$all)
# The number of questions coded by a given individual is between 12 and 347. The mean is 56.55 statements with a standard deviation of 49

## Initial prep of the data, renaming, dropping of columns, generating identifiers
df_f8 <- 
  df_f8 %>% 
  filter(`_golden` == "FALSE") %>% 
  dplyr::select(id, `_unit_id`, `_id`, `_worker_id`, broadcast_abstract,  year,
         does_the_news_deal_with_a_concern_or_event_that_is_politically_consequential_or_not,
         does_the_news_deal_with_a_local_national_or_international_concern_or_event,
         length, n, time) %>% 
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

## Reshaping and recoding of the data set 
df_f8 <- 
  df_f8 %>% 
  dplyr::select(sample_id, f8_unit_id, f8_worker_id, year, broadcast_abstract, 
                coder, geography, news, length, n, time) %>% 
  rename(id = f8_worker_id,
         abstracts_in_year = n,
         abstract_length = length) %>% 
  pivot_wider(id_cols = c(sample_id, f8_unit_id, year, broadcast_abstract, abstract_length, abstracts_in_year, time), 
              values_from = c(id, geography, news), 
              names_from = coder) %>% 
  mutate(news = ifelse(news_coder_1 == "hard" & news_coder_2 == "hard" & news_coder_3 == "hard", "hard", 
                        ifelse(news_coder_1 == "soft" & news_coder_2 == "soft" & news_coder_3 == "soft", "soft",  NA)),
         news_majority = pmap_chr(list(news_coder_1, news_coder_2, news_coder_3), ~ Mode(c(...))),
         geography = ifelse(geography_coder_1 == "international" & geography_coder_2 == "international" & geography_coder_3 == "international", "international", 
                            ifelse(geography_coder_1 == "national" & geography_coder_2 == "national" & geography_coder_3 == "national", "national", 
                                   ifelse(geography_coder_1 == "local" & geography_coder_2 == "local" & geography_coder_3 == "local", "local", 
                                          ifelse(geography_coder_1 == "not_clear" & geography_coder_2 == "not_clear" & geography_coder_3 == "not_clear", "not_clear", NA)))),
         geography_majority = pmap_chr(list(geography_coder_1, geography_coder_2, geography_coder_3), ~ Mode(c(...))))

## Adding in some pattern recog.
df_f8 <- 
  df_f8 %>% 
  mutate(president  = as.numeric(str_detect(broadcast_abstract, "president|President|White House|Nixon|Agnew|Ford|Rockefeller|Carter|Mondale|Reagan|Bush|Quayle|Clinton|Gore|Cheney|Obama|Biden|Trump|Pence")),
         president_name  = as.numeric(str_detect(broadcast_abstract, "Nixon|Ford|Carter|Reagan|Bush|Clinton|Obama|Trump")),
         p_nixon    = as.numeric(str_detect(broadcast_abstract, "Nixon")),
         vp_agnew    = as.numeric(str_detect(broadcast_abstract, "Agnew")), 
         p_ford     = as.numeric(str_detect(broadcast_abstract, "Ford")),
         vp_rockef   = as.numeric(str_detect(broadcast_abstract, "Rockefeller")),
         p_carter   = as.numeric(str_detect(broadcast_abstract, "Carter")),
         vp_mondale  = as.numeric(str_detect(broadcast_abstract, "Mondale")),
         p_reagan   = as.numeric(str_detect(broadcast_abstract, "Reagan")),
         p_bush     = as.numeric(str_detect(broadcast_abstract, "Bush")),
         vp_quayle   = as.numeric(str_detect(broadcast_abstract, "Quayle")),
         p_clinton  = as.numeric(str_detect(broadcast_abstract, "Clinton")),
         vp_gore     = as.numeric(str_detect(broadcast_abstract, "Gore")),
         vp_cheney   = as.numeric(str_detect(broadcast_abstract, "Cheney")),
         p_obama    = as.numeric(str_detect(broadcast_abstract, "Obama")),
         vp_biden    = as.numeric(str_detect(broadcast_abstract, "Biden")),
         p_trump    = as.numeric(str_detect(broadcast_abstract, "Trump|DJT")),
         war        = as.numeric(str_detect(broadcast_abstract, "war|War|Vietnam|Viet Nam|Afghanistan|Iraq|Somalia|Balkans|Kosovo|Yugoslavia|Serbia|Bosnia|Syria")),
         economy    = as.numeric(str_detect(broadcast_abstract, "economy|Economy|unemployment|Unemployment|inflation|Inflation|GDP|wages|Wages|tax|Tax|taxes|Taxes")),
         welfare    = as.numeric(str_detect(broadcast_abstract, "welfare|Welfare|Medicare|Medicaid|Health Insurance|Insurance|insurance|health|Health|Obamacare|Affordable Care Act")),
         election   = as.numeric(str_detect(broadcast_abstract, "election|Election")),
         election_date = ifelse(year %in% presidential_election, "presidential",
                                ifelse(year %in% midterm_election, "midterm", NA)))
         
         

# df_f8_sample <-
#   df_f8 %>% 
#   select(sample_id:geography_majority) %>% 
#   select(-f8_unit_id) %>% 
#   rename(broadcasts_in_year = abstracts_in_year,
#          broadcast_time     = time) %>% 
#   select(sample_id, year, broadcast_abstract, abstract_length, broadcasts_in_year, broadcast_time)
# 
# write_csv(df_f8_sample, "data/sample_questions.csv")

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

# Krippendorff's alpha for ordinal ratings
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



## Adding in channel information from original data 
sampled_ids <- df_f8$sample_id

df_vandy <- 
  df_vandy %>% 
  select(broadcast_abstract, broadcast_duration, broadcast_time, date, program_title) %>% 
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

#table(df_vandy$program_title)
#table(df_vandy$special)
#table(df_vandy$channel)

df_f8 <-
  df_f8 %>% 
  left_join(df_vandy, by = "sample_id") %>% 
  select(-f8_unit_id) %>% 
  rename(broadcasts_in_year = abstracts_in_year,
         broadcast_time     = time) %>% 
  filter(special==FALSE)
  

#write_csv(df_f8, "data/final_data.csv")

rm(df_vandy, sampled_ids, matrix_geography, matrix_news, presidential_election, midterm_election)

