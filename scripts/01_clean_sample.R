#####################################################################################
##
##    File Name:        01_vandy_clean_sample.R
##    Date:             2020-03-20
##    Author:           Daniel Weitzel
##    Purpose:          Clean and sample 5000 stories from Vandy news
##    Date Used:        2020-03-20
##    Data Used:        full_vandy.csv.zip
##    Output File:      (none)
##    Data Output:      sample_vandy.csv
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
library("lubridate")
library("rio")

## Loading the data 
df_vandy <- import("data/full_vandy.csv.zip")

## IDs of 40 gold stadnard questions already coded and saved in separate file on Github
gold_ids <- c(643545, 386911, 144217, 368617, 459367, 152619, 62501, 344203, 163486, 378541, 
              42774, 424675, 572714, 342763, 300049, 293266, 332163, 367264, 450889, 114038, 
              52570, 368427, 302580, 386290, 112059, 289088, 371160, 55356, 392587, 22000, 
              448454, 639895, 192973, 586793, 162631, 506760, 42123, 620004, 544601, 28089)

## Cleaning the data set to prepare for sampling 
## This samples for 1968 to 1977 first. Here we had the specials included. The code oversampled specials due to the longer 
## run time compared to normal news segments. From 1978 on we exclude specials in the sampling. This is done further down.
df_vandy_6877 <- 
  df_vandy %>% 
  select(broadcast_abstract, broadcast_duration, date) %>% 
  #slice(1:500) %>% 
  mutate_all(list(~na_if(.,""))) %>% 
  filter(!is.na(broadcast_abstract)) %>% 
  mutate(id = as.numeric(rownames(.)),
         broadcast_abstract = str_replace_all(broadcast_abstract, "\\(Studio\\)", ""),
         broadcast_abstract = str_replace_all(broadcast_abstract, "\\(video distortion throughout this tape\\)", ""),
         broadcast_abstract = str_replace_all(broadcast_abstract, "\\(video dropout throughout this tape\\)", ""),
         broadcast_abstract = str_replace_all(broadcast_abstract, "\\[VIDEO QUALITY OF PROGRAM POOR.\\]", ""),
         broadcast_abstract = str_replace_all(broadcast_abstract, "NOTE:   TIME NUMERALS REPRESENT SECONDS INTO BROADCAST, NOT CLOCK TIME, SINCE TELECAST ORIGINATED IN WASHINGTON, DC.	NOTE:   Video problems in some of the stories.", ""),
         broadcast_abstract = str_replace_all(broadcast_abstract, "NOTE (NO GOOD NIGHT)", ""),
         broadcast_abstract = str_replace_all(broadcast_abstract, 'Note: \\"\\"local headlines\\"\\" on bottom of screen almost all of the hour.', ""),
         broadcast_abstract = str_replace_all(broadcast_abstract, "NOTE: DUE TO TECHNICAL DIFFICULTIES, THIS BROADCAST AVAILABLE IN AUDIO ONLY", ""),
         broadcast_abstract = str_replace_all(broadcast_abstract, "NOTE - Local weather bulletin on screen from 5:30:10 to 5:50:00.", ""),
         broadcast_abstract = str_replace_all(broadcast_abstract, "Note - technical difficulties from 5:51:40 to 5:52:50.", ""),
         broadcast_abstract = str_replace_all(broadcast_abstract, "Note: local coverage interruption 6:02-6:10; 6:47-50", ""),
         broadcast_abstract = str_replace_all(broadcast_abstract, 'NOTE: \\"\\"WorldView\\"\\" abbreviated due to coverage of the Latrell Sprewell press conference', ""),
         broadcast_abstract = str_replace_all(broadcast_abstract, 'NOTE: \\"\\"Worldview\\"\\" abbreviated to present a special \\"\\"Our Digital Future\\"\\" at 5:30', ""),
         broadcast_abstract = str_replace_all(broadcast_abstract, "NOTE: Broadcast ends at 5:49:30 due to weather-related transmission problems.", ""),
         broadcast_abstract = str_replace_all(broadcast_abstract, "NOTE: Local weather bulletin on screen at various points in the broadcast.", ""),
         broadcast_abstract = str_replace_all(broadcast_abstract, "NOTE: Time numerals represent time into broadcast, not central time, since the telecast originated in Washington, DC. also, poor video quality throughout.", ""),
         broadcast_abstract = str_replace_all(broadcast_abstract, "NOTE: Time numerals represent time into broadcast, not central time, since the telecast originated in Washington, DC", ""),
         broadcast_abstract = str_replace_all(broadcast_abstract, "Report introduced", ""), 
         broadcast_abstract = str_replace_all(broadcast_abstract, "NOTE: Local weather bulletin on screen in parts of broadcast.", ""),
         broadcast_abstract = str_replace_all(broadcast_abstract, "(NOTE: AUDIO DISTORTED)", ""),
         broadcast_abstract = str_replace_all(broadcast_abstract, "(NOTE: DISTORTED AUDIO)", ""),
         broadcast_abstract = str_replace_all(broadcast_abstract, "NOTE: This broadcast is NOT available for loan from the Vanderbilt Television News Archive. For information about access to this broadcast, please contact us directly.", ""),
         broadcast_abstract = str_replace_all(broadcast_abstract, "This record is incomplete.", ""),
         broadcast_abstract = str_replace_all(broadcast_abstract, "CNN coverage of this event is a part of the Vanderbilt Television News Archive collection, but a complete catalog record of this broadcast is not yet available.", ""),
         broadcast_abstract = str_replace_all(broadcast_abstract, "is a part of the Vanderbilt Television News Archive collection, but a complete catalog record of this broadcast is not yet available.Preliminary Record.", ""),
         broadcast_abstract = str_replace_all(broadcast_abstract, "(NOTE: unstable time/date line in ABC version)", ""),
         broadcast_abstract = str_replace_all(broadcast_abstract, "NOTE: This catalog record is incomplete.", ""),
         broadcast_abstract = str_trim(broadcast_abstract),
         broadcast_abstract = str_squish(broadcast_abstract),
         time = as.duration(hms(broadcast_duration)),
         length = nchar(broadcast_abstract)) %>%
  separate(time, sep = "s", into = c("time", "drop")) %>% 
  separate(date, sep = ",", into = c("date", "day", "year")) %>% 
  select(-c(drop, day, date)) %>% 
  filter(nchar(broadcast_abstract) > 50) %>% 
  filter(time > 0) %>% 
  filter(!str_detect(broadcast_abstract, "part of the Vanderbilt Television News Archive collection|The following file names represent|Note to sponsor members|For additional details contact the Archive|The following file names represent presidential campaign events recorded on this date, which can be ordered individually by calling")) %>% 
  filter(!id %in% gold_ids) %>% 
  mutate(year = str_trim(year),
         time = as.numeric(time)) %>% 
  select(-broadcast_duration) %>% 
  drop_na(time) %>% 
  add_count(year) %>% 
  group_by(year) %>% 
  # for the sampling 5000/52 is 96.15
  sample_n(100, weight = time, replace = FALSE) %>%
  ungroup() %>% 
  filter(year < 1978)


## Check if gold standard questions are dupliacted
used_ids <- df_vandy_6877$id
gold_ids %in% used_ids

## Check if randomization by year worked
table(df_vandy_6877$year)


## Sample for 1978 to 2019 without including any specials 
df_vandy_7819 <- 
  df_vandy %>% 
  select(broadcast_abstract, broadcast_duration, program_title, date) %>% 
  #slice(1:500) %>% 
  mutate_all(list(~na_if(.,""))) %>% 
  filter(!is.na(broadcast_abstract)) %>% 
  mutate(id = as.numeric(rownames(.)),
         broadcast_abstract = str_replace_all(broadcast_abstract, "\\(Studio\\)", ""),
         broadcast_abstract = str_replace_all(broadcast_abstract, "\\(video distortion throughout this tape\\)", ""),
         broadcast_abstract = str_replace_all(broadcast_abstract, "\\(video dropout throughout this tape\\)", ""),
         broadcast_abstract = str_replace_all(broadcast_abstract, "\\[VIDEO QUALITY OF PROGRAM POOR.\\]", ""),
         broadcast_abstract = str_replace_all(broadcast_abstract, "NOTE:   TIME NUMERALS REPRESENT SECONDS INTO BROADCAST, NOT CLOCK TIME, SINCE TELECAST ORIGINATED IN WASHINGTON, DC.	NOTE:   Video problems in some of the stories.", ""),
         broadcast_abstract = str_replace_all(broadcast_abstract, "NOTE (NO GOOD NIGHT)", ""),
         broadcast_abstract = str_replace_all(broadcast_abstract, 'Note: \\"\\"local headlines\\"\\" on bottom of screen almost all of the hour.', ""),
         broadcast_abstract = str_replace_all(broadcast_abstract, "NOTE: DUE TO TECHNICAL DIFFICULTIES, THIS BROADCAST AVAILABLE IN AUDIO ONLY", ""),
         broadcast_abstract = str_replace_all(broadcast_abstract, "NOTE - Local weather bulletin on screen from 5:30:10 to 5:50:00.", ""),
         broadcast_abstract = str_replace_all(broadcast_abstract, "Note - technical difficulties from 5:51:40 to 5:52:50.", ""),
         broadcast_abstract = str_replace_all(broadcast_abstract, "Note: local coverage interruption 6:02-6:10; 6:47-50", ""),
         broadcast_abstract = str_replace_all(broadcast_abstract, 'NOTE: \\"\\"WorldView\\"\\" abbreviated due to coverage of the Latrell Sprewell press conference', ""),
         broadcast_abstract = str_replace_all(broadcast_abstract, 'NOTE: \\"\\"Worldview\\"\\" abbreviated to present a special \\"\\"Our Digital Future\\"\\" at 5:30', ""),
         broadcast_abstract = str_replace_all(broadcast_abstract, "NOTE: Broadcast ends at 5:49:30 due to weather-related transmission problems.", ""),
         broadcast_abstract = str_replace_all(broadcast_abstract, "NOTE: Local weather bulletin on screen at various points in the broadcast.", ""),
         broadcast_abstract = str_replace_all(broadcast_abstract, "NOTE: Time numerals represent time into broadcast, not central time, since the telecast originated in Washington, DC. also, poor video quality throughout.", ""),
         broadcast_abstract = str_replace_all(broadcast_abstract, "NOTE: Time numerals represent time into broadcast, not central time, since the telecast originated in Washington, DC", ""),
         broadcast_abstract = str_replace_all(broadcast_abstract, "Report introduced", ""), 
         broadcast_abstract = str_replace_all(broadcast_abstract, "NOTE: Local weather bulletin on screen in parts of broadcast.", ""),
         broadcast_abstract = str_replace_all(broadcast_abstract, "(NOTE: AUDIO DISTORTED)", ""),
         broadcast_abstract = str_replace_all(broadcast_abstract, "(NOTE: DISTORTED AUDIO)", ""),
         broadcast_abstract = str_replace_all(broadcast_abstract, "NOTE: This broadcast is NOT available for loan from the Vanderbilt Television News Archive. For information about access to this broadcast, please contact us directly.", ""),
         broadcast_abstract = str_replace_all(broadcast_abstract, "This record is incomplete.", ""),
         broadcast_abstract = str_replace_all(broadcast_abstract, "CNN coverage of this event is a part of the Vanderbilt Television News Archive collection, but a complete catalog record of this broadcast is not yet available.", ""),
         broadcast_abstract = str_replace_all(broadcast_abstract, "is a part of the Vanderbilt Television News Archive collection, but a complete catalog record of this broadcast is not yet available.Preliminary Record.", ""),
         broadcast_abstract = str_replace_all(broadcast_abstract, "(NOTE: unstable time/date line in ABC version)", ""),
         broadcast_abstract = str_replace_all(broadcast_abstract, "NOTE: This catalog record is incomplete.", ""),
         broadcast_abstract = str_trim(broadcast_abstract),
         broadcast_abstract = str_squish(broadcast_abstract),
         time               = as.duration(hms(broadcast_duration)),
         length             = nchar(broadcast_abstract),
         special            = str_detect(program_title, "special|Special")) %>%
  separate(time, sep = "s", into = c("time", "drop")) %>% 
  separate(date, sep = ",", into = c("date", "day", "year")) %>% 
  select(-c(drop, day, date)) %>% 
  filter(nchar(broadcast_abstract) > 50) %>% 
  filter(time > 0) %>% 
  filter(!str_detect(broadcast_abstract, "part of the Vanderbilt Television News Archive collection|The following file names represent|Note to sponsor members|For additional details contact the Archive|The following file names represent presidential campaign events recorded on this date, which can be ordered individually by calling")) %>% 
  filter(!id %in% gold_ids) %>% 
  mutate(year = str_trim(year),
         time = as.numeric(time)) %>% 
  select(-broadcast_duration) %>% 
  drop_na(time) %>% 
  filter(year > 1977) %>% 
  filter(special == FALSE) %>% 
  add_count(year) %>% 
  group_by(year) %>% 
  # for the sampling 5000/52 is 96.15
  sample_n(100, weight = time, replace = FALSE) %>%
  ungroup() %>% 
  select(-c(special, program_title))

## Check if gold standard questions are dupliacted
used_ids <- df_vandy_7819$id
gold_ids %in% used_ids

## Check if randomization by year worked
table(df_vandy_7819$year)

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

write_csv(df_vandy_6877, "data/subsets_10years/sample_1_questions_6877.csv")
write_csv(df_vandy_7887, "data/subsets_10years/sample_2_questions_7887.csv")
write_csv(df_vandy_8897, "data/subsets_10years/sample_3_questions_8897.csv")
write_csv(df_vandy_9807, "data/subsets_10years/sample_4_questions_9807.csv")
write_csv(df_vandy_0817, "data/subsets_10years/sample_5_questions_0817.csv")
write_csv(df_vandy_1819, "data/subsets_10years/sample_6_questions_1819.csv")


rm(gold_ids, used_ids, df_vandy, df_vandy_6877, df_vandy_7819,
   df_vandy_7887, df_vandy_8897, df_vandy_9807, df_vandy_0817, 
   df_vandy_1819)
