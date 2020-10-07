#####################################################################################
##
##    File Name:        01_clean_vandy.R
##    Date:             2020-03-20
##    Author:           Daniel Weitzel
##    Purpose:          Clean Vanderbilt news data
##    Date Used:        2020-09-29
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
library("lubridate")
library("rio")

## Loading the data 
df_vandy <- import("data/full_vandy.csv.zip")

## IDs of 40 gold stadnard questions already coded and saved in separate file on Github
gold_ids <- c(643545, 386911, 144217, 368617, 459367, 152619, 62501, 344203, 163486, 378541, 
              42774, 424675, 572714, 342763, 300049, 293266, 332163, 367264, 450889, 114038, 
              52570, 368427, 302580, 386290, 112059, 289088, 371160, 55356, 392587, 22000, 
              448454, 639895, 192973, 586793, 162631, 506760, 42123, 620004, 544601, 28089)

t2r <- read_file("scripts/text_to_replace.txt")
t2d <- read_file("scripts/text_to_delete.txt")
t2da <- c("part of the Vanderbilt Television News Archive collection|The following file names represent|Note to sponsor members|For additional details contact the Archive|The following file names represent presidential campaign events recorded on this date, which can be ordered individually by calling")

## Clean the Vanderbilt data set from transcripts that we do not care about. 
## I first remove Notes and Annotations that do not matter for the coding by replacing the text with nothing. 
## Those are listed in the text_to_replace.txt
## The minimum length of a transcript has to be 50 characters and I also generate a series of identifier and meta variables.

df_vandy3a <- 
  df_vandy %>% 
  mutate_all(list(~na_if(.,""))) %>% 
  filter(!is.na(broadcast_abstract)) %>% 
  mutate(id = as.numeric(rownames(.)),
         broadcast_abstract = str_replace_all(broadcast_abstract, t2r, ""),
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
  filter(!str_detect(broadcast_abstract, t2d)) %>% 
  filter(!id %in% gold_ids) %>% 
  mutate(year = str_trim(year),
         time = as.numeric(time)) %>% 
  drop_na(time) %>% 
  add_count(year) %>%  
  rename(sample_id = id) %>% 
  dplyr::select(-c("broadcast_reporter(s)","broadcast_order"))



rm(gold_ids, t2r, t2d)
