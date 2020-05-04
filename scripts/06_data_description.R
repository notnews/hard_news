#####################################################################################
##
##    File Name:        data_description.R
##    Date:             2017-09-20
##    Author:           Daniel Weitzel
##    Email:            daniel.weitzel@utexas.edu
##    Webpage:          www.danweitzel.net
##    Purpose:          
##    Date Used:        
##    Data Used:        
##    Output File:      (none)
##    Data Output:      
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
library("texreg")
library("rio")
library("scales")
library("wesanderson")

## Loading the data 
df_full   <- import("data/full_vandy.csv.zip")
df_sample <- read_csv("data/final_data.csv")

## The full data set 
df_full <-
  df_full %>% 
  select(broadcast_abstract, broadcast_duration, date, program_title) %>% 
  mutate(channel       = program_title,
         channel       = str_replace_all(channel, "Special|Evening News", ""),
         channel       = str_squish(channel),
         channel       = str_trim(channel)) %>%
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
  mutate(year = str_trim(year),
         year = as.numeric(year),
         time = as.numeric(time),
         special = str_detect(program_title, "special|Special")) %>%
  select(-broadcast_duration) %>% 
  filter(special == FALSE) %>% 
  drop_na(time) %>% 
  add_count(year)

table(df_full$year)

## Total news broadcast count
df_full %>% 
  select(year,channel) %>% 
  filter(channel %in% c("ABC", "CBS", "NBC")) %>% 
  group_by(year) %>% 
  add_count(year) %>% 
  unique() %>% 
  rename(abstracts = n) %>% 
  ungroup %>% 
  add_count(channel) %>% 
  filter(n > 9) %>% 
  ggplot(aes(x= year, y =abstracts)) + geom_line() +
  scale_y_continuous(breaks= pretty_breaks(n = 10)) +
  scale_x_continuous(breaks= pretty_breaks(n = 10)) +
  theme_minimal(base_size = 20) +
  scale_color_manual(values = wes_palette("IsleofDogs1")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y= "Number of News Broadcasts", x = "Year")
ggsave(filename = file.path("figs","fig_shows_total.pdf"), width = 13.92, height = 9.58)                        

## Broadcast count by channel 
df_full %>% 
  select(year, channel) %>% 
  group_by(year, channel) %>% 
  add_count(year, channel) %>% 
  unique() %>% 
  ungroup() %>% 
  rename(abstracts = n) %>% 
  filter(abstracts > 50) %>% 
  filter(channel != "FNC") %>% 
  filter(channel != "CNN") %>% 
  ggplot(aes(x= year, y = abstracts, color = channel)) + 
  geom_line(aes(color = channel), size = 1) +
  scale_y_continuous(breaks= pretty_breaks(n = 10))+
  scale_x_continuous(breaks= pretty_breaks(n = 10)) +
  theme_minimal(base_size = 20) +
  scale_color_manual(values = wes_palette("IsleofDogs1")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y= "Number of News Broadcasts", x = "Year", color = "Channel")
ggsave(filename = file.path("figs","fig_channel_total.pdf"), width = 13.92, height = 9.58)                        


## Describe the sample with tables and figures

table(df_sample$year)
table(df_sample2$channel)

df_sample <- 
  df_sample %>% 
  filter(channel %in% c("ABC", "CBS", "NBC")) 


df_sample %>% 
  select(year, channel) %>% 
  group_by(year, channel) %>% 
  add_count(year, channel) %>% 
  unique() %>% 
  ungroup() %>% 
  rename(abstracts = n) %>% 
  ggplot(aes(x= year, y = abstracts, color = channel)) + 
  geom_line(aes(color = channel), size = 1) +
  scale_y_continuous(breaks= pretty_breaks(n = 10))+
  scale_x_continuous(breaks= pretty_breaks(n = 10)) +
  theme_minimal(base_size = 20) +
  scale_color_manual(values = wes_palette("IsleofDogs1")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y= "Number of News Broadcasts", x = "Year", color = "Channel")
ggsave(filename = file.path("figs","fig_sample_channel_total.pdf"), width = 13.92, height = 9.58)                        

