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
source("scripts/01_clean_vandy.R")
df_sample <- read_csv("data/final_data.csv")

## The full data set 
df_full <-
  df_vandy %>% 
  select(broadcast_abstract, broadcast_duration, year, program_title, length, time, special) %>% 
  mutate(channel       = program_title,
         channel       = str_replace_all(channel, "Special|Evening News", ""),
         channel       = str_squish(channel),
         channel       = str_trim(channel)) %>% 
  filter(special == FALSE) 

## Yearly observation count
table(df_full$year)

## Total news broadcast count
df_full %>% 
  select(year,channel) %>% 
  filter(channel %in% c("ABC", "CBS", "NBC")) %>% 
  mutate(year = as.numeric(year)) %>% 
  group_by(year) %>% 
  add_count(year) %>% 
  unique() %>% 
  rename(abstracts = n) %>% 
  dplyr::select(-channel) %>% 
  unique() %>% 
  arrange(year) %>%
  ggplot(aes(x=year, y=abstracts)) + geom_line() +
  scale_y_continuous(breaks= pretty_breaks(n = 10)) +
  scale_x_continuous(breaks= pretty_breaks(n = 10)) +
  theme_minimal(base_size = 20) +
  scale_color_manual(values = wes_palette("IsleofDogs1")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y= "Number of News Broadcasts", x = "Year")
ggsave(filename = file.path("figs","fig_shows_total.pdf"), width = 13.92, height = 9.58)                        

## Broadcast count by channel 
df_full %>% 
  mutate(year = as.numeric(year)) %>% 
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
table(df_sample$channel)

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

rm(df_sample, df_vandy, df_full)
