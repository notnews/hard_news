#####################################################################################
##
##    File Name:        03_soft.R
##    Date:             2020-03-22
##    Author:           Daniel Weitzel
##    Email:            daniel.weitzel@utexas.edu
##    Purpose:          Hard/soft news analysis of the F8 data
##    Date Used:        2020-03-22
##    Data Used:        sample_coded_1000.zip
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
library("scales")
library("wesanderson")
library("texreg")
library("ggrepel")

## Load the data
source("scripts/02_recode.R")

############################################################################################
## Majority Coding 
df_f8_soft <-
  df_f8 %>% 
  filter(channel %in% c("ABC", "CBS", "NBC")) %>% 
  group_by(year) %>% 
  add_count() %>% 
  rename(n_shows = n) %>% 
  group_by(year, news_majority) %>% 
  add_count() %>% 
  rename(n_news = n) %>% 
  mutate(prob_news = n_news/n_shows) %>% 
  select(year, news_majority, prob_news) %>% 
  filter(news_majority != "not_clear") %>% 
  filter(!is.na(news_majority)) %>% 
  group_by(news_majority) %>% 
  mutate(label = if_else(year == max(year), as.character(news_majority), NA_character_)) %>% 
  unique

## Table of soft news per year 

df_f8_soft %>% 
  filter(news_majority == "soft") %>% 
  unique() %>% 
  head(n=20)

df_f8_soft %>% 
  filter(news_majority == "soft") %>% 
  unique() %>% 
  tail(n=20)


## Plotting the proportion of hard and soft news over time 
df_f8_soft %>% 
  ggplot(aes(x = year, y = prob_news, group = news_majority, color = news_majority)) + 
  geom_line(aes(color = news_majority), size = 1) +
  scale_color_manual(values = wes_palette("IsleofDogs1")) +
#  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_x_continuous(breaks= pretty_breaks(n = 10)) +
  scale_y_continuous(breaks= pretty_breaks(n = 10)) +
  #geom_smooth(method = "lm", fill = NA, color = "darkgrey", size = 0.5) +
  theme_minimal(base_size = 20) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
  labs(y= "Proportion of News", x = "Year", color = "News Type") + 
  geom_text_repel(aes(label = label), nudge_x = 1, direction = "both", na.rm = TRUE, size = 7)
ggsave(filename = file.path("figs","fig_prob_news_all.pdf"), width = 13.92, height = 9.58)                        
ggsave(filename = file.path("figs","fig_prob_news_all.png"), width = 13.92, height = 9.58)                        

## Proportion of hard news by channel, generation of the data set for graphs and models
df_f8_soft_channel <-
  df_f8 %>% 
  filter(channel %in% c("ABC", "CBS", "NBC")) %>% 
  group_by(year, channel) %>% 
  add_count() %>% 
  rename(n_shows = n) %>% 
  group_by(year, channel, news_majority) %>% 
  add_count() %>% 
  rename(n_news = n) %>% 
  mutate(prob_news = n_news/n_shows) %>% 
  select(year, news_majority, channel, n_shows, n_news, prob_news) %>% 
  filter(news_majority != "not_clear") %>% 
  filter(!is.na(news_majority)) %>% 
  filter(news_majority == "soft") %>% 
  unique

## Graph 
df_f8_soft_channel  %>% 
  ggplot(aes(x = year, y = prob_news)) + 
  geom_line(aes(color = channel), size = 1) +
  scale_x_continuous(breaks= pretty_breaks()) + ylim(0,1) +
  theme_minimal(base_size = 20) + 
  scale_color_manual(values = wes_palette("IsleofDogs1")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y= "Proportion of Hard News", x = "Year", color = "Channel")
ggsave(filename = file.path("figs","fig_prob_soft_channel.pdf"), width = 13.92, height = 9.58)                        

############################################################################################
## Unanimous coding 
## Same as above but instead of majority coding we are using unanimous coding results 
df_f8_soft <-
  df_f8 %>% 
  filter(channel %in% c("ABC", "CBS", "NBC")) %>% 
  group_by(year) %>% 
  add_count() %>% 
  rename(n_shows = n) %>% 
  group_by(year, news) %>% 
  add_count() %>% 
  rename(n_news = n) %>% 
  mutate(prob_news = n_news/n_shows) %>% 
  select(year, news, prob_news) %>% 
  filter(news != "not_clear") %>% 
  filter(!is.na(news))  %>% 
  group_by(news) %>% 
  mutate(label = if_else(year == max(year), as.character(news), NA_character_)) %>% 
  unique

## Table of soft news per year 
df_f8_soft %>% 
  filter(news == "soft") %>% 
  unique() %>% 
  head(n=20)

df_f8_soft %>% 
  filter(news == "soft") %>% 
  unique() %>% 
  tail(n=20)

## Plotting the proportion of hard and soft news over time 
df_f8_soft %>% 
  ggplot(aes(x = year, y = prob_news, group = news, color=news)) + 
  geom_line(aes(color = news), size = 1) +
  scale_color_manual(values = wes_palette("IsleofDogs1")) +
  #  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_x_continuous(breaks= pretty_breaks(n = 10)) +
  scale_y_continuous(breaks= pretty_breaks(n = 10)) +
  #geom_smooth(method = "lm", fill = NA, color = "darkgrey", size = 0.5) +
  theme_minimal(base_size = 20) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
  labs(y= "Proportion of News", x = "Year", color = "News Type") + 
  geom_text_repel(aes(label = label), nudge_x = 1, direction = "both", na.rm = TRUE, size = 7)
ggsave(filename = file.path("figs","fig_prob_news_all_unanimous.pdf"), width = 13.92, height = 9.58)                        

## Proportion of hard news by channel, generation of the data set for graphs and models
df_f8_soft_channel <-
  df_f8 %>% 
  filter(channel %in% c("ABC", "CBS", "NBC")) %>% 
  group_by(year, channel) %>% 
  add_count() %>% 
  rename(n_shows = n) %>% 
  group_by(year, channel, news) %>% 
  add_count() %>% 
  rename(n_news = n) %>% 
  mutate(prob_news = n_news/n_shows) %>% 
  select(year, news, channel, n_shows, n_news, prob_news) %>% 
  filter(news != "not_clear") %>% 
  filter(!is.na(news)) %>% 
  filter(news == "soft") %>% 
  unique

## Graph 
df_f8_soft_channel  %>% 
  ggplot(aes(x = year, y = prob_news)) + 
  geom_line(aes(color = channel), size = 1) +
  scale_x_continuous(breaks= pretty_breaks()) + ylim(0,1) +
  theme_minimal(base_size = 20) + 
  scale_color_manual(values = wes_palette("IsleofDogs1")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y= "Proportion of Hard News", x = "Year", color = "Channel")
ggsave(filename = file.path("figs","fig_prob_soft_channel_unanimous.pdf"), width = 13.92, height = 9.58)                        



