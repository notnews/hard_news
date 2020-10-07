#####################################################################################
##
##    File Name:        04_geo.R
##    Date:             2020-03-22
##    Author:           Daniel Weitzel
##    Email:            daniel.weitzel@utexas.edu
##    Purpose:          Geographical analysis of the F8 data
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

## Load the data
source("scripts/02_recode.R")

## Key dates
president_labels      <- c("Nixon", "Ford", "Carter", "Reagan", "Bush", "Clinton", "Bush", "Obama", "Trump")  
president_years       <- c(1971.5, 1975.5, 1979, 1985, 1991, 1997, 2005, 2013, 2019)
presidential_election <- seq(1968, 2016, 4)
midterm_election      <- seq(1970, 2018, 4)
wars_years            <- c(1971, 1975, 1978, 1989, 1991, 2001, 2003, 2005, 2014)         
wars_labels           <- c("Pentagon Papers", "Vietnam War Ends", "Camp David Accords", "Berlin Wall", 
                           "Gulf War", "Afghanistan", "Iraq", "Iraq War Protests", "Syria")

## Generating the geography data set
df_f8_geo <- 
  df_f8 %>% 
  filter(channel %in% c("ABC", "CBS", "NBC")) %>% 
  group_by(year) %>% 
  add_count() %>% 
  rename(n_shows = n) %>% 
  group_by(year, geography_majority) %>% 
  add_count() %>% 
  rename(n_news = n) %>% 
  mutate(prob_geo = n_news/n_shows) %>% 
  select(year, geography_majority , prob_geo) 

df_f8_geo_unanimous <- 
  df_f8 %>% 
  filter(channel %in% c("ABC", "CBS", "NBC")) %>% 
  group_by(year) %>% 
  add_count() %>% 
  rename(n_shows = n) %>% 
  group_by(year, geography) %>% 
  add_count() %>% 
  rename(n_news = n) %>% 
  mutate(prob_geo = n_news/n_shows) %>% 
  select(year, geography , prob_geo) 

## Table of local news per year 
df_f8_geo %>% 
  filter(geography_majority == "local") %>% 
  unique() %>% 
  head(n=20)

df_f8_geo %>% 
  filter(geography_majority == "local") %>% 
  unique() %>% 
  tail(n=20)

## Proportion of news by geography - majority coding
df_f8_geo %>% 
  group_by(geography_majority) %>% 
  mutate(label = if_else(year == min(year), as.character(geography_majority), NA_character_)) %>% 
  unique %>% 
  filter(geography_majority != "not_clear") %>% 
  filter(!is.na(geography_majority)) %>% 
  ggplot(aes(x = year, y = prob_geo, group = geography_majority, color = geography_majority)) + 
  geom_line(aes(color = geography_majority), size = 1) +
  scale_x_continuous(breaks= pretty_breaks(n=15)) +
  scale_y_continuous(breaks= pretty_breaks(n=10)) +
  theme_minimal(base_size = 20) + 
  scale_color_manual(values = wes_palette("IsleofDogs1")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "none") +
  geom_text_repel(aes(label = label), nudge_x = 1, direction = "both", na.rm = TRUE, size = 7) + 
  labs(y= "Proportion of News", x = "Year", color = "Scope")
ggsave(filename = file.path("figs","fig_geography.pdf"), width = 13.92, height = 9.58)                        
  
## Proportion of news by geography - unanimous coding
df_f8_geo_unanimous %>% 
  filter(geography!= "not_clear") %>% 
  filter(!is.na(geography)) %>% 
  ggplot(aes(x = year, y = prob_geo, group = geography)) + 
  geom_line(aes(color = geography), size = 1) +
  scale_x_continuous(breaks= pretty_breaks(n=15)) +
  scale_y_continuous(breaks= pretty_breaks(n=10)) +
  theme_minimal(base_size = 20) + 
  scale_color_manual(values = wes_palette("IsleofDogs1")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y= "Proportion of News", x = "Year", color = "Scope")
ggsave(filename = file.path("figs","fig_geography_unanimous.pdf"), width = 13.92, height = 9.58)                        


## Proportion of international news with wars added 
df_f8_geo %>% 
  filter(geography_majority == "international") %>% 
  filter(!is.na(geography_majority)) %>% 
  ggplot(aes(x = year, y = prob_geo, group = geography_majority)) + 
  geom_line(aes(color = geography_majority), size = 1) +
  geom_vline(xintercept = wars_years , linetype = "dashed", color = "darkgrey") +
  annotate("text", x = wars_years, y = c(0.38, 0.36, 0.39, 0.58, 0.55, 0.45, 0.47, 0.395, 0.4), label = wars_labels) +
  scale_x_continuous(breaks= pretty_breaks(n=15)) +
  scale_y_continuous(breaks= pretty_breaks(n=10)) +
  theme_minimal(base_size = 20) + 
  scale_color_manual(values = wes_palette("IsleofDogs1")[1]) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  labs(y= "Proportion of International News", x = "Year")
ggsave(filename = file.path("figs","fig_geography_wars.pdf"), width = 13.92, height = 9.58)                        

## Proportion of national news with elections added 
df_f8_geo %>% 
  filter(geography_majority == "national") %>% 
  filter(!is.na(geography_majority)) %>% 
  ggplot(aes(x = year, y = prob_geo, group = geography_majority)) + 
  geom_line(aes(color = geography_majority), size = 1) +
  geom_vline(xintercept = presidential_election, linetype = "longdash", color = "darkgrey") +
  scale_x_continuous(breaks= pretty_breaks(n=15)) +
  scale_y_continuous(breaks= pretty_breaks(n=10)) +
  theme_minimal(base_size = 20) + 
  #annotate("rect", xmin = 1969, xmax = 1974, ymin=0, ymax=Inf, alpha = .2) + 
  #annotate("rect", xmin = 1977, xmax = 1981, ymin=0, ymax=Inf, alpha = .2) + 
  #annotate("rect", xmin = 1989, xmax = 1993, ymin=0, ymax=Inf, alpha = .2) + 
  #annotate("rect", xmin = 2001, xmax = 2009, ymin=0, ymax=Inf, alpha = .2) + 
  #annotate("rect", xmin = 2017, xmax = 2021, ymin=0, ymax=Inf, alpha = .2) + 
  #annotate("text", x = president_years, y = c(0.67, 0.67, 0.67, 0.67, 0.67, 0.67, 0.67, 0.67, 0.67), label = president_labels) +
  scale_color_manual(values = wes_palette("IsleofDogs1")[3]) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  labs(y= "Proportion of National News", x = "Year", color = "Scope", caption = "Presidential elections highlighted")
ggsave(filename = file.path("figs","fig_geography_presidential.pdf"), width = 13.92, height = 9.58)  

df_f8_geo %>% 
  filter(geography_majority == "national") %>% 
  filter(!is.na(geography_majority)) %>% 
  ggplot(aes(x = year, y = prob_geo, group = geography_majority)) + 
  geom_line(aes(color = geography_majority), size = 1) +
  geom_vline(xintercept = midterm_election, linetype = "dotdash", color = "darkgrey") +
  scale_x_continuous(breaks= pretty_breaks(n=15)) +
  scale_y_continuous(breaks= pretty_breaks(n=10)) +
  theme_minimal(base_size = 20) + 
  scale_color_manual(values = wes_palette("IsleofDogs1")[3]) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  labs(y= "Proportion of National News",x = "Year", color = "Scope", caption = "Midterm Elections highlighted")
ggsave(filename = file.path("figs","fig_geography_midterm.pdf"), width = 13.92, height = 9.58)  

