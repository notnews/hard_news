#####################################################################################
##
##    File Name:        05_topics.R
##    Date:             2020-03-22
##    Author:           Daniel Weitzel
##    Email:            daniel.weitzel@utexas.edu
##    Purpose:          Topic analysis of the F8 data
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
df_econ          <- read_csv("data/econ/gdp_change.csv")
president_labels <- c("Nixon", "Ford", "Carter", "Reagan", "Bush", "Clinton", "Bush", "Obama", "Trump")  
president_years  <- c(1971.5, 1975.5, 1979, 1985, 1991, 1997, 2005, 2013, 2019)

## Stories about war
df_f8 %>% 
  ungroup() %>% 
  dplyr::select(year, war,news_majority) %>% 
  group_by(year) %>% 
  add_count() %>% 
  rename(n_shows = n) %>% 
  group_by(year) %>% 
  add_tally(war) %>% 
  rename(n_war = n) %>% 
  mutate(prob_war = n_war/n_shows) %>% 
  select(year, news_majority, n_shows, n_war, prob_war) %>% 
  filter(news_majority != "not_clear") %>% 
  filter(!is.na(news_majority)) %>% 
  filter(news_majority == "hard") %>% 
  ggplot(aes(x = year, y = prob_war)) + 
  geom_line() +
  scale_x_continuous(breaks= pretty_breaks()) + ylim(0,1) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y= "Proportion of News About War", x = "Year")

## Stories about economy
## Prepare the econ data for merge 
df_econ <-
  df_econ %>% 
  separate(DATE, into = c("year", "drop"), sep = "-") %>% 
  select(-drop) %>% 
  rename(gdp_change = A191RL1A225NBEA) %>% 
  mutate(year = as.numeric(year),
         gdp_change = gdp_change /100) %>% 
  filter(year > 1967)


## Prepare the news data and merge it with the econ data 
df_f8_econ <-
  df_f8 %>% 
  ungroup() %>% 
  select(year, economy,news_majority) %>% 
  group_by(year) %>% 
  add_count() %>% 
  rename(n_shows = n) %>% 
  group_by(year) %>% 
  add_tally(economy) %>% 
  rename(n_economy = n) %>% 
  mutate(prob_economy = n_economy/n_shows) %>% 
  select(year, news_majority, n_shows, n_economy, prob_economy) %>% 
  filter(news_majority != "not_clear") %>% 
  filter(!is.na(news_majority)) %>% 
  filter(news_majority == "hard") %>% 
  unique() %>% 
  left_join(df_econ, by = "year")

## Graph about econ stories and gdp
df_f8_econ %>% 
  ggplot(.,aes(x = year)) + 
  geom_line(aes(y = prob_economy)) +
  geom_line(aes(y = gdp_change), color = "red", linetype = "dashed") + 
  scale_x_continuous(breaks= pretty_breaks()) + 
  scale_y_continuous(breaks= pretty_breaks()) + #ylim(-0.25,0.5) +
  theme_minimal(base_size = 20) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y= "Proportion", x = "Year")
ggsave(filename = file.path("figs","fig_topic_econ.png"), width = 13.92, height = 9.58)                        

## Stories about welfare
df_f8 %>% 
  ungroup() %>% 
  select(year, welfare,news_majority) %>% 
  group_by(year) %>% 
  add_count() %>% 
  rename(n_shows = n) %>% 
  group_by(year) %>% 
  add_tally(welfare) %>% 
  rename(n_welfare = n) %>% 
  mutate(prob_welfare = n_welfare/n_shows) %>% 
  select(year, news_majority, n_shows, n_welfare, prob_welfare) %>% 
  filter(news_majority != "not_clear") %>% 
  filter(!is.na(news_majority)) %>% 
  filter(news_majority == "hard") %>% 
  ggplot(aes(x = year, y = prob_welfare)) + 
  geom_line() +
  scale_x_continuous(breaks= pretty_breaks()) + ylim(0,1) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y= "Proportion of News About the welfare", x = "Year")


## Stories about election
df_f8 %>% 
  ungroup() %>% 
  select(year, election,news_majority) %>% 
  group_by(year) %>% 
  add_count() %>% 
  rename(n_shows = n) %>% 
  group_by(year) %>% 
  add_tally(election) %>% 
  rename(n_election = n) %>% 
  mutate(prob_election = n_election/n_shows) %>% 
  select(year, news_majority, n_shows, n_election, prob_election) %>% 
  filter(news_majority != "not_clear") %>% 
  filter(!is.na(news_majority)) %>% 
  filter(news_majority == "hard") %>% 
  ggplot(aes(x = year, y = prob_election)) + 
  geom_line() +
  scale_x_continuous(breaks= pretty_breaks()) + ylim(0,1) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y= "Proportion of News About the election", x = "Year")

df_f8 %>% 
  ungroup() %>% 
  select(year, p_trump,news_majority) %>% 
  group_by(year) %>% 
  add_count() %>% 
  rename(n_shows = n) %>% 
  group_by(year) %>% 
  add_tally(p_trump) %>% 
  rename(n_p_trump = n) %>% 
  mutate(prob_p_trump = n_p_trump/n_shows) %>% 
  select(year, news_majority, n_shows, n_p_trump, prob_p_trump) %>% 
  filter(news_majority != "not_clear") %>% 
  filter(!is.na(news_majority)) %>% 
  filter(news_majority == "hard") %>% 
  ggplot(aes(x = year, y = prob_p_trump)) + 
  geom_line() +
  scale_x_continuous(breaks= pretty_breaks()) + ylim(0,1) +
  theme_minimal(base_size = 20) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y= "Proportion of News about Trump", x = "Year", color = "Channel")
ggsave(filename = file.path("figs","fig_topic_trump.png"), width = 13.92, height = 9.58)                        



### News about presidents 
df_f8 %>% 
  ungroup() %>% 
  select(year, starts_with("p_")) %>% 
  pivot_longer(-year, names_to = "president", values_to = "count") %>% 
  group_by(year, president) %>%
  add_tally(count) %>% 
  ungroup() %>% 
  mutate(prob_president = n/100,
         prob_president = ifelse(prob_president == 0, NA, prob_president),
         president = str_replace_all(president, "p_", ""),
         president = str_to_title(president))  %>% 
  #filter(news_majority != "not_clear") %>% 
  #filter(!is.na(news_majority)) %>% 
  #filter(news_majority == "hard") %>% 
  ggplot(aes(x = year, y = prob_president, group = president)) + 
  geom_line(aes(color = president), size = 1) +
  scale_x_continuous(breaks= pretty_breaks(n=20)) +
  scale_y_continuous(breaks= pretty_breaks(n=10))+ #ylim(0,0.78) +
  theme_minimal(base_size = 20) + 
  annotate("rect", xmin = 1969, xmax = 1974, ymin=0, ymax=Inf, alpha = .2) + 
  annotate("rect", xmin = 1977, xmax = 1981, ymin=0, ymax=Inf, alpha = .2) + 
  annotate("rect", xmin = 1989, xmax = 1993, ymin=0, ymax=Inf, alpha = .2) + 
  annotate("rect", xmin = 2001, xmax = 2009, ymin=0, ymax=Inf, alpha = .2) + 
  annotate("rect", xmin = 2017, xmax = 2021, ymin=0, ymax=Inf, alpha = .2) + 
  annotate("text", x = president_years, y = c(0.70, 0.65, 0.70, 0.65, 0.70, 0.65, 0.70, 0.65, 0.70), label = president_labels) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y= "Proportion of News about Presidents", x = "Year", color = "President")
ggsave(filename = file.path("figs","fig_topic_president.png"), width = 13.92, height = 9.58)                        

## Presidents as only one line
df_f8 %>% 
  ungroup() %>% 
  select(year, starts_with("p_")) %>% 
  pivot_longer(-year, names_to = "president", values_to = "count") %>% 
  group_by(year, president) %>%
  add_tally(count) %>% 
  ungroup() %>% 
  mutate(prob_president = n/100,
         prob_president = ifelse(prob_president == 0, NA, prob_president),
         president = str_replace_all(president, "p_", ""),
         president = str_to_title(president)) %>% 
  mutate(keep = ifelse(president == "Nixon" & year > 1968 & year < 1975, 1,
                       ifelse(president == "Ford" & year > 1973 & year < 1978, 1,
                              ifelse(president == "Carter" & year > 1977 & year < 1982, 1,
                                     ifelse(president == "Reagan" & year > 1981 & year < 1990, 1,
                                            ifelse(president == "Bush" & year > 1989 & year < 1994, 1,
                                                   ifelse(president == "Clinton" & year > 1993 & year < 2001, 1,
                                                          ifelse(president == "Bush" & year > 2000 & year < 2009, 1,
                                                                 ifelse(president == "Obama" & year > 2008 & year < 2017, 1,
                                                                        ifelse(president == "Trump" & year > 2016 & year < 2021, 1, 0)))))))))) %>% 
  filter(keep == 1) %>% 
  dplyr::select(-c(keep, count, n)) %>% 
  unique %>% 
  ggplot(aes(x = year, y = prob_president)) + 
  geom_line(size = 1) +
  scale_x_continuous(breaks= pretty_breaks(n=20)) +
  scale_y_continuous(breaks= pretty_breaks(n=10))+ #ylim(0,0.78) +
  theme_minimal(base_size = 20) + 
  annotate("rect", xmin = 1969, xmax = 1974, ymin=0, ymax=Inf, alpha = .2) + 
  annotate("rect", xmin = 1977, xmax = 1981, ymin=0, ymax=Inf, alpha = .2) + 
  annotate("rect", xmin = 1989, xmax = 1993, ymin=0, ymax=Inf, alpha = .2) + 
  annotate("rect", xmin = 2001, xmax = 2009, ymin=0, ymax=Inf, alpha = .2) + 
  annotate("rect", xmin = 2017, xmax = 2021, ymin=0, ymax=Inf, alpha = .2) + 
  annotate("text", x = president_years, y = c(0.68, 0.66, 0.68, 0.66, 0.68, 0.66, 0.68, 0.66, 0.68), label = president_labels) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y= "Proportion of News About The President", x = "Year", color = "President")
ggsave(filename = file.path("figs","fig_topic_presidency_combined.png"), width = 13.92, height = 9.58)                        


## Trump and Clinton
president_labels <- c("Obama", "Trump")  
president_years  <- c(2013, 2019)

df_f8 %>% 
  ungroup() %>% 
  select(year, starts_with("p_")) %>% 
  pivot_longer(-year, names_to = "president", values_to = "count") %>% 
  group_by(year, president) %>%
  add_tally(count) %>% 
  ungroup() %>% 
  mutate(prob_president = n/100,
         prob_president = ifelse(prob_president == 0, NA, prob_president),
         president = str_replace_all(president, "p_", ""),
         president = str_to_title(president))  %>% 
  filter(president %in% c("Obama", "Clinton", "Trump")) %>% 
  #filter(news_majority != "not_clear") %>% 
  #filter(!is.na(news_majority)) %>% 
  #filter(news_majority == "hard") %>% 
  ggplot(aes(x = year, y = prob_president, group = president)) + 
  geom_line(aes(color = president), size = 1) +
  scale_x_continuous(breaks= pretty_breaks(n=8)) +
  scale_y_continuous(breaks= pretty_breaks(n=10))+ #ylim(0,0.78) +
  theme_minimal(base_size = 20) + xlim(2010, 2021) +
  annotate("rect", xmin = 2017, xmax = 2021, ymin=0, ymax=Inf, alpha = .2) + 
  annotate("text", x = president_years, y = c(0.70, 0.70), label = president_labels) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values=c("Obama"="#00B0F6", "Clinton"="#7CAE00", "Trump"="#E76BF3")) +
  labs(y= "Proportion of News about Presidents", x = "Year", color = "President")
ggsave(filename = file.path("figs","fig_topic_trump_clinton.png"), width = 13.92, height = 9.58)                        

