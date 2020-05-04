#####################################################################################
##
##    File Name:        07_regressions.R
##    Date:             2020-04-27
##    Author:           Daniel Weitzel
##    Email:            daniel.weitzel@utexas.edu
##    Webpage:          www.danweitzel.net
##    Purpose:          Running regressions on VTNA data
##    Date Used:        2020-04-27
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
library("texreg")
library("scales")
library("lme4")

## Loading the data 
source("scripts/02_recode.R")
df_regression <- read_csv("data/coded/sample_coded_long.csv")

## Regression models 
### Preparing the data set - Unit of observation: show
df_regression_show <-
  df_f8 %>% 
  filter(special == FALSE) %>% 
  ungroup() %>% 
  dplyr::select(year, channel, news, news_majority, geography, geography_majority, special) %>% 
  filter(channel %in% c("ABC", "CBS", "NBC")) %>% 
  mutate(soft = ifelse(news_majority == "soft", 1, 0),
         #                               ifelse(news_majority == "hard", 0, NA)),
         international  = ifelse(geography_majority == "international", 1, 0), 
         national       = ifelse(geography_majority == "national", 1, 0), 
         local          =  ifelse(geography_majority == "local", 1, 0), 
         unanimous_news = ifelse(!is.na(news), 1, 0),
         unanimous_geo  = ifelse(!is.na(geography),1, 0))


## Soft news show level regression
lm_soft_show_m   <- lm(soft ~ as.factor(channel) + scale(year), data = df_regression_show)
lm_soft_show_u   <- lm(soft ~ as.factor(channel) + scale(year), data = subset(df_regression_show, unanimous_news == 1))
lm_soft_show_my  <- lm(soft ~ as.factor(channel) + scale(year), data = subset(df_regression_show, year > 1969 & year < 2015))

### Geography show level regression
lm_int_show_m  <- lm(international ~ as.factor(channel) + scale(year), data = df_regression_show)
lm_int_show_my <- lm(international ~ as.factor(channel) + scale(year), data = subset(df_regression_show, year > 1969 & year < 2015))
lm_int_show_u  <- lm(international ~ as.factor(channel) + scale(year), data = subset(df_regression_show, unanimous_geo == 1))
lm_nat_show_m  <- lm(national ~ as.factor(channel) + scale(year), data = df_regression_show)
lm_nat_show_my <- lm(national ~ as.factor(channel) + scale(year), data = subset(df_regression_show, year > 1969 & year < 2015))
lm_nat_show_u  <- lm(national ~ as.factor(channel) + scale(year), data = subset(df_regression_show, unanimous_geo == 1))
lm_loc_show_m  <- lm(local ~ as.factor(channel) + scale(year), data = df_regression_show)
lm_loc_show_my <- lm(local ~ as.factor(channel) + scale(year),data = subset(df_regression_show, year > 1969 & year < 2015))
lm_loc_show_u  <- lm(local ~ as.factor(channel) + scale(year), data = subset(df_regression_show, unanimous_geo == 1))

## Exporting to latex
## Show-level table
## Majority coding
reg_tab1 <-
  texreg(list(lm_soft_show_m, lm_int_show_m, lm_nat_show_m, lm_loc_show_m),
         custom.coef.names = c("(Intercept)", "Channel: CBS", "Channel: NBC", "Year"), 
         caption = "Explaining The Provision of Different Types of News by Channel and Year",
         caption.above = TRUE,
         custom.model.names = c("Soft", "International", "National", "Local"),
         label = "tab:news_over_time",
         digits = 2, include.adjrs = FALSE, include.rmse = FALSE)
print(reg_tab1, file = "tabs/news_over_time.tex")

# Unanimous coding 
reg_tab2 <- 
  texreg(list(lm_soft_show_u, lm_int_show_u, lm_nat_show_u, lm_loc_show_u),
         custom.coef.names = c("(Intercept)", "Channel: CBS", "Channel: NBC", "Year"), 
         caption = "Proportion of News Types with unanimous coding",
         caption.above = TRUE,
         custom.model.names = c("Soft", "International", "National", "Local"),
         label = "tab:news_over_time_unanimous",
         digits = 2, include.adjrs = FALSE, include.rmse = FALSE)
print(reg_tab2, file = "tabs/news_over_time_unanimous.tex")

## Majority coding for subset of year 1970-2014
reg_tab3 <- 
  texreg(list(lm_soft_show_m, lm_int_show_m, lm_nat_show_m, lm_loc_show_m),
         custom.coef.names = c("(Intercept)", "Channel: CBS", "Channel: NBC", "Year"), 
         caption = "Explaining The Provision of Different Types of News by Channel and Year (1970-2014)",
         caption.above = TRUE,
         custom.model.names = c("Soft", "International", "National", "Local"),
         label = "tab:news_over_time_7014",
         digits = 2, include.adjrs = FALSE, include.rmse = FALSE)
print(reg_tab3, file = "tabs/news_over_time_19702014.tex")



### Code below generates the sample_coded_long.csv loaded above. The data was i wide format originally 
### but for the regression models we need it in long.
#
# source("scripts/02_recode_quality.R")
# 
# df_reg_news_coder <-
#   df_f8 %>%
#   dplyr::select(sample_id, year, starts_with("id_"), special, channel) %>%
#   pivot_longer(cols = starts_with("id_coder"),
#                names_to = "coder",names_prefix = "id_coder_", values_to = "coder_id")
# 
# df_reg_news_news <-
#   df_f8 %>%
#   rename(news_unanimous = news) %>%
#   dplyr::select(sample_id, year,  starts_with("news")) %>%
#   pivot_longer(cols = starts_with("news_coder"),
#                names_to = "coder",names_prefix = "news_coder_", values_to = "news_coded")
# 
# df_reg_geo_news <-
#   df_f8 %>%
#   rename(geography_unanimous = geography) %>%
#   dplyr::select(sample_id, year,  starts_with("geo")) %>%
#   pivot_longer(cols = starts_with("geography_coder"),
#                names_to = "coder",names_prefix = "geography_coder_", values_to = "geography_coded")
# 
# df_reg_news <-
#   df_reg_news_coder %>%
#   left_join(df_reg_news_news) %>% 
#   left_join(df_reg_geo_news)
# 
# write_csv(df_reg_news, "data/coded/sample_coded_long.csv")
# rm(df_reg_news_coder, df_reg_news_news,df_reg_geo_news)
