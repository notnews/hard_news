## Hard News: Rise of Apolitical News in Network Television News

Network television news is among the most frequently consumed news in the country. But there is little data on what is covered on network news. Or how the quality of network news has fared over the years. We exploit the Vanderbilt Television News Archive, the largest publicly available database of TV news, to learn about two important aspects of the production of news: geographic focus and political content. Using data from a random sample of over 5,000 broadcast segments spanning 1968--2019, we find that the percentage of network television news devoted to topics unrelated to politics steadily increased from less than 5% in 1968 to over 10% in the last 15 years. The pattern of change in geographic focus is more complex, but there is a clear rise in the percentage of local news over the last two decades. The percentage of local news increased from about 5% in 2000 to over 25% in 2019.

<p align="center">
<img src="figs/fig_prob_news_all.png" width="750">
</p>

### Data

* [Vanderbilt Broadcast TV News Archives](https://github.com/notnews/vandy_tv_news_abstracts)
* [Coding instrument](data/coding_instrument.docx)
* [Sample](data/sample_questions.csv)
* [Screenshots](data/screenshots/)
* [Gold standard coding](data/sample_questions_gold.csv)
* [Final data](data/final_data.csv)

### Scripts

* [Create Sample](scripts/01_clean_sample.R)
    - Stratified random sample by year
    - Weighted random sample by duration of the news segment

* [Recode Data](scripts/02_recode.R)

* [Coverage of soft news, by channel, by year, by weekday/weekend](scripts/03_soft.R)

* [Coverage of local, national, and foreign, by channel, by year, month, ](scripts/04_geo.R)

* [Coverage of topics in agg., by channel, year, etc.](scripts/05_topics.R)

* [Summary statistics on full data set](scripts/06_data_description.R)

* [Regression models](scripts/07_regression.R)

* [Assess Reliability and Quality of Data](scripts/08_quality_checks.R)

### Figures and Tables

* [Figures](figs/)
* [Tables](tabs/)

### Manuscript

* [PDF](ms/us_news.pdf)
* [.tex](ms/us_news.tex)
* [.bib](ms/us_news.bib)

### Authors

Gaurav Sood and Daniel Weitzel
