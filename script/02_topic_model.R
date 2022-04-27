#############################
#### Topic Model for DGRL v 17.5 ==> 6682 JOURNAL ARTICLES
#############################
## RUN PACKAGES
library(quanteda)
library(quanteda.textmodels)
library(quanteda.textplots)
library(quanteda.textstats)
library(stm)
library(stminsights)
library(wordcloud)

## Topic Model of Journal Articles
dgrl_tm <- to_corpus %>% filter(type.x == "journalArticle")

## r articles by year to insert in md
dgrl_tm %>% group_by(year.x) %>% count(sort = TRUE) %>%
  ggplot(aes(year.x, n)) +
  geom_line()+
  xlab ("Year") +
  ylab ("Number of Documents")

## Most frequent publication titles
dgrl_tm %>% count(pub_title.x) %>% arrange(desc(n))
