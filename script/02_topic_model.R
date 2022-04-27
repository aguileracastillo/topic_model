#############################
#### Topic Model for DGRL v 17.5 ==> 6682 JOURNAL ARTICLES
#############################

## Topic Model of Journal Articles
dgrl_tm <- to_corpus %>% filter(type.x == "journalArticle")

dgrl_tm %>% group_by(year.x) %>% count(sort = TRUE) %>%
  ggplot(aes(year.x, n)) +
  geom_line()+
  xlab ("Year") +
  ylab ("Number of Documents")

