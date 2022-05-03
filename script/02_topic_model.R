#############################
#### Topic Model for DGRL v 17.5 ==> 6682 JOURNAL ARTICLES
#############################
## RUN PACKAGES
library(ggplot2)
library(quanteda)
library(quanteda.textmodels)
library(quanteda.textplots)
library(quanteda.textstats)
library(stm)
library(stminsights)
library(wordcloud)
library(magrittr)

## Topic Model of Journal Articles
dgrl175_tm <- to_corpus %>% filter(type.x == "journalArticle")

## r articles by year to insert in md
dgrl175_tm %>% group_by(year.x) %>% count(sort = TRUE) %>%
  ggplot(aes(year.x, n)) +
  geom_line()+
  xlab ("Year") +
  ylab ("Number of Documents")

## Publications by year
dgrl175_tm %>% count(year.x) %>% arrange(desc(year.x)) %>% print(n =40)

## Most frequent publication titles
dgrl175_tm %>% count(pub_title.x) %>% arrange(desc(n)) %>% print(n = 100)

#### BUILD A CORPUS 
## QUANTEDA Best practice: Corpus should remain unchanged during subsequent analysis and processing
## Construct a corpus from "abstract.x" column in dgrl175_tm
dgrl175_corpus <- corpus(dgrl175_tm, text_field = "abstract.x")
print(dgrl175_corpus)
ndoc(dgrl175_corpus)
head(docvars(dgrl175_corpus))
summary(dgrl175_corpus, 5)
docvars(dgrl175_corpus, field = "year.x")

## Histogram # Tokens in dgrl175_corpus
## Is is poss to make a histogram with tokens number?

## TOKENIZATION & REMOVE PUNTUATION, SYMBOLS, NUMBERS, URL
dgrl175_tokens <- tokens(dgrl175_corpus, what = "word",
                      remove_punct = TRUE,
                      remove_symbols = TRUE,
                      remove_numbers = TRUE,
                      remove_url = TRUE)

## SELECT TOKENS (NO STOPWORDS) & TO LOWERCASE
dgrl175_tokens <- tokens_select(dgrl175_tokens, pattern = stopwords("en"), selection = "remove")
dgrl175_tokens <- tokens_tolower(dgrl175_tokens)
print(dgrl175_tokens)

## No stemming to provide a more human readable descriptor (De Battisti et al 2015)

### How to view a document in a corpus
dgrl175_corpus[[521]]

## KEYWORDS IN CONTEXT

## Create a dictionary of words of interest

