#############################
#### Topic Model for DGRL v 17.5 ==> 6682 JOURNAL ARTICLES
#############################
## RUN PACKAGES
library(ggplot2)
library(quanteda)
library(quanteda.textmodels)
library(quanteda.textplots)
library(quanteda.textstats)
library(tidytext)
library(seededlda)
library(stm)
library(stminsights)
library(topicmodels)
library(tm)
library(wordcloud)
library(magrittr)
library(LDAvis)
library(ggplot2)
library(spacyr)

## Topic Model of Journal Articles / 2000 ~ 2021
dgrl175_tm <- to_corpus %>% filter(type.x == "journalArticle") %>%
  filter(year.x > 1999) %>% filter(year.x < 2022)

## r articles by year to insert in md
dgrl175_tm %>% group_by(year.x) %>% count(sort = TRUE) %>%
  ggplot(aes(year.x, n)) +
  geom_line()+
  xlab ("Year") +
  ylab ("Number of Documents")

## Publications by year 2000 ~ 2021 n=6664
dgrl175_tm %>% count(year.x) %>% arrange(desc(year.x)) %>% print(n = 23)


## Most frequent publication titles
top_journals <- dgrl175_tm %>% count(pub_title.x) %>% arrange(desc(n))
print(top_journals)
write.csv(top_journals, "~/GitHub/topic_model/data\\top_journals.csv", row.names = TRUE)

## After detecting uninformative but pervasive string => Remove
dgrl175_tm$abstract.x <- gsub("â€*?", " ", dgrl175_tm$abstract.x)

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
                      remove_url = TRUE,
                      padding = TRUE)
  

## my_stopwords from downstream eyeballing ##

my_stopwords <- c("(c)", "elsevier", "ltd*", "all", "rights", "reserved",
                  "abstract", "copyright*", "inc*", "e.g*")

## SELECT TOKENS (NO STOPWORDS) & TO LOWERCASE
dgrl175_tokens <- tokens_select(dgrl175_tokens, pattern = c(stopwords("en"), my_stopwords), 
                                selection = "remove", padding = TRUE)
dgrl175_tokens <- tokens_tolower(dgrl175_tokens)
print(dgrl175_tokens)

### WORD COLLOCATIONS ### --> "future research" #42
word_collocations <- textstat_collocations(dgrl175_tokens, min_count = 10)
head(word_collocations, 100)

## APPLY STEMMING ALGORITHM??
dgrl175_tokens <- tokens_wordstem(dgrl175_tokens, language = "english")

#### n-gram search ####

## Unigram ##
dgrl175_unigram = tokens_ngrams(dgrl175_tokens, n = 1)
dgrl175_unigram_dfm <- dfm(dgrl175_unigram)
unigram_freq <-textstat_frequency(dgrl175_unigram_dfm)

#plot wordcloud to show most frequent words
textplot_wordcloud(dgrl175_unigram_dfm, max_words = 100,
                   ordered_color = TRUE)

## Bigram ##
dgrl175_bigram = tokens_ngrams(dgrl175_tokens, n = 2)
dgrl175_bigram_dfm <- dfm(dgrl175_bigram)
bigram_freq <-textstat_frequency(dgrl175_bigram_dfm)

#plot wordcloud to show most frequent words
textplot_wordcloud(dgrl175_bigram_dfm, max_words = 50,
                   ordered_color = TRUE)

## Trigram ##
dgrl175_trigram = tokens_ngrams(dgrl175_tokens, n = 3)
dgrl175_trigram_dfm <- dfm(dgrl175_trigram)
trigram_freq <-textstat_frequency(dgrl175_trigram_dfm)

#plot wordcloud to show most frequent words
textplot_wordcloud(dgrl175_trigram_dfm, max_words = 30,
                   ordered_color = TRUE)


## No stemming to provide a more human readable descriptor (De Battisti et al 2015)

## POS with SpaCyR ##

### How to view a document in a corpus
dgrl175_corpus[[1170]]

#### KEYWORDS-IN-CONTEXT (theories sample test)
# NPM 28 MATCHES
kw_npm <- kwic(dgrl175_tokens, pattern = "npm")
head(kw_npm)
View(kw_npm)
# UTAUT 96 MATCHES
kw_utaut <- kwic(dgrl175_tokens, pattern = "utaut")
head(kw_utaut)
View(kw_utaut)
# THEOR* 1950 MATCHES --> CHECK CO-OCCURRENCE
kw_theor <- kwic(dgrl175_tokens, pattern = "theor*")
head(kw_theor)
View(kw_theor)
# BIBLIO* 48 MATCHES
kw_biblio <- kwic(dgrl175_tokens, pattern = "biblio*")
View(kw_biblio)

### KWIC WORK RELATED ###
# work* 1k+ MATCHES
kw_work <- kwic(dgrl175_tokens, pattern = "work*")
head(kw_work)
View(kw_work)

#################
## Create a dictionary of words of interest
################
dgrl175_dictionary <- dictionary(list(verb = c("reduc*", "replac*", "elimin*", "save", "lower", "substitut*", "autom*"),
                                      object = c("labor", "worker*", "human", "employe*", "manpow*", "job*"),
                                      attribute = c("cost*", "expenditur*", "expense*", "hour*", "intens*", "task*", "time", "skill")))
print(dgrl175_dictionary)

## Test tokens_lookup
labor_saving <- tokens_lookup(dgrl175_tokens, dgrl175_dictionary, nomatch = "NULL")

tokens_lookup(dgrl175_tokens, dgrl175_dictionary, nomatch = "other") %>%
  dfm() %>%
  dfm_weight(scheme = "prop")

### WORK RELATED VOCABULARY
work_vocab <- c("job*", "employ*", "work*", "labo*", "occupat*", "workforc*", "workplac*", "unemploy*", "workload*", "task*", "staff*")

#### CONSTRUCT A DOCUMENT-FEATURE MATRIX (DFM) ####
dfm_dgrl175 <- dfm(dgrl175_tokens)
print(dfm_dgrl175)
ndoc(dfm_dgrl175)
nfeat(dfm_dgrl175)

## 6664 documents and 18748 features (99.60% sparse)

### TOP FEATURES IN DFM_DGRL -- Best practice remove very rare and very common
topfeatures(dfm_dgrl175, 250)
## Wordclouds
set.seed(123)
textplot_wordcloud(dfm_dgrl175, max_words = 100)

### TFIDF Term Frequency-Inverse Document Frequency
dfm_dgrl_tfidf <- dfm_tfidf(dfm_dgrl175)
print(dfm_dgrl_tfidf)

#### DIMENSIONALITY REDUCTION ####
## TRIM VERY RARE FEATURES Sparse matrix > 1081 features 94.42% sparsity
dfm_dgrl175_trim <- dfm_trim(dfm_dgrl175, min_termfreq = 100)
print(dfm_dgrl175_trim)

## TRIM VERY COMMON FEATURES IF OCCURRENCE >10% OF DOCUMENTS => REMOVE
## 6682 documents and 916 features 96.64% sparse
dfm_dgrl175_trim_docfreq <- dfm_trim(dfm_dgrl175_trim, max_docfreq = 0.1, docfreq_type = "prop")
print(dfm_dgrl175_trim_docfreq)
topfeatures(dfm_dgrl175_trim_docfreq, 250)

#### FCM Feature Co-occurrence Matrix ####
## What can be done with this? ##
fcm_dfm_dgrl175_trim_docfreq <- fcm(dfm_dgrl175_trim_docfreq)
dim(fcm_dfm_dgrl175_trim_docfreq)
topfeatures(fcm_dfm_dgrl175_trim_docfreq, 50)

###############################
#### LDA Model
###############################

## Split train set and test set
set.seed(60091)
data_to_lda <- dfm_dgrl175_trim_docfreq
n <- nrow(data_to_lda)

splitter <- sample(1:n, round(n * 0.75))
train_dgrl175 <- data_to_lda[splitter, ]
test_dgrl175 <- data_to_lda[-splitter, ]



#### TESTING SEEDEDLDA PACKAGE #### Do not run
train_lda25 <- textmodel_lda(train_dgrl175, k = 25)
terms(train_lda25, 10)


## number of topics determined by number of keys in dictionary ##
dictionary_dgrl175 <- textmodel_seededlda(train_dgrl175, dictionary = dgrl175_dictionary)
terms(dictionary_dgrl175, 25)

#### CONVERT FOR VISUALIZATION IN GGPLOT2 ####
converted <- convert(train_dgrl175, to = "topicmodels")
converted12 <- LDA(converted, k = 12, control = list(seed = 123))
converted25 <- LDA(converted, k = 25, control = list(seed = 123))
converted50 <- LDA(converted, k = 50, control = list(seed = 123))
tidy_converted12 <- tidy(converted12)
tidy_converted25 <- tidy(converted25)
tidy_converted50 <- tidy(converted50)

## Visualization k = 12
top_terms <- tidy_converted12 %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(
    topic = factor(topic),
    term = reorder_within(term, beta, topic)
  ) %>%
  ggplot(aes(term, beta, fill = topic)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  scale_x_reordered() +
  facet_wrap(facets = vars(topic), scales = "free", ncol = 2) +
  coord_flip()

## Visualization k = 25
top_terms <- tidy_converted25 %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(
    topic = factor(topic),
    term = reorder_within(term, beta, topic)
  ) %>%
  ggplot(aes(term, beta, fill = topic)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  scale_x_reordered() +
  facet_wrap(facets = vars(topic), scales = "free", ncol = 4) +
  coord_flip()

