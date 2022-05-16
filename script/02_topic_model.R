#############################
#### Topic Model for DGRL v 17.5 ==> 6682 JOURNAL ARTICLES
#############################
## RUN PACKAGES
library(ggplot2)
library(quanteda)
library(quanteda.textmodels)
library(quanteda.textplots)
library(quanteda.textstats)
library(seededlda)
library(stm)
library(stminsights)
library(topicmodels)
library(tm)
library(wordcloud)
library(magrittr)
library(LDAvis)

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
top_journals <- dgrl175_tm %>% count(pub_title.x) %>% arrange(desc(n))
print(top_journals)
write.csv(top100, "~/GitHub/topic_model/data\\top100.csv", row.names = TRUE)


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
                      remove_url = TRUE)

## SELECT TOKENS (NO STOPWORDS) & TO LOWERCASE
dgrl175_tokens <- tokens_select(dgrl175_tokens, pattern = stopwords("en"), selection = "remove")
dgrl175_tokens <- tokens_tolower(dgrl175_tokens)
print(dgrl175_tokens)

## APPLY STEMMING ALGORITHM
dgrl175_tokens <- tokens_wordstem(dgrl175_tokens, language = "english")

## No stemming to provide a more human readable descriptor (De Battisti et al 2015)

### How to view a document in a corpus
dgrl175_corpus[[3]]

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

## 6682 documents and 18825 features (99.60% sparse)

### TOP FEATURES IN DFM_DGRL -- Best practice remove very rare and very common
topfeatures(dfm_dgrl175, 250)
## Wordclouds
set.seed(123)
textplot_wordcloud(dfm_dgrl175, max_words = 100)

### TFIDF Term Frequency-Inverse Document Frequency
dfm_dgrl_tfidf <- dfm_tfidf(dfm_dgrl175)
print(dfm_dgrl_tfidf)

#### DIMENSIONALITY REDUCTION ####
## TRIM VERY RARE FEATURES Sparse matrix > 1096 features 94.43% sparsity
dfm_dgrl175_trim <- dfm_trim(dfm_dgrl175, min_termfreq = 100)
print(dfm_dgrl175_trim)

## TRIM VERY COMMON FEATURES IF OCCURRENCE >10% OF DOCUMENTS => REMOVE
## 6682 documents and 929 features 96.65% sparse
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

## Test Number of Topics (topicmodels) package
n_topics <- c(10, 20, 50, 80, 100, 150, 200, 300)

optimal_k <- n_topics %>%
  map(LDA, x = train_dgrl175, control = list(seed = 2023))


tibble(k = n_topics,
           perplex = map_dbl(optimal_k, perplexity)) %>%
  ggplot(aes(k, perplex)) +
  geom_point() +
  geom_line() +
  labs(title = "Evaluating LDA topic models",
       subtitle = "Optimal number of topics (smaller is better)",
       x = "Number of topics",
       y = "Perplexity")

## SEEDEDLDA PACKAGE
train_lda25 <- textmodel_lda(train_dgrl175, k = 25)
terms(train_lda25, 10)
train_lda80 <- textmodel_lda(train_dgrl175, k = 80)
terms(train_lda80, 10)
train_lda100 <- textmodel_lda(train_dgrl175, k = 100)
terms(train_lda100, 10)

## number of topics determined by number of keys in dictionary ##
dictionary_dgrl175 <- textmodel_seededlda(train_dgrl175, dictionary = dgrl175_dictionary)
terms(dictionary_dgrl175, 25)

