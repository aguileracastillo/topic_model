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
# QUALITAT* 448 MATCHES
kw_ql <- kwic(dgrl175_tokens, pattern = "qualitat*")
View(kw_ql)
# QUANTITAT* 292 MATCHES
kw_qt <- kwic(dgrl175_tokens, pattern = "quantitat*")
head(kw_qt)
View(kw_qt)
# EMPIRIC* 984 MATCHES
kw_emp <- kwic(dgrl175_tokens, pattern = "empiric*")
head(kw_emp)
View(kw_emp)
# BIBLIO* 48 MATCHES
kw_biblio <- kwic(dgrl175_tokens, pattern = "biblio*")
View(kw_biblio)

## KWIC PHRASES
# PUBLIC VALUE* 289 MATCHES
kw_pv <- kwic(dgrl175_tokens, pattern = phrase("public value*"))
head(kw_pv)
View(kw_pv)
# NEW PUBLIC MANAGEMENT 40 MATCHES 
kw_new_pub_mgmt <- kwic(dgrl175_tokens, pattern = phrase("new public management"))
head(kw_new_pub_mgmt)

#################
## Create a dictionary of words of interest
################
dgrl175_dictionary <- dictionary(list(verb = c("reduc*", "replac*", "elimin*", "save", "lower", "substitut*", "autom*"),
                                      object = c("labor", "worker*", "human", "employe*", "manpow*", "job*"),
                                      attribute = c("cost*", "expenditur*", "expense*", "hour*", "intens*", "task*", "time", "skill")))

## Test tokens_lookup
labor_saving <- tokens_lookup(dgrl175_tokens, dgrl175_dictionary, nomatch = "NULL")

tokens_lookup(dgrl175_tokens, dgrl175_dictionary, nomatch = "other") %>%
  dfm() %>%
  dfm_weight(scheme = "prop")

#### CONSTRUCT A DOCUMENT-FEATURE MATRIX (DFM) ####
dfm_dgrl175 <- dfm(dgrl175_tokens)
print(dfm_dgrl175)
ndoc(dfm_dgrl175)
nfeat(dfm_dgrl175)

## 6682 documents and 28749 features (99.72% sparse)

### TOP FEATURES IN DFM_DGRL -- Best practice remove very rare and very common
topfeatures(dfm_dgrl175, 250)
## Wordclouds
set.seed(123)
textplot_wordcloud(dfm_dgrl175, max_words = 100)

### TFIDF Term Frequency-Inverse Document Frequency
dfm_dgrl_tfidf <- dfm_tfidf(dfm_dgrl175)
print(dfm_dgrl_tfidf)

#### DIMENSIONALITY REDUCTION ####
## TRIM VERY RARE FEATURES Sparse matrix > 95.77% sparsity at 100 freq
dfm_dgrl175_trim <- dfm_trim(dfm_dgrl175, min_termfreq = 100)
print(dfm_dgrl175_trim)

## TRIM VERY COMMON FEATURES IF OCCURRENCE >10% OF DOCUMENTS => REMOVE
dfm_dgrl175_trim_docfreq <- dfm_trim(dfm_dgrl175_trim, max_docfreq = 0.1, docfreq_type = "prop")
print(dfm_dgrl175_trim_docfreq)
topfeatures(dfm_dgrl175_trim_docfreq, 250)

#### FCM Feature Co-occurrence Matrix ####
fcm_dfm_dgrl175_trim_docfreq <- fcm(dfm_dgrl175_trim_docfreq)
dim(fcm_dfm_dgrl175_trim_docfreq)
topfeatures(fcm_dfm_dgrl175_trim_docfreq, 50)

