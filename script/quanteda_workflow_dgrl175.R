## QUANTEDA WORKFLOW FOR DGRL17.5
library(rmarkdown)
library(knitr)
library(tidyverse)
library(quanteda)
library(quanteda.textmodels)
library(quanteda.textplots)
library(quanteda.textstats)
library(readtext)
library(stm)
library(stminsights)
library(wordcloud)
library(gsl)
library(topicmodels)
library(devtools)
library(quanteda.corpora)
library(spacyr)
library(newsmap)
library(seededlda)
library(readr)
library(naniar)
library(visdat)
library(dplyr)
library(ggplot2)
library(lubridate)
library(seededlda)
library(tm)
library(readr)
library(revtools)
library(caret)
library(stringr)
library(here)

## Set project path to GitHub 
here()

#### QUANTEDA WORKFLOW DATA IMPORT ####
DGRLv17_5_zotero <- read_csv(here("data", "DGRLv17.5_zotero.csv"))
View(DGRLv17_5_zotero)
## Get names of variables in dataset
names(DGRLv17_5_zotero)
## View missing information
vis_miss(DGRLv17_5_zotero, warn_large_data = FALSE)
## 80.7% missing information... 
## Keep variables of interest ("Key", "Item Type", "Publication Year", "Author", "Title", "Publication Title", "Abstract Note", "Language", "Manual Tags", "Conference Name")

## Select variables of interest
DGRLv17_5_zotero_redux <- DGRLv17_5_zotero %>% select(1:9, 11, 29, 40, 72)
View(DGRLv17_5_zotero_redux)
vis_miss(DGRLv17_5_zotero_redux)

DGRLv17_5_zotero_redux %>%
  group_by(type) %>%
  miss_var_summary() %>%
  filter(variable == "abstract") %>%
  arrange(pct_miss)

DGRLv17_5_zotero_redux %>%
  group_by(type) %>%
  miss_var_summary() %>%
  filter(variable == "DOI") %>%
  arrange(pct_miss)

DGRLv17_5_zotero_redux %>%
  group_by(type) %>%
  miss_var_summary() %>%
  filter(variable == "year") %>%
  arrange(pct_miss)
  

##YEAR MISS --> Journal Article 49.8% // Conference paper 0.3%

DGRLv17_5_zotero_redux %>%
  group_by(type) %>%
  miss_var_summary() %>%
  filter(variable == "DOI") %>%
  arrange(pct_miss)

## ONLY JOURNAL ARTICLES (49% missing publication year!!!)
journals <- DGRLv17_5_zotero_redux %>% group_by(`Item Type`)%>%
  filter(`Item Type` == "journalArticle")
View(journals)
vis_miss(journals)

## Treat redux for NAs
## TITLE & ABSTRACTS 
abs_title_redux <- DGRLv17_5_zotero_redux %>% select (1, 2, 5, 7)
View(abs_title_redux)
vis_miss(abs_title_redux)

## REMOVE ABSTRACT NAs  
abs_title_lean <- na.omit(abs_title_redux)
View(abs_title_lean)
vis_miss(abs_title_lean)
## CHECK ITEM TYPE
item_types <- abs_title_lean %>% group_by(`Item Type`) %>% count(sort = TRUE) 
View(item_types)

######################
##DOI MISS --> Journal Article 33.4 // Conference paper 64.9%
######################

##Visualize NAs
vis_miss(doi_miss)
vis_miss(year_miss)

## Search for YEAR NAs using Zotero 
DGRLv17_5_zotero <- DGRLv17_5_zotero %>% rename(type = "Item Type", year= "Publication Year", abstract = "Abstract Note")
DGRLv17_5_zotero[, c(2,3)] <- lapply(DGRLv17_5_zotero[, c(2,3)], as.factor)
levels(DGRLv17_5_zotero$year)

year_miss <- doi_miss %>% drop_na(year)
vis_miss(year_miss)
write.csv(year_miss, "C:/Users/eagle/Dropbox/URBINO/DISSERTATION/03_STRUCTURE/01_Topic_Model/Digital Government Reference Library/EvSynth\\zotero_update.csv", row.names = TRUE)
write_bibliography(year_miss, "zotero_update.bib", format = "bib")

## Treatment of missing YEAR with Zotero AddOn "DOI Manager" and DOI search for metadata
zotero_updated_year <- read_csv("zotero_update_with_years.csv")
vis_miss(zotero_update_with_years)
zotero_update_with_years <- zotero_update_with_years %>% drop_na(year)

################ BOOK SECTION TREATMENT DO NOT USE ############
## Book section cleaning // Do not use ISBN is for whole book
book_section_search <- DGRLv17_5_zotero %>% filter(type == "bookSection")
book_section_search <- book_section_search %>% drop_na(ISBN)
View(book_section_search)
vis_miss(book_section_search)
book_section_search <- as.data.frame(book_section_search)

## Convert to RIS to process in Zotero ISBN // 
write.csv(book_section_search, "C:/Users/eagle/Dropbox/URBINO/DISSERTATION/03_STRUCTURE/01_Topic_Model/Digital Government Reference Library/EvSynth\\isbn_search.csv", row.names = TRUE)
as.data.frame(book_section_search)
write_bibliography(book_section_search, "isbn_search.bib", format = "bib")
View(book_section_search)
## Problematic step ISBN search catches the whole book, not bookSection


##########################
## Zotero after DOI search
##########################
write.csv(doi_miss, "C:/Users/eagle/Dropbox/URBINO/DISSERTATION/03_STRUCTURE/01_Topic_Model/Digital Government Reference Library/EvSynth\\doi_miss.csv", row.names = TRUE)
doi_miss2 <- read_csv("doi_miss2.csv")
vis_miss(doi_miss2)

## Missing Abstract Note in doi_miss2
doi_miss2 %>%
  group_by(type) %>%
  miss_var_summary() %>%
  filter(variable == "Abstract Note") %>%
  arrange(pct_miss)

doi_miss2 %>%
  group_by(type) %>% count()

### DROP MISSING ABSTRACTS
doi_miss2 <- doi_miss2 %>% drop_na("Abstract Note")
vis_miss(doi_miss2)

doi_miss2 %>% group_by(type) %>% count()
## 1 conferencePaper  1867 /// 2 journalArticle   5145

#############################
#### Topic Model for DGRL v 17.5 ==> 7012 DOCUMENTS
#############################


dgrl_tm <- doi_miss2 %>% select(2:4, 12) %>% rename(abstract = "Abstract Note")
View(dgrl_tm)
vis_miss(dgrl_tm)


## Documents by Year
dgrl_tm %>% group_by(year) %>% count(sort = TRUE) %>%
  ggplot(aes(year, n)) +
  geom_line()+
  xlab ("Year") +
  ylab ("Number of Documents")

#### BUILD A CORPUS QUANTEDA best practice: Corpus should remain unchanged during subsequent analysis and processing
dgrl_corpus <- corpus(dgrl_tm, text_field = "abstract")
print(dgrl_corpus)
ndoc(dgrl_corpus)
head(docvars(dgrl_corpus))
summary(dgrl_corpus, 5)
docvars(dgrl_corpus, field = "year")


## SUBSET CORPUS BY TYPE OF DOCUMENT (shall we slice by this criteria) ##
dgrl_journals <- corpus_subset(dgrl_corpus, type == "journalArticle")
view(dgrl_journals)
ndoc(dgrl_journals)

dgrl_conference <- corpus_subset(dgrl_corpus, type == "conferencePaper")
view(dgrl_conference)
ndoc(dgrl_conference)

## Dunno if step is helpful, year has been dropped. Maybe another method?

## TOKENIZATION 
dgrl_tokens <- tokens(dgrl_corpus, what = "word",
                     remove_punct = TRUE,
                     remove_symbols = TRUE,
                     remove_numbers = TRUE,
                     remove_url = TRUE)
print(dgrl_tokens)

## Check remove hyphens? --> Deprecated

## REMOVE STOPWORDS
dgrl_tokens <- tokens_select(dgrl_tokens, pattern = stopwords("en"), selection = "remove")
dgrl_tokens <- tokens_tolower(dgrl_tokens)
print(dgrl_tokens)



## KEYWORDS-IN-CONTEXT (theories sample test)
kw_npm <- kwic(dgrl_tokens, pattern = "npm")
head(kw_npm)
View(kw_npm)
kw_utaut <- kwic(dgrl_tokens, pattern = "utaut")
head(kw_utaut)
View(kw_utaut)
kw_theory <- kwic(dgrl_tokens, pattern = "theory")
head(kw_theory)
View(kw_theory)
kw_ql <- kwic(dgrl_tokens, pattern = "qualitat*")
View(kw_ql)
kw_qt <- kwic(dgrl_tokens, pattern = "quantitat*")
head(kw_qt)
View(kw_qt)
kw_emp <- kwic(dgrl_tokens, pattern = "empiric*")
head(kw_emp)
View(kw_emp)
kw_biblio <- kwic(dgrl_tokens, pattern = "biblio*")
View(kw_biblio)

## KWIC Phrases
kw_pv <- kwic(dgrl_tokens, pattern = phrase("public value"))
head(kw_pv)
View(kw_pv)
kw_new_pub_mgmt <- kwic(dgrl_tokens, pattern = phrase("new public management"))
head(kw_new_pub_mgmt)

### How to view a document in a corpus
dgrl_corpus[[7012]]

#### CREATE A DICTIONARY -- SENTENCE?
tech_labor_dict <- c("reduc*", "replac", "elimin", "save", "lower", "substitut", "autom")
work_string <- str_detect(dgrl_tokens, tech_labor_dict, negate = FALSE)
view(work_string)

#### CONSTRUCT A DOCUMENT-FEATURE MATRIX (DFM) ####
dfm_dgrl <- dfm(dgrl_tokens)
print(dfm_dgrl)
ndoc(dfm_dgrl)
nfeat(dfm_dgrl)
### TOP FEATURES IN DFM_DGRL -- Best practice remove very rare and very common
topfeatures(dfm_dgrl, 100)
### TFIDF Term Frequency-Inverse Document Frequency
dfm_dgrl_tfidf <- dfm_tfidf(dfm_dgrl)
print(dfm_dgrl_tfidf)

#### DIMENSIONALITY REDUCTION ####
## TRIM VERY RARE FEATURES Sparse matrix > 95% at 100 freq
dfm_dgrl_trim <- dfm_trim(dfm_dgrl, min_termfreq = 100)
print(dfm_dgrl_trim)

## TRIM VERY COMMON FEATURES More than 10% of documents
## If max_docfreq = 0.1, features that occur in more than 10% of the documents are removed.

dfm_dgrl_trim_hifreq <- dfm_trim(dfm_dgrl_trim, max_docfreq = 0.1, docfreq_type = "prop")
print(dfm_dgrl_trim_hifreq)
topfeatures(dfm_dgrl_trim_hifreq, 100)

#### FCM Feature Co-occurrence Matrix ####
fcm_dfm_dgrl_trim_hifreq <- fcm(dfm_dgrl_trim_hifreq)
dim(fcm_dfm_dgrl_trim_hifreq)
topfeatures(fcm_dfm_dgrl_trim_hifreq, 50)

#### Simple frequency analysis ####
dfm_dgrl %>% 
  textstat_frequency(n = 15) %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Frequency") +
  theme_minimal()

dfm_dgrl_trim_hifreq %>% 
  textstat_frequency(n = 15) %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Frequency") +
  theme_minimal()

## Wordclouds
set.seed(123)
textplot_wordcloud(dfm_dgrl_trim_hifreq, max_words = 100)
textplot_wordcloud(dfm_dgrl, max_words = 100)

#### MULTIPLE WORD CO-OCCURRENCE --> Montobbio et al 2022.
  






