## QUANTEDA WORKFLOW FOR DGRL17.5
library(rmarkdown)
library(knitr)
library(tidyverse)
library(tidytext)
library(reclin)
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
library(naniar)
library(visdat)
library(lubridate)
library(seededlda)
library(tm)
library(revtools)
library(caret)
library(here)
library(bib2df)

## Set project path to GitHub 
here()

#### DATA IMPORT ####
## MASTER FILES FROM ENDNOTE
mf_ris <- read.csv(here("data", "DGRL_Lit_Master_v17_ris.csv"))
mf_ris <- as_tibble(mf_ris)
mf_ris <- mf_ris %>%
  replace_with_na_all(condition = ~.x %in% common_na_strings)
print(mf_ris)
vis_miss(mf_ris, warn_large_data = FALSE)

mf_bib <- read.csv(here("data", "DGRL_Lit_Master_v17_bib.csv"))
mf_bib <- as_tibble(mf_bib)
mf_bib <- mf_bib %>%
  replace_with_na_all(condition = ~.x %in% common_na_strings)
View(mf_bib)
print(mf_bib)
vis_miss(mf_bib, warn_large_data = FALSE)


## Rename variables
mf_bib <- mf_bib %>% rename(type = `Item.Type`, year = `Publication.Year`, author = `Author`, doc_title = `Title`, pub_title = `Publication.Title`, abstract = `Abstract.Note`)
mf_ris <- mf_ris %>% rename(type = `Item.Type`, year = `Publication.Year`, author = `Author`, doc_title = `Title`, pub_title = `Publication.Title`, abstract = `Abstract.Note`)

## Keep variables of interest (Item Type", "Publication Year", "Author", "Title", "Publication Title", "Abstract Note")
mf_bib_redux <- mf_bib %>% select(2:6, 9, 11)
mf_ris_redux <- mf_ris %>% select(2:6, 9, 11)

## DOI as search key in both tables -> drop_na(DOI)
mf_bib_redux_doi <- mf_bib_redux %>% drop_na(DOI)
vis_miss(mf_bib_redux_doi)

mf_ris_redux_doi <- mf_ris_redux %>% drop_na(DOI)
vis_miss(mf_ris_redux_doi)

## Missing variable (year) by type bib 74% for conferencePaper  
mf_bib_redux_doi %>%
  group_by(type) %>%
  miss_var_summary() %>%
  filter(variable == "year") %>%
  arrange(desc(pct_miss))

mf_bib_redux_doi %>%
  group_by(type) %>%
  miss_var_summary() %>%
  filter(variable == "abstract") %>%
  arrange(desc(pct_miss))

mf_bib_redux_doi %>%
  group_by(type) %>%
  miss_var_summary() %>%
  filter(variable == "pub_title") %>%
  arrange(desc(pct_miss))

## Missing variable (year) in ris is a number -> to NA 42% missing
mf_ris_redux_doi <- mf_ris_redux_doi %>%
  replace_with_na(replace = list(year = c(1:12)))

print(mf_ris_redux_doi)
vis_miss(mf_ris_redux_doi)

## 55% missing year in journalArticle
mf_ris_redux_doi %>%
  group_by(type) %>%
  miss_var_summary() %>%
  filter(variable == "year") %>%
  arrange(desc(pct_miss))

mf_ris_redux_doi %>%
  group_by(type) %>%
  miss_var_summary() %>%
  filter(variable == "abstract") %>%
  arrange(desc(pct_miss))

## 49% missing pub_title in conferencePaper
mf_ris_redux_doi %>%
  group_by(type) %>%
  miss_var_summary() %>%
  filter(variable == "pub_title") %>%
  arrange(desc(pct_miss))

## FULL JOIN (CROSS JOIN) OF RIS AND BIS WITH DOI
fj1 <- full_join(mf_bib_redux_doi, mf_ris_redux_doi, by = "DOI") %>% 
  select(type.x, year.x, author.x, doc_title.x, pub_title.x, DOI, abstract.x) %>%
  drop_na(year.x)
fj2 <- full_join(mf_ris_redux_doi, mf_bib_redux_doi, by = "DOI") %>% 
  select(type.x, year.x, author.x, doc_title.x, pub_title.x, DOI, abstract.x) %>%
  drop_na(year.x)

bound_bib_ris <- bind_rows(fj1, fj2)
bound_bib_ris <- bound_bib_ris %>% distinct(DOI, .keep_all = TRUE)
bound_bib_ris %>% count(`type.x`) %>% arrange(desc(n))

bound_bib_ris %>%
  group_by(type.x) %>%
  miss_var_summary() %>%
  filter(variable == "pub_title.x") %>%
  arrange(desc(pct_miss)) 

## KEEP NAs in RIS_REDUX_DOI TO SEARCH FOR DOI MATCH
year_na_ris <- mf_ris_redux_doi[is.na(mf_ris_redux_doi$year),]
print(year_na_ris)

## full_join
search_ris <- full_join(mf_bib_redux_doi, year_na_ris, by = "DOI") %>%
  select(type.x, year.x, author.x, doc_title.x, pub_title.x, DOI, abstract.x) %>%
  drop_na(year.x)
print(search_ris)
vis_miss(search_ris)

## Replicate for BIB_REDUX_DOI TO SEARCH FOR DOI MATCH
year_na_bib <- mf_bib_redux_doi[is.na(mf_bib_redux_doi$year),]
print(year_na_bib)
vis_miss(year_na_bib)

## full_join
search_bib <- full_join(mf_ris_redux_doi, year_na_bib, by = "DOI") %>%
  select(type.x, year.x, author.x, doc_title.x, pub_title.x, DOI, abstract.x) %>%
  drop_na(year.x)
print(search_bib)
vis_miss(search_bib)

## MERGE BY DOI
joint <- bind_rows(search_bib, search_ris)
joint <- joint %>% distinct(DOI, .keep_all = TRUE)
print(joint)
vis_miss(joint)


## RIS FILE ##
DGRLv17_5_RIS <- read_csv(here("data", "DGRLv17.5_RIS.csv"))
View(DGRLv17_5_RIS)

## View missing information
vis_miss(DGRLv17_5_RIS, warn_large_data = FALSE)
pct_miss(DGRLv17_5_RIS)
## 80.7% missing information... 

## BIB FILE ##
DGRLv17_5_BIB <- read.csv(here("data", "DGRLv17.5_BIB.csv"))
DGRLv17_5_BIB <- as_tibble(DGRLv17_5_BIB)
View(DGRLv17_5_BIB)
vis_miss(DGRLv17_5_BIB, warn_large_data = FALSE)
pct_miss(DGRLv17_5_BIB)
## 70.5% missing information...


## Rename Variables of Interest
DGRLv17_5_RIS <- DGRLv17_5_RIS %>% rename(id = `Key`, type = `Item Type`, year = `Publication Year`, author = `Author`, doc_title = `Title`, pub_title = `Publication Title`, abstract = `Abstract Note`)
DGRLv17_5_BIB <- DGRLv17_5_BIB %>% rename(id = `ï..Key`, type = `Item.Type`, year = `Publication.Year`, author = `Author`, doc_title = `Title`, pub_title = `Publication.Title`, abstract = `Abstract.Note`)

## Dimensionality reduction of RIS and BIB
names(DGRLv17_5_RIS)
names(DGRLv17_5_BIB)

DGRLv17_5_RIS_redux <- DGRLv17_5_RIS %>% select(1:6, 9, 11)
print(DGRLv17_5_RIS_redux)
View(DGRLv17_5_RIS_redux)
vis_miss(DGRLv17_5_RIS_redux)

## In BIB data set "" not recognized as NA
DGRLv17_5_BIB_redux <- DGRLv17_5_BIB %>% select(1:6, 9, 11)
print(DGRLv17_5_BIB_redux)
View(DGRLv17_5_BIB_redux)
vis_miss(DGRLv17_5_BIB_redux)

## Treat BIB file convert columns to match RIS
DGRLv17_5_BIB_redux <-  DGRLv17_5_BIB_redux %>%
  mutate(year = as.double(year),
         abstract = as.character(abstract))

## Replace "" as NA
DGRLv17_5_BIB_redux <- DGRLv17_5_BIB_redux %>%
  replace_with_na_all(condition = ~.x %in% common_na_strings)



by_type_RIS <- DGRLv17_5_RIS %>% count(`type`) %>% arrange(desc(n))
by_type_RIS

by_type_BIB <- DGRLv17_5_BIB %>% count(`type`) %>% arrange(desc(n))
by_type_BIB


## Exploring missing values
with_doi_RIS <- DGRLv17_5_RIS_redux %>% drop_na(DOI)
vis_miss(with_doi_RIS)
write.csv(with_doi_RIS, "~/GitHub/topic_model/data\\with_doi_RIS.csv", row.names = TRUE)

## BIB 18.6% missing year
with_doi_BIB <- DGRLv17_5_BIB_redux %>% drop_na(DOI)
vis_miss(with_doi_BIB)

with_year <- DGRLv17_5_RIS_redux %>% drop_na(year)
vis_miss(with_year)

## Missing years in BIB
with_doi_BIB %>%
  group_by(type) %>%
  miss_var_summary() %>%
  filter(variable == "year") %>%
  arrange(desc(pct_miss))

## Missing years in RIS
with_doi_RIS %>%
  group_by(type) %>%
  miss_var_summary() %>%
  filter(variable == "year") %>%
  arrange(desc(pct_miss))

with_doi %>%
  group_by(type) %>%
  miss_var_summary() %>%
  filter(variable == "pub_title") %>%
  arrange(desc(pct_miss)) 

## Subset of Journal Articles
articles_RIS <- DGRLv17_5_RIS_redux %>% group_by(`type`)%>%
  filter(`type` == "journalArticle")
vis_miss(articles_RIS)

articles_BIB <- DGRLv17_5_BIB_redux %>% group_by(`type`)%>%
  filter(`type` == "journalArticle")
vis_miss(articles_BIB)

## Top Journals
top_articles <- articles %>% count(`pub_title`) %>% arrange(desc(n))
top_articles

## Subset of Conference Papers
papers <- DGRLv17_5_RIS_redux %>% group_by(`type`) %>%
  filter(`type` == "conferencePaper")
vis_miss(papers)

# Top Conferences
top_papers <- papers %>% count(`pub_title`) %>% arrange(desc(n))
top_papers

## Search Zotero DOI Manager


DGRLv17_5_RIS_redux %>%
  group_by(type) %>%
  miss_var_summary() %>%
  filter(variable == "abstract") %>%
  arrange(pct_miss)

DGRLv17_5_RIS_redux %>%
  group_by(type) %>%
  miss_var_summary() %>%
  filter(variable == "DOI") %>%
  arrange(pct_miss)

DGRLv17_5_RIS_redux %>%
  group_by(type) %>%
  miss_var_summary() %>%
  filter(variable == "year") %>%
  arrange(pct_miss)
  

##YEAR MISS --> Journal Article 49.8% // Conference paper 0.3%


DGRLv17_5_RIS_redux %>%
  group_by(type) %>%
  miss_var_summary() %>%
  filter(variable == "year") %>%
  arrange(pct_miss)

## Missing information (49% missing publication year!!!)
articles <- DGRLv17_5_RIS_redux %>% group_by(`type`)%>%
  filter(`type` == "journalArticle")
vis_miss(articles)

## Missing information (21% of abstracts)
papers <- DGRLv17_5_RIS_redux %>% group_by(`type`) %>%
  filter(`type` == "conferencePaper")
vis_miss(papers)

## Treat redux for NAs
## TITLE & ABSTRACTS 
abs_title_redux <- DGRLv17_5_RIS_redux %>% select (1, 2, 5, 7)
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
DGRLv17_5_RIS <- DGRLv17_5_RIS %>% rename(type = "Item Type", year= "Publication Year", abstract = "Abstract Note")
DGRLv17_5_RIS[, c(2,3)] <- lapply(DGRLv17_5_RIS[, c(2,3)], as.factor)
levels(DGRLv17_5_RIS$year)

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
book_section_search <- DGRLv17_5_RIS %>% filter(type == "bookSection")
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
  






