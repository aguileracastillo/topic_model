############################# 
## DGRLv17.5 DATA IMPORT AND INITIAL CLEANING
#############################
## RUN PACKAGES
library(tidyverse)
library(here)
library(magrittr)
library(naniar)

## IMPORT MASTER FILES FROM ENDNOTE LIBRARY
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

## RENAME VARIABLES OF INTEREST
mf_bib <- mf_bib %>% rename(type = `Item.Type`, year = `Publication.Year`, author = `Author`, doc_title = `Title`, pub_title = `Publication.Title`, abstract = `Abstract.Note`)
mf_ris <- mf_ris %>% rename(type = `Item.Type`, year = `Publication.Year`, author = `Author`, doc_title = `Title`, pub_title = `Publication.Title`, abstract = `Abstract.Note`)

## KEEP VARIABLES OF INTEREST (Item Type", "Publication Year", "Author", "Title", "Publication Title", "Abstract Note")
mf_bib_redux <- mf_bib %>% select(2:6, 9, 11)
mf_ris_redux <- mf_ris %>% select(2:6, 9, 11)

## KEEP DOI IN BOTH RIS AND BIB IMPORTS -> drop_na(DOI)
## 19.5 % missing year
mf_bib_redux_doi <- mf_bib_redux %>% drop_na(DOI)
vis_miss(mf_bib_redux_doi)

## 11.7% missing publication title
mf_ris_redux_doi <- mf_ris_redux %>% drop_na(DOI)
vis_miss(mf_ris_redux_doi)

### A deeper examination of both data sets showed different missing variables
## BIB data set: Missing variable (year) 74% for conferencePaper  
mf_bib_redux_doi %>%
  group_by(type) %>%
  miss_var_summary() %>%
  filter(variable == "year") %>%
  arrange(desc(pct_miss))

## Abstracts were missing for 3.17 of conference papers and 1.16% of journal articles
mf_bib_redux_doi %>%
  group_by(type) %>%
  miss_var_summary() %>%
  filter(variable == "abstract") %>%
  arrange(desc(pct_miss))

## Negligible missing values of publication title
mf_bib_redux_doi %>%
  group_by(type) %>%
  miss_var_summary() %>%
  filter(variable == "pub_title") %>%
  arrange(desc(pct_miss))

## RIS data set: Missing variable (year) 42% missing
mf_ris_redux_doi <- mf_ris_redux_doi %>%
  replace_with_na(replace = list(year = c(1:12)))
print(mf_ris_redux_doi)
vis_miss(mf_ris_redux_doi)

## 55.2% missing year in journalArticle
mf_ris_redux_doi %>%
  group_by(type) %>%
  miss_var_summary() %>%
  filter(variable == "year") %>%
  arrange(desc(pct_miss))

## Around 5% missing abstracts for journalArticles and 3.5% for conferencePapers
mf_ris_redux_doi %>%
  group_by(type) %>%
  miss_var_summary() %>%
  filter(variable == "abstract") %>%
  arrange(desc(pct_miss))

## 49.5% or 1087 missing pub_title in conferencePaper
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
vis_miss(bound_bib_ris)

## DATA TO BE USED IN TOPIC MODEL
to_corpus <- bound_bib_ris %>% drop_na()
to_corpus %>% count(type.x)
vis_miss(to_corpus)
write.csv(to_corpus, "~/GitHub/topic_model/data\\to_corpus.csv", row.names = TRUE)

## RESULT: to_corpus data set contains abstracts of 6682  journal articles and 1079 conference papers 
## containing relevant variables to run the topic model.
