#### Structural Topic Model ####
library(stm)
library(dplyr)
library(broom)
library(tidytext)
library(LDAvis)
library(stminsights)

## CONVERT FROM QUANTEDA TO STM
train_stm <- convert(train_dgrl175, to = "stm")

train_stm$meta$period <- as.numeric(train_stm$meta$period)
train_stm$meta$year.x <- as.numeric(train_stm$meta$year.x)

out <- list(documents = train_stm$documents,
            vocab = train_stm$vocab,
            meta = train_stm$meta)

str(train_stm)

#### searchK() with different values for k
## Visualization of Goodness of Fit Measures 

## SearchK for small K
find_smallestK <- searchK(train_stm$documents, 
                          train_stm$vocab, 
                          K = c(5:25),
                          prevalence = ~ year.x, 
                          data = train_stm$meta, 
                          init.type = "Spectral",
                          verbose=FALSE)

plot(find_smallestK)

## Medium K
find_mediumK <- searchK(train_stm$documents, 
                        train_stm$vocab, 
                        K = c(25:50),
                        prevalence = ~ year.x, 
                        data = train_stm$meta, 
                        init.type = "Spectral",
                        verbose=FALSE)

plot(find_mediumK)

## Larger K
findingK <- searchK(train_stm$documents, 
                    train_stm$vocab, 
                    K = c(50:75),
                    prevalence = ~year.x, 
                    data = train_stm$meta, 
                    init.type = "Spectral",
                    verbose=FALSE)

plot(findingK)

## Between 75 ~ 100
find_needle <- searchK(train_stm$documents, 
                          train_stm$vocab, 
                          K = c(75:100),
                          prevalence = ~ year.x, 
                          data = train_stm$meta, 
                          init.type = "Spectral",
                          verbose=FALSE)

plot(find_needle)

## Find best K
find_needle2 <- searchK(train_stm$documents, 
                       train_stm$vocab, 
                       K = c(100:125),
                       prevalence = ~ year.x, 
                       data = train_stm$meta, 
                       init.type = "Spectral",
                       verbose=FALSE)

plot(find_needle2)

### Potential options for K (S/M/L) => 22, 44, 53, 80

#### CALCULATE STM k = 29 ####
dgrl_stm22 <- stm(train_stm$documents, 
                  train_stm$vocab, 
                  K = 22,
                  prevalence = ~ year.x,
                  max.em.its = 75,
                  data = train_stm$meta, 
                  init.type = "Spectral")

## PRINT WORDS PER TOPIC
data.frame(t(labelTopics(dgrl_stm22, n = 10)$prob))

train22_labels <- labelTopics(dgrl_stm22, n = 10)
train22_labels

## ESTIMATE EFFECT
fx_22 <- estimateEffect(1:22 ~ year.x, 
                        dgrl_stm22, 
                        meta = out$meta, 
                        uncertainty = "Global")

## VISUALIZE TOPIC PREVALENCE

## Topic 7 Covid-19
plot(fx_22, "year.x", method = "continuous", topics = 7,
     model = z, printlegend = FALSE, xaxt = "n", xlab = "Year")
axis(1,at=c(2000,2005,2010,2015,2020),
     labels=c(2000,2005,2010,2015,2020),las=2)

## Topic 11 Elections 
plot(fx_22, "year.x", method = "continuous", topics = 11,
     model = z, printlegend = FALSE, xaxt = "n", xlab = "Year")
axis(1,at=c(2000,2005,2010,2015,2020),
     labels=c(2000,2005,2010,2015,2020),las=2)

## Topic 4 AI
plot(fx_22, "year.x", method = "continuous", topics = 4,
     model = z, printlegend = FALSE, xaxt = "n", xlab = "Year")
axis(1,at=c(2000,2005,2010,2015,2020),
     labels=c(2000,2005,2010,2015,2020),las=2)


## Topic Prevalence over Time ##
par(mfrow=c(3,3))
for (i in seq_along(sample(1:22, size = 9)))
{
  plot(fx_22, "year.x", method = "continuous", topics = i, main = paste0(train22_labels$prob[i,1:3], collapse = ", "), printlegend = F)
}

## SHARE OF TOPICS OVER ALL CORPUS ##
plot(
  dgrl_stm22,
  type = "summary",
  text.cex = 0.5,
  main = "STM topic shares",
  xlab = "Share estimation") 

#### CALCULATE STM k = 44 ####
dgrl_stm44 <- stm(train_stm$documents, 
                  train_stm$vocab, 
                  K = 44,
                  prevalence = ~ year.x,
                  max.em.its = 75,
                  data = train_stm$meta, 
                  init.type = "Spectral")

## PRINT WORDS PER TOPIC
data.frame(t(labelTopics(dgrl_stm44, n = 10)$prob))

train44_labels <- labelTopics(dgrl_stm44, n = 10)
train44_labels

fx_44 <- estimateEffect(1:44 ~ year.x, 
                       dgrl_stm44, 
                       meta = out$meta, 
                       uncertainty = "Global")

## Topic 10 Cybersecurity
plot(fx_44, "year.x", method = "continuous", topics = 10,
     model = z, printlegend = FALSE, xaxt = "n", xlab = "Year")
axis(1,at=c(2000,2005,2010,2015,2020),
     labels=c(2000,2005,2010,2015,2020),las=2)

## Topic 43 Covid-19
plot(fx_44, "year.x", method = "continuous", topics = 43,
     model = z, printlegend = FALSE, xaxt = "n", xlab = "Year")
axis(1,at=c(2000,2005,2010,2015,2020),
     labels=c(2000,2005,2010,2015,2020),las=2)

## Topic 8 Cloud
plot(fx_44, "year.x", method = "continuous", topics = 8,
     model = z, printlegend = FALSE, xaxt = "n", xlab = "Year")
axis(1,at=c(2000,2005,2010,2015,2020),
     labels=c(2000,2005,2010,2015,2020),las=2)


## Topic Prevalence over Time ##
par(mfrow=c(3,3))
for (i in seq_along(sample(1:44, size = 9)))
{
  plot(fx_44, "year.x", method = "continuous", topics = i, main = paste0(train44_labels$prob[i,1:3], collapse = ", "), printlegend = F)
}

## SHARE OF TOPICS OVER ALL CORPUS ##
plot(
  dgrl_stm44,
  type = "summary",
  text.cex = 0.5,
  main = "STM topic shares",
  xlab = "Share estimation")


#### CALCULATE STM k = 53 ####
dgrl_stm53 <- stm(train_stm$documents, 
                  train_stm$vocab, 
                  K = 53,
                  prevalence = ~ year.x,
                  max.em.its = 75,
                  data = train_stm$meta, 
                  init.type = "Spectral")

## SHARE OF TOPICS OVER ALL CORPUS ##
plot(
  dgrl_stm53,
  type = "summary",
  text.cex = 0.8,
  main = "Estimated Topic Proportions Training Set",
  xlab = "Share estimation")

## PRINT WORDS PER TOPIC
data.frame(t(labelTopics(dgrl_stm53, n = 10)$prob))

train53_labels <- labelTopics(dgrl_stm53, n = 10)
train53_labels

## Top 3 Topics
top5_train53 <- labelTopics(dgrl_stm53, c(47,27,51,45,23))
top5_train53

plot(dgrl_stm53,
     main = "Most Prevalent Topics and Words by Score Measure",
     type="labels",
     labeltype = "score",
     topics=c(47,27,51,45,23))

## Sage Labels?
print(sageLabels(dgrl_stm53))

fx_53 <- estimateEffect(1:53 ~ year.x, 
                        dgrl_stm53, 
                        meta = out$meta, 
                        uncertainty = "Global")

## Topic 6 Mobile Phone
plot(fx_53, "year.x", method = "continuous", topics = 6,
     model = z, printlegend = FALSE, xaxt = "n", xlab = "Year")
axis(1,at=c(2000,2005,2010,2015,2020),
     labels=c(2000,2005,2010,2015,2020),las=2)

## Topic 38 Web
plot(fx_53, "year.x", method = "continuous", topics = 38,
     model = z, printlegend = FALSE, xaxt = "n", xlab = "Year")
axis(1,at=c(2000,2005,2010,2015,2020),
     labels=c(2000,2005,2010,2015,2020),las=2)

## Topic 9 SM and Elections
plot(fx_53, "year.x", method = "continuous", topics = 25,
     model = z, printlegend = FALSE, xaxt = "n", xlab = "Year")
axis(1,at=c(2000,2005,2010,2015,2020),
     labels=c(2000,2005,2010,2015,2020),las=2)


## Topic Prevalence over Time ##
par(mfrow=c(1,1))
for (i in seq_along(sample(1:53, size = 53)))
{
  plot(fx_53, "year.x", method = "continuous", topics = i, main = paste0(train53_labels$prob[i,1:3], collapse = ", "), printlegend = T)
}

## Tweak Plot ??##
## Topic Prevalence over Time ##
par(mfrow=c(1,1))
for (i in seq_along(sample(1:53, size = 53)))
{
  plot(fx_53, "year.x", method = "continuous", topics = c(4,8,26), main = paste0(train53_labels$prob[i,1:3], collapse = ", "), printlegend = T)
}

## Topic Quality


#### TIDY APPROACH TO GRAPHICS Under exploration #### 

# tidy the word-topic combinations
td_beta21 <- tidy(dgrl_stm21)
td_beta21

# Examine the topics
td_beta21 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  ggplot(aes(term, beta)) +
  geom_col() +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# tidy the document-topic combinations, with optional document names
td_gamma21 <- tidy(dgrl_stm21, matrix = "gamma",
                 document_names = rownames(train_stm))
td_gamma21


# tidy theta
td_theta21 <- tidy(dgrl_stm21, matrix = "theta",
                   document_names = rownames(train_stm))

td_theta21


#### FindThoughts ####



## WORDCLOUD OF MOST PREVALENT TOPIC review usefulness
stm::cloud(dgrl_stm53,
           topic = 6,
           scale = c(3.25, .95))

## EYEBALLING TOPICS 
plot(dgrl_stm17, type = "perspectives", topics = c(6, 12)) 

## DOCUMENT TOPIC PROPORTIONS
plot(dgrl_stm53, type = "hist", topics = sample(1:53, size = 9))

#### LDAvis k=53 ####
toLDAvis(dgrl_stm53,
         train_stm$documents,
         R = 25,
         plot.opts = list(xlab = "PC1", ylab = "PC2"),
         lambda.step = 0.1,
         out.dir = tempfile(),
         open.browser = interactive(),
         as.gist = FALSE,
         reorder.topics = TRUE)



