#### Structural Topic Model ####
library(stm)
library(dplyr)
library(broom)
library(tidytext)
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
findingK <- searchK(train_stm$documents, 
                    train_stm$vocab, 
                    K = c(10, 30, 50, 80, 100),
                    prevalence = ~year.x, 
                    data = train_stm$meta, 
                    init.type = "Spectral",
                    verbose=FALSE)

## Visualization of Goodness of Fit 
plot(findingK)

## SearchK for small K

find_smallestK <- searchK(train_stm$documents, 
                          train_stm$vocab, 
                          K = c(5:25),
                          prevalence = ~ year.x, 
                          data = train_stm$meta, 
                          init.type = "Spectral",
                          verbose=FALSE)

plot(find_smallestK)

find_mediumK <- searchK(train_stm$documents, 
                          train_stm$vocab, 
                          K = c(25:50),
                          prevalence = ~ year.x, 
                          data = train_stm$meta, 
                          init.type = "Spectral",
                          verbose=FALSE)

plot(find_mediumK)

### Write potential options for K (S/M/L)

## CALCULATE STM k = 22 ##
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

## VISUALIZE MODEL

plot(fx_22, "year.x", method = "continuous", topics = 7,
     model = z, printlegend = FALSE, xaxt = "n", xlab = "Year")
axis(1,at=c(2000,2005,2010,2015,2020),
     labels=c(2000,2005,2010,2015,2020),las=2)

plot(fx_22, "year.x", method = "continuous", topics = 11,
     model = z, printlegend = FALSE, xaxt = "n", xlab = "Year")
axis(1,at=c(2000,2005,2010,2015,2020),
     labels=c(2000,2005,2010,2015,2020),las=2)

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

### MEDIUM SIZE K

## CALCULATE STM k = 46 ##
dgrl_stm46 <- stm(train_stm$documents, 
                  train_stm$vocab, 
                  K = 46,
                  prevalence = ~ year.x,
                  max.em.its = 75,
                  data = train_stm$meta, 
                  init.type = "Spectral")

## PRINT WORDS PER TOPIC
data.frame(t(labelTopics(dgrl_stm46, n = 10)$prob))

train21_labels <- labelTopics(dgrl_stm21, n = 10)
train21_labels

fx_21 <- estimateEffect(1:21 ~ year.x, 
                       dgrl_stm21, 
                       meta = out$meta, 
                       uncertainty = "Global")


plot(fx_21, "year.x", method = "continuous", topics = 7,
     model = z, printlegend = FALSE, xaxt = "n", xlab = "Year")
axis(1,at=c(2000,2005,2010,2015,2020),
     labels=c(2000,2005,2010,2015,2020),las=2)

plot(fx_21, "year.x", method = "continuous", topics = 11,
     model = z, printlegend = FALSE, xaxt = "n", xlab = "Year")
axis(1,at=c(2000,2005,2010,2015,2020),
     labels=c(2000,2005,2010,2015,2020),las=2)

plot(fx_21, "year.x", method = "continuous", topics = 4,
     model = z, printlegend = FALSE, xaxt = "n", xlab = "Year")
axis(1,at=c(2000,2005,2010,2015,2020),
     labels=c(2000,2005,2010,2015,2020),las=2)


## Topic Prevalence over Time ##
par(mfrow=c(3,3))
for (i in seq_along(sample(1:21, size = 9)))
{
  plot(fx_21, "year.x", method = "continuous", topics = i, main = paste0(train21_labels$prob[i,1:3], collapse = ", "), printlegend = F)
}

## SHARE OF TOPICS OVER ALL CORPUS ##
plot(
  dgrl_stm21,
  type = "summary",
  text.cex = 0.5,
  main = "STM topic shares",
  xlab = "Share estimation")

## TIDY APPROACH TO GRAPHICS ##

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

## Search for medium K
finding_smallK <- searchK(train_stm$documents, 
                    train_stm$vocab, 
                    K = c(25:50),
                    prevalence =~ year.x, 
                    data = train_stm$meta, 
                    init.type = "Spectral",
                    verbose=FALSE)

## Potential at k = 35, k = 44, k = 49
plot(finding_smallK)

## CALCULATE STM k = 35 ##
dgrl_stm35 <- stm(train_stm$documents, 
                  train_stm$vocab, 
                  K = 35,
                  prevalence = ~ year.x,
                  max.em.its = 75,
                  data = train_stm$meta, 
                  init.type = "Spectral")

## PRINT WORDS PER TOPIC
data.frame(t(labelTopics(dgrl_stm35, n = 10)$prob))

## SHARE OF TOPICS OVER ALL CORPUS ##
plot(
  dgrl_stm35,
  type = "summary",
  text.cex = 0.5,
  main = "STM topic shares",
  xlab = "Share estimation")

## CALCULATE STM k = 44 ##
dgrl_stm44 <- stm(train_stm$documents, 
                  train_stm$vocab, 
                  K = 44,
                  prevalence = ~ year.x,
                  max.em.its = 75,
                  data = train_stm$meta, 
                  init.type = "Spectral")

data.frame(t(labelTopics(dgrl_stm44, n = 10)$prob))

## SHARE OF TOPICS OVER ALL CORPUS ##
plot(
  dgrl_stm44,
  type = "summary",
  text.cex = 0.5,
  main = "STM topic shares",
  xlab = "Share estimation")

## CALCULATE STM k = 49 ##
dgrl_stm49 <- stm(train_stm$documents, 
                  train_stm$vocab, 
                  K = 49,
                  prevalence = ~ year.x,
                  max.em.its = 75,
                  data = train_stm$meta, 
                  init.type = "Spectral")

## PRINT WORDS PER TOPIC
data.frame(t(labelTopics(dgrl_stm49, n = 10)$prob))

plot(
  dgrl_stm49,
  type = "summary",
  text.cex = 0.5,
  main = "STM topic shares",
  xlab = "Share estimation")


## Highest Prob / FREX / LIFT / Score
train17_labels <- labelTopics(dgrl_stm17, n = 10)
train17_labels

train44_labels <- labelTopics(dgrl_stm44, n = 10)
train44_labels

## estimateEffect k = 35, 45, 47
fx_35 <- estimateEffect(1:35 ~ year.x, dgrl_stm35, meta = train_stm$meta)


## FindThoughts



## WORDCLOUD OF MOST PREVALENT TOPIC review usefulness
stm::cloud(dgrl_stm47,
           topic = 20,
           scale = c(2.25, .5))

## EYEBALLING TOPICS 
plot(dgrl_stm17, type = "perspectives", topics = c(6, 12)) 

## DOCUMENT TOPIC PROPORTIONS
plot(dgrl_stm50, type = "hist", topics = sample(1:50, size = 9))

## LDAvis k=17 ##
toLDAvis(dgrl_stm17,
         train_stm$documents,
         R = 30,
         plot.opts = list(xlab = "PC1", ylab = "PC2"),
         lambda.step = 0.1,
         out.dir = tempfile(),
         open.browser = interactive(),
         as.gist = FALSE,
         reorder.topics = TRUE)

## LDAvis k = 49 ##
toLDAvis(dgrl_stm49,
         train_stm$documents,
         R = 30,
         plot.opts = list(xlab = "PC1", ylab = "PC2"),
         lambda.step = 0.1,
         out.dir = tempfile(),
         open.browser = interactive(),
         as.gist = FALSE,
         reorder.topics = TRUE)

## LDAvis k=75 ##
toLDAvis(dgrl_stm24,
         train_stm$documents,
         R = 30,
         plot.opts = list(xlab = "PC1", ylab = "PC2"),
         lambda.step = 0.1,
         out.dir = tempfile(),
         open.browser = interactive(),
         as.gist = FALSE,
         reorder.topics = TRUE)

## LDAvis k = 50 ##
toLDAvis(dgrl_stm50,
         train_stm$documents,
         R = 30,
         plot.opts = list(xlab = "PC1", ylab = "PC2"),
         lambda.step = 0.1,
         out.dir = tempfile(),
         open.browser = interactive(),
         as.gist = FALSE,
         reorder.topics = TRUE)

