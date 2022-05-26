#### Structural Topic Model ####
library(stm)
library(stminsights)

## CONVERT FROM QUANTEDA TO STM
quant2stm <- convert(train_dgrl175, to = "stm")

## Running of searchK() with different values for k
findingK <- searchK(quant2stm$documents, 
                    quant2stm$vocab, 
                    K = c(10, 30, 50, 80, 100),
                    prevalence =~ year.x, 
                    data = quant2stm$meta, 
                    init.type = "Spectral",
                    verbose=FALSE)


plot(findingK)

finding_smallK <- searchK(quant2stm$documents, 
                    quant2stm$vocab, 
                    K = c(25:50),
                    prevalence =~ year.x, 
                    data = quant2stm$meta, 
                    init.type = "Spectral",
                    verbose=FALSE)

## Potential at k = 33, k = 45, k = 47
plot(finding_smallK)

## CALCULATE STM k = 33 ##
dgrl_stm33 <- stm(quant2stm$documents, 
                  quant2stm$vocab, 
                  K = 33,
                  prevalence = ~ year.x,
                  max.em.its = 75,
                  data = quant2stm$meta, 
                  init.type = "Spectral")

## PRINT WORDS PER TOPIC
data.frame(t(labelTopics(dgrl_stm33, n = 10)$prob))

## SHARE OF TOPICS OVER ALL CORPUS ##
plot(
  dgrl_stm33,
  type = "summary",
  text.cex = 0.5,
  main = "STM topic shares",
  xlab = "Share estimation")

## CALCULATE STM k = 45 ##
dgrl_stm45 <- stm(quant2stm$documents, 
                  quant2stm$vocab, 
                  K = 45,
                  prevalence = ~ year.x,
                  max.em.its = 75,
                  data = quant2stm$meta, 
                  init.type = "Spectral")

data.frame(t(labelTopics(dgrl_stm45, n = 10)$prob))

## SHARE OF TOPICS OVER ALL CORPUS ##
plot(
  dgrl_stm45,
  type = "summary",
  text.cex = 0.5,
  main = "STM topic shares",
  xlab = "Share estimation")

## CALCULATE STM k = 47 ##
dgrl_stm47 <- stm(quant2stm$documents, 
                  quant2stm$vocab, 
                  K = 47,
                  prevalence = ~ year.x,
                  max.em.its = 75,
                  data = quant2stm$meta, 
                  init.type = "Spectral")

## PRINT WORDS PER TOPIC
data.frame(t(labelTopics(dgrl_stm47, n = 10)$prob))

plot(
  dgrl_stm47,
  type = "summary",
  text.cex = 0.5,
  main = "STM topic shares",
  xlab = "Share estimation")

## estimateEffect k = 33, 45, 47
fx_33 <- estimateEffect(1:33 ~ year.x, dgrl_stm33, meta = quant2stm$meta)


## Highest Prob / FREX / LIFT / Score
train33_labels <- labelTopics(dgrl_stm33, n = 10)
train33_labels

train47_labels <- labelTopics(dgrl_stm47, n = 10)
train47_labels



## FindThoughts



## WORDCLOUD OF MOST PREVALENT TOPIC review usefulness
stm::cloud(dgrl_stm47,
           topic = 20,
           scale = c(2.25, .5))

## EYEBALLING TOPICS 
plot(dgrl_stm50, type = "perspectives", topics = c(30,46)) 

## DOCUMENT TOPIC PROPORTIONS
plot(dgrl_stm50, type = "hist", topics = sample(1:50, size = 9))

## LDAvis k= 12 ##
toLDAvis(dgrl_stm12,
         quant2stm$documents,
         R = 30,
         plot.opts = list(xlab = "PC1", ylab = "PC2"),
         lambda.step = 0.1,
         out.dir = tempfile(),
         open.browser = interactive(),
         as.gist = FALSE,
         reorder.topics = TRUE)

## LDAvis k=25 ##
toLDAvis(dgrl_stm25,
         quant2stm$documents,
         R = 30,
         plot.opts = list(xlab = "PC1", ylab = "PC2"),
         lambda.step = 0.1,
         out.dir = tempfile(),
         open.browser = interactive(),
         as.gist = FALSE,
         reorder.topics = TRUE)

## LDAvis k=50 ##
toLDAvis(dgrl_stm50,
         quant2stm$documents,
         R = 30,
         plot.opts = list(xlab = "PC1", ylab = "PC2"),
         lambda.step = 0.1,
         out.dir = tempfile(),
         open.browser = interactive(),
         as.gist = FALSE,
         reorder.topics = TRUE)

## LDAvis k=75 ##
toLDAvis(dgrl_stm75,
         quant2stm$documents,
         R = 30,
         plot.opts = list(xlab = "PC1", ylab = "PC2"),
         lambda.step = 0.1,
         out.dir = tempfile(),
         open.browser = interactive(),
         as.gist = FALSE,
         reorder.topics = TRUE)

## LDAvis k = 100 ##
toLDAvis(dgrl_stm100,
         quant2stm$documents,
         R = 30,
         plot.opts = list(xlab = "PC1", ylab = "PC2"),
         lambda.step = 0.1,
         out.dir = tempfile(),
         open.browser = interactive(),
         as.gist = FALSE,
         reorder.topics = TRUE)

