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

## Visualization of Goodness of Fit 
plot(findingK)

## SearchK for small K

find_smallestK <- searchK(quant2stm$documents, 
                          quant2stm$vocab, 
                          K = c(5:25),
                          prevalence =~ year.x, 
                          data = quant2stm$meta, 
                          init.type = "Spectral",
                          verbose=FALSE)

plot(find_smallestK)

## Potential at k = 24

## CALCULATE STM k = 24 ##
dgrl_stm24 <- stm(quant2stm$documents, 
                  quant2stm$vocab, 
                  K = 24,
                  prevalence = ~ year.x,
                  max.em.its = 75,
                  data = quant2stm$meta, 
                  init.type = "Spectral")

## PRINT WORDS PER TOPIC
data.frame(t(labelTopics(dgrl_stm24, n = 10)$prob))

## Search for medium K
finding_smallK <- searchK(quant2stm$documents, 
                    quant2stm$vocab, 
                    K = c(25:50),
                    prevalence =~ year.x, 
                    data = quant2stm$meta, 
                    init.type = "Spectral",
                    verbose=FALSE)

## Potential at k = 35, k = 44, k = 49
plot(finding_smallK)

## CALCULATE STM k = 35 ##
dgrl_stm35 <- stm(quant2stm$documents, 
                  quant2stm$vocab, 
                  K = 35,
                  prevalence = ~ year.x,
                  max.em.its = 75,
                  data = quant2stm$meta, 
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
dgrl_stm44 <- stm(quant2stm$documents, 
                  quant2stm$vocab, 
                  K = 44,
                  prevalence = ~ year.x,
                  max.em.its = 75,
                  data = quant2stm$meta, 
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
dgrl_stm49 <- stm(quant2stm$documents, 
                  quant2stm$vocab, 
                  K = 49,
                  prevalence = ~ year.x,
                  max.em.its = 75,
                  data = quant2stm$meta, 
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
train35_labels <- labelTopics(dgrl_stm35, n = 10)
train35_labels

train44_labels <- labelTopics(dgrl_stm44, n = 10)
train44_labels

## estimateEffect k = 33, 45, 47
fx_33 <- estimateEffect(1:33 ~ year.x, dgrl_stm33, meta = quant2stm$meta)

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

## LDAvis k=24 ##
toLDAvis(dgrl_stm24,
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

