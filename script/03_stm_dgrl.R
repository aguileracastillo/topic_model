#### Structural Topic Model ####
library(stm)
library(stminsights)

## CONVERT FROM QUANTEDA TO STM
quant2stm <- convert(train_dgrl175, to = "stm")

## Running of searchK() with different values for k
findingK <- searchK(quant2stm$documents, quant2stm$vocab, K = c(25, 50, 75, 100, 150, 200),
                    prevalence =~ year.x, data = quant2stm$meta, set.seed(02913),verbose=FALSE)

plot(findingK)


## CALCULATE STM k = 25 ##
dgrl_stm25 <- stm(quant2stm$documents, 
                  quant2stm$vocab, 
                  K = 25,
                  prevalence = ~ year.x,
                  max.em.its = 75,
                  data = quant2stm$meta, 
                  init.type = "Spectral")

fx_25 <- estimateEffect(1:25 ~ year.x, dgrl_stm25, meta = quant2stm$meta)

## PRINT WORDS PER TOPIC
data.frame(t(labelTopics(dgrl_stm25, n = 10)$prob))

## SHARE OF TOPICS OVER ALL CORPUS ##
plot(
  dgrl_stm25,
  type = "summary",
  text.cex = 0.5,
  main = "STM topic shares",
  xlab = "Share estimation")

## CALCULATE STM k = 50 ##
dgrl_stm50 <- stm(quant2stm$documents, 
                  quant2stm$vocab, 
                  K = 50,
                  prevalence = ~ year.x,
                  max.em.its = 75,
                  data = quant2stm$meta, 
                  init.type = "Spectral")

fx_50 <- estimateEffect(1:50 ~ year.x, dgrl_stm50, meta = quant2stm$meta)

## PRINT WORDS PER TOPIC
data.frame(t(labelTopics(dgrl_stm50, n = 10)$prob))

## SHARE OF TOPICS OVER ALL CORPUS ##
plot(
  dgrl_stm50,
  type = "summary",
  text.cex = 0.5,
  main = "STM topic shares",
  xlab = "Share estimation")


## WORDCLOUD OF MOST PREVALENT TOPIC review usefulness
stm::cloud(dgrl_stm50,
           topic = 21,
           scale = c(2.25, .5))

## EYEBALLING COVID RELATED TOPICS 
plot(dgrl_stm50, type = "perspectives", topics = c(30,46)) 

## DOCUMENT TOPIC PROPORTIONS
plot(dgrl_stm50, type = "hist", topics = sample(1:50, size = 9))

## LDAvis k=25 ##
toLDAvis(dgrl_stm25,
         quant2stm$documents,
         R = 30,
         plot.opts = list(xlab = "PC1", ylab = "PC2"),
         lambda.step = 0.1,
         out.dir = tempfile(),
         open.browser = interactive(),
         as.gist = FALSE,
         reorder.topics = TRUE
         )

## LDAvis k=50 ##
toLDAvis(dgrl_stm50,
         quant2stm$documents,
         R = 30,
         plot.opts = list(xlab = "PC1", ylab = "PC2"),
         lambda.step = 0.1,
         out.dir = tempfile(),
         open.browser = interactive(),
         as.gist = FALSE,
         reorder.topics = TRUE
         )




