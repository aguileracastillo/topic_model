#### stminsights ####
library(stm)
library(stminsights)

## CONVERT FROM QUANTEDA TO STM
test_stm <- convert(test_dgrl175, to = "stm")

test_stm$meta$year.x <- as.numeric(test_stm$meta$year.x)
test_stm$meta$period <- as.numeric(test_stm$meta$period)

out <- list(documents = test_stm$documents,
            vocab = test_stm$vocab,
            meta = test_stm$meta)

str(test_stm)

## k=53 with Test set
dgrl_test_stm53 <- stm(test_stm$documents, 
                       test_stm$vocab, 
                       K = 53,
                       prevalence = ~ year.x,
                       max.em.its = 75,
                       data = test_stm$meta, 
                       init.type = "Spectral")

## SHARE OF TOPICS OVER ALL CORPUS ##
plot(
  dgrl_test_stm53,
  type = "summary",
  text.cex = 0.8,
  main = "Estimated Topic Proportions Test Set",
  xlab = "Share estimation")

#### Topic Quality do not run ####
topicQuality(dgrl_test_stm53, 
             documents = test_stm$documents, 
             xlab = "Semantic Coherence",
             ylab = "Exclusivity",
             labels = dgrl_test_stm53$theta,
             M = 10)

## PRINT WORDS PER TOPIC
data.frame(t(labelTopics(dgrl_test_stm53, n = 10)$prob))

## PRINT WORDS PER TOPIC
data.frame(t(labelTopics(topic_train33, n = 10)$prob))

## Highest Prob / FREX / LIFT / Score
test53_labels <- labelTopics(dgrl_test_stm53, n = 10)
test53_labels

fx_test_53 <- estimateEffect(1:53 ~ year.x, 
                     dgrl_test_stm53, 
                     meta = out$meta, 
                     uncertainty = "Global")

## Topic Prevalence over Time ##
par(mfrow=c(3,3))
for (i in seq_along(sample(1:33, size = 9)))
{
  plot(fx, "year.x", method = "continuous", topics = i, main = paste0(train33_labels$prob[i,1:3], collapse = ", "), printlegend = F)
}

save(topic_train50, fx, file = "topic_train50.RData")

run_stminsights(use_browser = TRUE)

## test ldavis ##
toLDAvis(topic_train33,
         test_stm$documents,
         R = 30,
         plot.opts = list(xlab = "PC1", ylab = "PC2"),
         lambda.step = 0.1,
         out.dir = tempfile(),
         open.browser = interactive(),
         as.gist = FALSE,
         reorder.topics = TRUE)


