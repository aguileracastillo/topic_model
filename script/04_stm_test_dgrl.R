#### Structural Topic Model for DGRL 17.5 TEST SET K=30 ####
library(stm)
library(stminsights)
library(officer)
library(flextable)


## CONVERT FROM QUANTEDA TO STM
test_stm <- convert(test_dgrl175, to = "stm")

test_stm$meta$year.x <- as.numeric(test_stm$meta$year.x)
test_stm$meta$period <- as.numeric(test_stm$meta$period)

out <- list(documents = test_stm$documents,
            vocab = test_stm$vocab,
            meta = test_stm$meta)

str(test_stm)

## k=30 with Test set
dgrl_test_stm30 <- stm(test_stm$documents, 
                       test_stm$vocab, 
                       K = 30,
                       prevalence = ~ year.x,
                       max.em.its = 75,
                       data = test_stm$meta, 
                       init.type = "Spectral") ## Spectral for Reproducibility ##

## SHARE OF TOPICS OVER ALL CORPUS ##
plot(
  dgrl_test_stm30,
  type = "summary",
  text.cex = 0.8,
  main = "Estimated Topic Proportions Test Set",
  xlab = "Share estimation")

#### Topic Quality do not run ####
topicQuality(dgrl_test_stm30, 
             documents = test_stm$documents, 
             xlab = "Semantic Coherence",
             ylab = "Exclusivity",
             labels = dgrl_test_stm30$theta,
             M = 10)

## PRINT WORDS PER TOPIC
top30df <- data.frame(t(labelTopics(dgrl_test_stm30, n = 10)$prob))
test30_labels_transpose <- as.data.frame(t(top30df))
rownames(test30_labels_transpose) <- paste0("X", 1:ncol(test30_labels_transpose))
print(test30_labels_transpose)

write.csv(test30_labels_transpose, file = "~/GitHub/topic_model/output\\test_30topics.csv", row.names = TRUE)

## Highest Prob / FREX / LIFT / Score
test30_labels <- labelTopics(dgrl_test_stm30, n = 10, frexweight = 0.5)
print(test30_labels)

## Wordcloud Topic-Word Probabilities
stm::cloud(dgrl_test_stm30,
           topic = 1,
           scale = c(3.25, .95))


## Estimate Effect Topic Prevalence over time ##
fx_test_30 <- estimateEffect(1:30 ~ year.x, 
                     dgrl_test_stm30, 
                     meta = out$meta, 
                     uncertainty = "Global")

## Visualization of Topic Prevalence over Time ##
par(mfrow=c(1,1))
for (i in seq_along(sample(1:30, size = 30)))
{
  plot(fx_test_30, "year.x", method = "continuous", topics = i, main = paste0(test30_labels$prob[i,1:3], collapse = ", "), printlegend = T)
}

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


