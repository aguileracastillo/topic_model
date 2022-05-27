#### stminsights ####
library(stm)
library(stminsights)

## CONVERT FROM QUANTEDA TO STM
test_stm <- convert(test_dgrl175, to = "stm")

time <- as.numeric(test_stm$meta$year.x)

out <- list(documents = test_stm$documents,
            vocab = test_stm$vocab,
            meta = test_stm$meta)

topic_train33 <- stm(documents = out$documents,
                     vocab = out$vocab,
                     data = out$meta,
                     prevalence =~ year.x,
                     K = 33)

plot(topic_train33)

## PRINT WORDS PER TOPIC
data.frame(t(labelTopics(topic_train33, n = 10)$prob))

## Highest Prob / FREX / LIFT / Score
train33_labels <- labelTopics(topic_train33, n = 10)
train33_labels

fx33 <- estimateEffect(1:33 ~ s(year.x), 
                     topic_train33, 
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


