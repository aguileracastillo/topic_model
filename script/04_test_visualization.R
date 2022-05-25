#### stminsights ####
library(stm)
library(stminsights)

## CONVERT FROM QUANTEDA TO STM
quant2stm <- convert(train_dgrl175, to = "stm")

out <- list(documents = quant2stm$documents,
            vocab = quant2stm$vocab,
            meta = quant2stm$meta)

topic_train50 <- stm(documents = out$documents,
                     vocab = out$vocab,
                     data = out$meta,
                     prevalence =~ year.x,
                     K = 50)

fx <- estimateEffect(1:50 ~ year.x, topic_train50, meta = out$meta)

save(topic_train50, fx, file = "topic_train50.RData")

run_stminsights(use_browser = TRUE)

## test ldavis ##
toLDAvis(topic_train50,
         quant2stm$documents,
         R = 30,
         plot.opts = list(xlab = "PC1", ylab = "PC2"),
         lambda.step = 0.1,
         out.dir = tempfile(),
         open.browser = interactive(),
         as.gist = FALSE,
         reorder.topics = TRUE)


