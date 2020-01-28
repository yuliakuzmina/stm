library(stm)

set.seed(1)
tmpdf2 = as.data.frame(tmpdf2)
processed <- textProcessor(tmpdf2$text_lemm_1000, metadata = tmpdf2, stem=F, removestopwords = TRUE, language = "ru")

out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta  <-out$meta
saveRDS(out, file = "intermediate_data/out1306.RDS")
out <- readRDS(file = "intermediate_data/out1306.RDS")

# search K (the number of topics for the model)
storage <- searchK(out$documents,
                   out$vocab,
                   K = c(10, 15, 20, 25, 30),
                   prevalence =~fed,
                   data = out$meta)
storage$results
plot(storage)

saveRDS(storage, file = "intermediate_data/storage2406.RDS")
storage <- readRDS(file = "/Users/yuliyakuzmina/Documents/R/integrum_sex_ed/intermediate_data/storage.RDS")

# k 20 seems to suit best
cov_fed_20 <- stm(documents = out$documents, vocab = out$vocab,
                  K = 20, prevalence =~fed,
                  max.em.its = 100, data = out$meta,
                  init.type = "Spectral")

saveRDS(cov_fed_20, file = "intermediate_data/cov_fed_20_210619")
cov_fed_20 <- readRDS(file = "intermediate_data/cov_fed_20_210619")

