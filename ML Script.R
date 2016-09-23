library(tm)
library(SnowballC)

taxlist <- read.csv("C:/ML Taxlist/Taxlist.csv", header = TRUE, sep=";")
set.seed(123)
row_num <- nrow(taxlist)
train_num <- round(row_num/2)
train_sample <- sample(row_num, train_num)
taxlist_train <- taxlist[train_sample,]
taxlist_test <- taxlist[-train_sample,]
crossval_row <- round(nrow(taxlist_test)/2)
taxlist_crossval <- taxlist_test[1:crossval_row,]
taxlist_test <- taxlist_test[(crossval_row+1):nrow(taxlist_test),]

description <- taxlist$Deskripsi.Invoice
description_corpus <- VCorpus(VectorSource(taxlist$Deskripsi.Invoice))
description_corpus_clean <- tm_map(description_corpus, content_transformer(tolower))
description_corpus_clean <- tm_map(description_corpus_clean, removePunctuation)
description_dtm <- DocumentTermMatrix(description_corpus_clean)

desc_freq_words <- findFreqTerms(description_dtm, 5)
description_dtm <- description_dtm[, desc_freq_words]

description_matrix <- as.matrix(description_dtm)





as.character(description_corpus_clean[[1]])
str(description_dtm)
