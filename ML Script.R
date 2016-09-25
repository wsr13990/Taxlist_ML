library(tm)
library(SnowballC)

taxlist <- read.csv("D:/Taxlist_ML/Taxlist.csv", header = TRUE, sep=";")
set.seed(123)
row_num <- nrow(taxlist)
train_num <- round(row_num/2)
train_sample <- sample(row_num, train_num)
taxlist_train <- taxlist[train_sample,]
taxlist_test <- taxlist[-train_sample,]
crossval_row <- round(nrow(taxlist_test)/2)
taxlist_crossval <- taxlist_test[1:crossval_row,]
taxlist_test <- taxlist_test[(crossval_row+1):nrow(taxlist_test),]

process_description <- function(data, freq){
  description <- data$Deskripsi.Invoice
  description_corpus <- VCorpus(VectorSource(data$Deskripsi.Invoice))
  description_corpus_clean <- tm_map(description_corpus, content_transformer(tolower))
  description_corpus_clean <- tm_map(description_corpus_clean, removePunctuation)
  taxlist_dtm <- DocumentTermMatrix(description_corpus_clean)
  desc_freq_words <- findFreqTerms(taxlist_dtm, freq)
  taxlist_dtm <- taxlist_dtm[, desc_freq_words]
  taxlist_dtm <- as.matrix(taxlist_dtm)
  return(taxlist_dtm)
}

join_taxlist_dtm <- function(taxlist, dtm_matrix){
  matrix <- subset(taxlist, select=-c(Deskripsi.Invoice,Tarif.PPh.SPT,pasal.SPT,Jenis.Jasa))
  matrix <- cbind(matrix, dtm_matrix)
  return(matrix)
}

train_dtm <- process_description(taxlist_train, 5)
crossval_dtm <- process_description(taxlist_crossval, 5)
test_dtm <- process_description(taxlist_test, 5)

train_matrix <- join_taxlist_dtm(taxlist_train, train_dtm)
crossval_matrix <- join_taxlist_dtm(taxlist_crossval, crossval_dtm)
test_matrix <- join_taxlist_dtm(taxlist_test, test_dtm)







as.character(description_corpus_clean[[1]])
str(description_dtm)
