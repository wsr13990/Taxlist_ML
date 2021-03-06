library(tm)
library(SnowballC)
library(C50)
library(RWeka)
library(caret)
library(partykit)

options(java.parameters = "-Xmx8000m")

process_description <- function(data, freq){
  description <- data$Deskripsi.Invoice
  description_corpus <- VCorpus(VectorSource(data$Deskripsi.Invoice))
  description_corpus_clean <- tm_map(description_corpus, content_transformer(tolower))
  description_corpus_clean <- tm_map(description_corpus_clean, removePunctuation)
  description_corpus_clean <- tm_map(description_corpus_clean, stripWhitespace)
  taxlist_dtm <- DocumentTermMatrix(description_corpus_clean)
  desc_freq_words <- findFreqTerms(taxlist_dtm, freq)
  taxlist_dtm <- taxlist_dtm[, desc_freq_words]
  taxlist_dtm <- apply(taxlist_dtm, MARGIN=2, convert_count)
  taxlist_dtm <- as.matrix(taxlist_dtm)
  return(taxlist_dtm)
}

join_taxlist_dtm <- function(taxlist, dtm_matrix){
  matrix <- subset(taxlist, select=-c(Deskripsi.Invoice,Tarif.PPh.SPT,pasal.SPT,Jenis.Jasa,Classification,Mata.Uang.DPP))
  matrix <- cbind(matrix, dtm_matrix)
  return(matrix)
}

convert_count <- function(x){
  x <- ifelse(x, "Yes", "No")
}

taxlist <- read.csv("D:/Taxlist_ML/Taxlist.csv", header = TRUE, sep=",")
taxlist <- taxlist[taxlist$Nama.Vendor %in% c("Tower Bersama"),]
#taxlist <- taxlist[taxlist$Nama.Vendor %in% c(
#   "Huawei Tech Investment",
#   "Ericsson Indonesia",
#   "Nokia Solutions and Networks Indonesia",
#   "Westindo Esa Perkasa",
#   "Westindo Putra Perkasa",
#   "Wiraky Nusa Telekomunikasi",
#   "Hariff Daya Tunggal Engineering",
#   "Abhimata Citra Abadi",
#   "INFRASTRUKTUR TELEKOMUNIKASI INDONESIA",
#   "Consistel Indonesia",
#   "Ericsson AB",
#   "Nokia Solutions and Networks Oy",
#   "Huawei International Pte Ltd",
#   "Huawei Technologies Co Ltd"),]
set.seed(123)
row_num <- nrow(taxlist)
train_num <- round(row_num/2)
train_sample <- sample(row_num, train_num)
taxlist_train <- taxlist[train_sample,]
taxlist_test <- taxlist[-train_sample,]
crossval_row <- round(nrow(taxlist_test)/2)
taxlist_crossval <- taxlist_test[1:crossval_row,]
taxlist_test <- taxlist_test[(crossval_row):nrow(taxlist_test),]
#taxlist_train <- taxlist

train_dtm <- process_description(taxlist_train,1)
#train_dtm <- process_description(taxlist_train, 5)
#crossval_dtm <- process_description(taxlist_crossval, 5)
#test_dtm <- process_description(taxlist_test, 5)

train_matrix <- join_taxlist_dtm(taxlist_train, train_dtm)
#crossval_matrix <- join_taxlist_dtm(taxlist_crossval, crossval_dtm)
#test_matrix <- join_taxlist_dtm(taxlist_test, test_dtm)

train_class <- as.factor(as.matrix(taxlist_train["Classification"]))
#crossval_class <- as.factor(as.matrix(taxlist_crossval["Classification"]))
#test_class <- as.factor(as.matrix(taxlist_test["Classification"]))
train_matrix <- cbind(train_class,train_matrix)

train_dtm <- as.data.frame(train_dtm)
#Huawei <- C5.0(x = train_dtm, , trials = 5, train_class, rules = TRUE)
#Huawei <- C5.0(train_class~., data = train_matrix[,-c(1,3)], rules = TRUE)
model <- C5.0(train_dtm, train_class, rules = TRUE, control = C5.0Control(earlyStopping = FALSE))
#model <- train(train_matrix$train_class~.,
#               data = train_matrix[,-c(2:4)],
#               method = "C5.0Cost",
#               tuneGrid = expand.grid(model = "rule",winnow = FALSE, trials =1, cost = 10))


#tree_model <- train(train_matrix, train_class, method = "C5.0")
#rule_model <- train(train_matrix, train_class, method = "JRip")

summary(model)
summary(train_matrix[,c(3)])

#C5.0.graphviz(c50_model, "c50_model.txt")
#ripper_model
#write(capture.output(summary(taxlist_model)), "c50model.txt")
