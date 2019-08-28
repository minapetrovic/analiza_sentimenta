#install.packages("tm")
#install.packages("tidyr") #koriscena fja unite
library(tm)
library(tidyr)
library(ggplot2)

#Ucitavanje dataseta
ecommerce_reviews <- read.csv("e-commerce_reviews.csv", stringsAsFactors = FALSE)

summary(ecommerce_reviews)
str(ecommerce_reviews)

#provera da li imamo NA vrednosti u datasetu
length(which(!complete.cases(ecommerce_reviews)))

length(which(ecommerce_reviews$Title==""))

length(which(ecommerce_reviews$Review.Text==""))

length(which(ecommerce_reviews$Title=="" & ecommerce_reviews$Review.Text==""))

#posto ovakvih unosa, gde nemamo ni naslov ni tekst komentara ima 3% (=844/23486) od ukupnog dataseta,
#da li zanemariti ovo i izbaciti te unose?
# Nikola: izbaciti, nije moguce zameniti ih necim. Ali neka ostane ovaj korak u skripti i spomenucete ga i u tekstu

na.indeksi <- which(ecommerce_reviews$Title=="" & ecommerce_reviews$Review.Text=="")
ecommerce_reviews <- ecommerce_reviews[-na.indeksi,] #ovim se smanjuje dataset na 22642 opservacije

#izbacivanje nepotrebnih atributa: X, Clothing.ID, Age, Division.Name, Department.Name, Class.Name
#i Positive.Feedback.Count
ecommerce_reviews$X <- NULL
ecommerce_reviews$Clothing.ID <- NULL
ecommerce_reviews$Age <- NULL
ecommerce_reviews$Division.Name <- NULL
ecommerce_reviews$Department.Name <- NULL
ecommerce_reviews$Class.Name <- NULL
ecommerce_reviews$Positive.Feedback.Count <- NULL

#spajanje kolona Title i Review.Text u novu "Review", radi lakse obrade tektsa, a prethodne se uklanjaju
ecommerce_reviews <- unite(ecommerce_reviews, "Review", c("Title","Review.Text"), sep = " ", remove = T)

#da bismo oznacili komentare kao pozitivne/negativne, uzimamo u obzir preostale 2 varijable
#(Rating i Recommended.IND)
corr.matrix <- cor(ecommerce_reviews[-1])

#(kako raste Rating (ka oceni 5), tako Recommended.IND naginje vise ka 1 (dakle da osoba preporucuje proizvod))
#Na osnovu ovoga, moze se uzeti Rating kao osnova za oznacavanje podataka.

#Oznacavanje podataka: ocene 1 i 2 -> NEG, ocene 4 i 5 -> POS,
#ali moramo videti sta cemo raditi sa ocenom 3
prop.table(table(ecommerce_reviews$Rating,ecommerce_reviews$Recommended.IND), margin = 1)
# Nikola: dodao sam margin = 1 kako bi se redovi sumirali u 100%, odnosno u 1
#Mina: da, to ima vise smisla. Svakako ocena 3 ima raspodelu 59/41, tako da svakako ne utice drasticno 
#Takodje, nekako sam izostavila uklanjanje Recomended.IND varijable, da li je iskljuciti na dalje?
ecommerce_reviews$Recommended.IND <- NULL

#Na osnovu raspodele vrednosti Rating po tome da li su preporucene (Recommended.IND=1) ili ne,
#vidimo da je priblizno jednaka raspodela ocene 3 (7% ne preporucuje, dok 5% preporucuje).
#Posto ni jedna "strana" raspodele ne dominira, zakljucak :
#ocena 3 se moze iskljuciti iz daljeg razmatranja za oznacavanje podataka.
ecommerce_reviews <- ecommerce_reviews[-which(ecommerce_reviews$Rating==3),]

summary(ecommerce_reviews)

#Oznacavamo preostale opservacije.
ecommerce_reviews$Label <- ifelse(test = ecommerce_reviews$Rating == 1 | ecommerce_reviews$Rating == 2,
                                  yes = "NEG",
                                  no = "POS")

ecommerce_reviews$Label <- as.factor(ecommerce_reviews$Label)

#Raspodela labela, odnosno klasa koje imamo (NEG i POS)
prop.table(table(ecommerce_reviews$Label))
#12% negativnih komentara i 88% pozitivnih, dakle neravnomerna distribucija komentara u korist pozitivnih


##################################
#KREIRANJE KORPUSA
##################################
corpus_review <- VCorpus(VectorSource(ecommerce_reviews$Review))

#detalji o 123. dokumentu
inspect(corpus_review[123])
#Primer sadrzaja 123. dokumenta korpusa
inspect(corpus_review[[123]])

# 1. Sve reci u svim dokumentima pretvoriti u reci sa malim slovima.
corpus_review <- tm_map(corpus_review, content_transformer(tolower))

# 2. Uklanjanje stopwords-a iz tm-ove liste stopwords-a
corpus_review <- tm_map(corpus_review, removeWords, stopwords("english"))

# 3. Uklanjanje brojeva
corpus_review <- tm_map(corpus_review, removeNumbers)

# 4. Uklanjanje znakova interpunkcije
corpus_review <- tm_map(corpus_review , removePunctuation,
                        preserve_intra_word_contractions = TRUE,
                        preserve_intra_word_dashes = TRUE)

# 5. Uklanjanje nepotrebnih razmaka
corpus_review <- tm_map(corpus_review, stripWhitespace)

# 6. Stemovanje reci
#install.packages('SnowballC')
library(SnowballC)
corpus_review <- tm_map(corpus_review , stemDocument, language = "english")

#####################################################
#KREIRANJE DTM, TF-IDF za unigrame, bigrame, trigrame
#####################################################

library(quanteda)
if (requireNamespace("tm", quietly = TRUE)) {
  qCorpus <- corpus(corpus_review)
}


#Kreiranje tokena za svaki od dokumenata u korpusu, (token = unigram)
review.tokens <- tokens(qCorpus$documents$texts, what = "word")
#Prikaz tokena 123. dokuemnta
review.tokens[[123]]          

#Kreiranje DcoumentTermMatrix (DocumentFeatureMatrix)
review.tokens.dfm <- dfm(review.tokens, tolower = FALSE)
#Prebacivanje DFM u matricu
#review.tokens.matrix <- as.matrix(review.tokens.dfm)
dim(review.tokens.dfm)
review.tokens.dfm

#Top 20 najfrekventnijih reci i njihove frekvencije
topfeatures(review.tokens.dfm, 20)

#Wordcloud - vece reci i roze su one koje se najcesce pojavljuju, a manje i zelene, one koje se manje pojavljuju
set.seed(100)
textplot_wordcloud(review.tokens.dfm, min_count = 700, random_order = FALSE,
                   rotation = .25,
                   color = RColorBrewer::brewer.pal(4, "Dark2"))

#TF-IDF od unigrama (tokena) sa labelama
review.tokens.dfm.tfidf <- dfm_tfidf(review.tokens.dfm, scheme_tf = "prop",
                                   scheme_df = "inverse", base = 10)
dim(review.tokens.dfm.tfidf)

#PROVERA NORMALNOSTI VARIJBLI DATASET-a

#install.packages('nortest')
library(nortest)
dataframe <- as.data.frame(review.tokens.dfm.tfidf)
kolone <- floor(runif(10, min=1, ncol(dataframe)))
for (i in kolone){
  print(ad.test(dataframe[,i]))
}
#Slucajnim uzorcima iz dataframe-a vidimo da varijable ne podlezu Normalnoj raspodeli


#########################################################
#DISKRETIZACIJA DATASET-OVA
#########################################################
#The Binarized Multinomial Naive Bayes is used when the frequencies of the words
#don’t play a key role in our classification. Such an example is Sentiment Analysis,
#where it does not really matter how many times someone mentions the word “bad” but rather
#only the fact that he does.
#izvor: http://blog.datumbox.com/machine-learning-tutorial-the-naive-bayes-text-classifier/
#Boolean feature (Binarized) Multinomial Naive Bayes
#Function to convert the word frequencies to yes and no labels
convert_counts <- function(x) {
  x <- as.factor(ifelse(x > 0, "Yes", "No"))
}

#Vektor sa ukupnim tf-idf skorovima za svaki unigram 
total.tfidf.scores.unigram <- as.vector(colSums(review.tokens.dfm.tfidf))

#75. i 90. percentil
percentil75 <- quantile(total.tfidf.scores.unigram, 0.99) #proba sa 99. percentilom
percentil90 <- quantile(total.tfidf.scores.unigram, 0.995) #proba sa 995. percentilom

#UNIGRAMI sa total TF-IDF skorom > 75 percentila
unigram.index.removals.75 <- which(total.tfidf.scores.unigram < percentil75)
unigrams.tfidf.75 <- dfm_select(review.tokens.dfm.tfidf, pattern = colnames(review.tokens.dfm.tfidf)[unigram.index.removals.75],
                               selection = "remove")
unigrams.tfidf.75 <- apply(unigrams.tfidf.75, MARGIN = 2, convert_counts)
unigrams.tfidf.75 <- as.data.frame(unigrams.tfidf.75) 
unigrams.tfidf.75 <- cbind(Label = ecommerce_reviews$Label,unigrams.tfidf.75)


#UNIGRAMI sa total TF-IDF skorom > 90 percentila
unigram.index.removals.90 <- which(total.tfidf.scores.unigram < percentil90)
unigrams.tfidf.90 <- dfm_select(review.tokens.dfm.tfidf, pattern = colnames(review.tokens.dfm.tfidf)[unigram.index.removals.90],
                                selection = "remove")
unigrams.tfidf.90 <- apply(unigrams.tfidf.90, MARGIN = 2, convert_counts)
unigrams.tfidf.90 <- as.data.frame(unigrams.tfidf.90) 
unigrams.tfidf.90 <- cbind(Label = ecommerce_reviews$Label,unigrams.tfidf.90)

dim(unigrams.tfidf.75)
dim(unigrams.tfidf.90)


##############################
#Kreiramo bigrame
##############################



bigrams <- tokens_ngrams(review.tokens, n = 2:2)
#Prikaz bigrama 123. dokumenta
bigrams[[123]]
#Kreiramo DFM od bigrama
review.bigrams.dfm <- dfm(bigrams, tolower = FALSE)

dim(review.bigrams.dfm)
review.bigrams.dfm

#TF-IDF od bigrama sa labelama
review.bigrams.dfm.tfidf <- dfm_tfidf(review.bigrams.dfm, scheme_tf = "prop",
                                     scheme_df = "inverse", base = 10)
dim(review.bigrams.dfm.tfidf)

#Vektor sa ukupnim tf-idf skorovima za svaki bigram 
total.tfidf.scores.bigram <- as.vector(colSums(review.bigrams.dfm.tfidf))

#75. i 90. percentil
percentil75 <- quantile(total.tfidf.scores.bigram, 0.99) #proba sa 97. percentilom
percentil90 <- quantile(total.tfidf.scores.bigram, 0.995) #proba sa 99. percentilom

#BIGRAMI sa total TF-IDF skorom > 75 percentila
bigram.index.removals.75 <- which(total.tfidf.scores.bigram < percentil75)
bigrams.tfidf.75 <- dfm_select(review.bigrams.dfm.tfidf, pattern = colnames(review.bigrams.dfm.tfidf)[bigram.index.removals.75],
                               selection = "remove")
bigrams.tfidf.75 <- apply(bigrams.tfidf.75, MARGIN = 2, convert_counts)
bigrams.tfidf.75 <- as.data.frame(bigrams.tfidf.75) 
bigrams.tfidf.75 <- cbind(Label = ecommerce_reviews$Label,bigrams.tfidf.75)

dim(bigrams.tfidf.75)

#BIGRAMI sa total TF-IDF skorom > 90 percentila
bigram.index.removals.90 <- which(total.tfidf.scores.bigram < percentil90)
bigrams.tfidf.90 <- dfm_select(review.bigrams.dfm.tfidf, pattern = colnames(review.bigrams.dfm.tfidf)[bigram.index.removals.90],
                               selection = "remove")
bigrams.tfidf.90 <- apply(bigrams.tfidf.90, MARGIN = 2, convert_counts)
bigrams.tfidf.90 <- as.data.frame(bigrams.tfidf.90) 
bigrams.tfidf.90 <- cbind(Label = ecommerce_reviews$Label,bigrams.tfidf.90)

dim(bigrams.tfidf.90)

##############################
#Kreiramo trigrame
##############################



trigrams <- tokens_ngrams(review.tokens, n = 3:3)
#Prikaz trigrama 123. dokumenta
trigrams[[123]]
#Kreiramo DFM od trigrama
review.trigrams.dfm <- dfm(trigrams, tolower = FALSE)

dim(review.trigrams.dfm)
review.trigrams.dfm

#TF-IDF od trigrama sa labelama
review.trigrams.dfm.tfidf <- dfm_tfidf(review.trigrams.dfm, scheme_tf = "prop",
                                      scheme_df = "inverse", base = 10)
dim(review.trigrams.dfm.tfidf)

#Vektor sa ukupnim tf-idf skorovima za svaki trigram 
total.tfidf.scores.trigram <- as.vector(colSums(review.trigrams.dfm.tfidf))

#75. i 90. percentil
percentil75 <- quantile(total.tfidf.scores.trigram, 0.99) #proba sa 97. percentilom
percentil90 <- quantile(total.tfidf.scores.trigram, 0.995) #proba sa 99. percentilom

#TRIGRAMI sa total TF-IDF skorom > 75 percentila
trigram.index.removals.75 <- which(total.tfidf.scores.trigram < percentil75)
trigrams.tfidf.75 <- dfm_select(review.trigrams.dfm.tfidf, pattern = colnames(review.trigrams.dfm.tfidf)[trigram.index.removals.75],
                               selection = "remove")
trigrams.tfidf.75 <- apply(trigrams.tfidf.75, MARGIN = 2, convert_counts)
trigrams.tfidf.75 <- as.data.frame(trigrams.tfidf.75) 
trigrams.tfidf.75 <- cbind(Label = ecommerce_reviews$Label,trigrams.tfidf.75)

dim(trigrams.tfidf.75)

#TRIGRAMI sa total TF-IDF skorom > 90 percentila
trigram.index.removals.90 <- which(total.tfidf.scores.trigram < percentil90)
trigrams.tfidf.90 <- dfm_select(review.trigrams.dfm.tfidf, pattern = colnames(review.trigrams.dfm.tfidf)[trigram.index.removals.90],
                               selection = "remove")

trigrams.tfidf.90 <- apply(trigrams.tfidf.90, MARGIN = 2, convert_counts)
trigrams.tfidf.90 <- as.data.frame(trigrams.tfidf.90) 
trigrams.tfidf.90 <- cbind(Label = ecommerce_reviews$Label,trigrams.tfidf.90)

dim(trigrams.tfidf.90)

########################################################################
# KONFIGURACIJE
########################################################################

#install.packages(c("dplyr","DMwR","purr"))
library(dplyr) # for data manipulation
library(caret) # for model-building
library(DMwR) # for smote implementation
library(purrr) # for functional programming (map)
library(pROC) # for AUC calculations

########################################################################
# KONFIGURACIJA 1 sa podkonfiguracijama original, down-sampling i SMOTE 
########################################################################

set.seed(1010)
train.indexes <- createDataPartition(unigrams.tfidf.75$Label, p = .80, list = FALSE)
train1 <- unigrams.tfidf.75[train.indexes,]
test1 <- unigrams.tfidf.75[-train.indexes,]
prop.table(table(test1$Label))

# Kontrolna funkcija za 10fold cross-validation
#install.packages("doSNOW")
library(e1071)
library(doSNOW)


ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 2,
                     verboseIter = TRUE,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE)


# Treniramo trening set koriscenjem Naive Bayes metode
start.time <- Sys.time()

set.seed(5627)
orig_fit <- train(Label ~ .,
                   data = train1,
                   method = "naive_bayes",
                   verbose = FALSE,
                   metric = "ROC",
                   trControl = trainControl(method="none", classProbs = TRUE),
                   tuneGrid = data.frame(usekernel=FALSE,laplace=1,adjust=FALSE))

total.time <- Sys.time() - start.time
total.time

# Build custom AUC function to extract AUC
# from the caret model object

orig_fit %>%
  test_roc(data = test1) %>%
  auc()

# Use the same seed to ensure same cross-validation splits

ctrl$seeds <- orig_fit$control$seeds

# Build down-sampled model

ctrl$sampling <- "down"

down_fit <- train(Label ~ .,
                   data = train1,
                   method = "naive_bayes",
                   verbose = FALSE,
                   metric = "ROC",
                   trControl = trainControl(method="none", classProbs = TRUE, 
                                            seeds = orig_fit$control$seeds,
                                            sampling = "down"),
                   tuneGrid = data.frame(usekernel=FALSE,laplace=1,adjust=FALSE))


# Build smote model

ctrl$sampling <- "smote"

smote_fit <- train(Label ~ .,
                    data = train1,
                    method = "naive_bayes",
                    verbose = FALSE,
                    metric = "ROC",
                    trControl = trainControl(method="none", classProbs = TRUE, 
                                             seeds = orig_fit$control$seeds,
                                             sampling = "smote"),
                    tuneGrid = data.frame(usekernel=FALSE,laplace=1,adjust=FALSE))

# Examine results for test set

model_list <- list(original = orig_fit,
                    down = down_fit,
                    SMOTE = smote_fit)

model_list_roc <- model_list %>%
  map(test_roc, data = test1)

model_list_roc %>%
  map(auc)



########################################################################
# KONFIGURACIJA 2 sa podkonfiguracijama original, down-sampling i SMOTE 
########################################################################

set.seed(1010)
train2.indexes <- createDataPartition(bigrams.tfidf.75$Label, p = .80, list = FALSE)
train2 <- bigrams.tfidf.75[train2.indexes,]
test2 <- bigrams.tfidf.75[-train2.indexes,]
prop.table(table(test2$Label))

# Kontrolna funkcija za 10fold cross-validation
ctrl2 <- trainControl(method = "repeatedcv",
                      number = 10,
                      repeats = 2,
                      verboseIter = TRUE,
                      summaryFunction = twoClassSummary,
                      classProbs = TRUE)
#install.packages("naivebayes")
library(naivebayes)
# Treniramo trening set koriscenjem Naive Bayes metode
start.time <- Sys.time()

set.seed(5627)
orig_fit2 <- train(Label ~ .,
                   data = train2,
                   method = "naive_bayes",
                   verbose = FALSE,
                   metric = "ROC",
                   trControl = trainControl(method="none", classProbs = TRUE),
                   tuneGrid = data.frame(usekernel=FALSE,laplace =1,adjust=FALSE))

total.time <- Sys.time() - start.time
total.time

# Build custom AUC function to extract AUC
# from the caret model object

orig_fit2 %>%
  test_roc(data = test2) %>%
  auc()

# Use the same seed to ensure same cross-validation splits

ctrl2$seeds <- orig_fit2$control$seeds

# Build down-sampled model

ctrl2$sampling <- "down"

down_fit2 <- train(Label ~ .,
                   data = train2,
                   method = "naive_bayes",
                   verbose = FALSE,
                   metric = "ROC",
                   trControl = trainControl(method="none", classProbs = TRUE, 
                                            seeds = orig_fit2$control$seeds,
                                            sampling = "down"),
                   tuneGrid = data.frame(usekernel=FALSE,laplace=1,adjust=FALSE))


# Build smote model

ctrl2$sampling <- "smote"

smote_fit2 <- train(Label ~ .,
                    data = train2,
                    method = "naive_bayes",
                    verbose = FALSE,
                    metric = "ROC",
                    trControl = trainControl(method="none", classProbs = TRUE, 
                                             seeds = orig_fit2$control$seeds,
                                             sampling = "smote"),
                    tuneGrid = data.frame(usekernel=FALSE,laplace=1,adjust=FALSE))

# Examine results for test set

model_list2 <- list(original = orig_fit2,
                    down = down_fit2,
                    SMOTE = smote_fit2)

model_list_roc2 <- model_list2 %>%
  map(test_roc, data = test2)

model_list_roc2 %>%
  map(auc)



########################################################################
# KONFIGURACIJA 3 sa podkonfiguracijama original, down-sampling i SMOTE 
########################################################################

set.seed(1234)
train3.indexes <- createDataPartition(trigrams.tfidf.75$Label, p = .80, list = FALSE)
train3 <- trigrams.tfidf.75[train3.indexes,]
test3 <- trigrams.tfidf.75[-train3.indexes,]
prop.table(table(test3$Label))

# Kontrolna funkcija za 10fold cross-validation
ctrl3 <- trainControl(method = "repeatedcv",
                      number = 10,
                      repeats = 2,
                      verboseIter = TRUE,
                      summaryFunction = twoClassSummary,
                      classProbs = TRUE)

# Treniramo trening set koriscenjem Naive Bayes metode
start.time <- Sys.time()

set.seed(5627)
orig_fit3 <- train(Label ~ .,
                   data = train3,
                   method = "naive_bayes",
                   verbose = FALSE,
                   metric = "ROC",
                   trControl = trainControl(method="none", classProbs = TRUE),
                   tuneGrid = data.frame(usekernel=FALSE,laplace =1,adjust=FALSE))

total.time <- Sys.time() - start.time
total.time

# Build custom AUC function to extract AUC
# from the caret model object

orig_fit3 %>%
  test_roc(data = test3) %>%
  auc()

# Use the same seed to ensure same cross-validation splits

ctrl3$seeds <- orig_fit3$control$seeds

# Build down-sampled model

ctrl2$sampling <- "down"

down_fit3 <- train(Label ~ .,
                   data = train3,
                   method = "naive_bayes",
                   verbose = FALSE,
                   metric = "ROC",
                   trControl = trainControl(method="none", classProbs = TRUE, 
                                            seeds = orig_fit3$control$seeds,
                                            sampling = "down"),
                   tuneGrid = data.frame(usekernel=FALSE,laplace=1,adjust=FALSE))


# Build smote model

ctrl3$sampling <- "smote"

smote_fit3 <- train(Label ~ .,
                    data = train3,
                    method = "naive_bayes",
                    verbose = FALSE,
                    metric = "ROC",
                    trControl = trainControl(method="none", classProbs = TRUE, 
                                             seeds = orig_fit3$control$seeds,
                                             sampling = "smote"),
                    tuneGrid = data.frame(usekernel=FALSE,laplace=1,adjust=FALSE))

# Examine results for test set

model_list3 <- list(original = orig_fit3,
                    down = down_fit3,
                    SMOTE = smote_fit3)

model_list_roc3 <- model_list3 %>%
  map(test_roc, data = test3)

model_list_roc3 %>%
  map(auc)


########################################################################
# KONFIGURACIJA 4 sa podkonfiguracijama original, down-sampling i SMOTE 
########################################################################

set.seed(1010)
train.indexes4 <- createDataPartition(unigrams.tfidf.90$Label, p = .80, list = FALSE)
train4 <- unigrams.tfidf.90[train.indexes4,]
test4 <- unigrams.tfidf.90[-train.indexes4,]
prop.table(table(test4$Label))

ctrl4 <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 2,
                     verboseIter = TRUE,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE)

# Treniramo trening set koriscenjem Naive Bayes metode
start.time <- Sys.time()

set.seed(5627)
orig_fit4 <- train(Label ~ .,
                  data = train4,
                  method = "nb",
                  verbose = FALSE,
                  metric = "ROC",
                  trControl = trainControl(method="none", classProbs = TRUE),
                  tuneGrid = data.frame(usekernel=FALSE,fL=0,adjust=FALSE))

total.time <- Sys.time() - start.time
total.time

# Build custom AUC function to extract AUC
# from the caret model object

orig_fit4 %>%
  test_roc(data = test4) %>%
  auc()

# Use the same seed to ensure same cross-validation splits

ctrl4$seeds <- orig_fit4$control$seeds

# Build down-sampled model

ctrl4$sampling <- "down"

down_fit4 <- train(Label ~ .,
                   data = train4,
                   method = "nb",
                   verbose = FALSE,
                   metric = "ROC",
                   trControl = trainControl(method="none", classProbs = TRUE, 
                                            seeds = orig_fit4$control$seeds,
                                            sampling = "down"),
                   tuneGrid = data.frame(usekernel=FALSE,fL=0,adjust=FALSE))


# Build smote model

ctrl4$sampling <- "smote"

smote_fit4 <- train(Label ~ .,
                    data = train4,
                    method = "nb",
                    verbose = FALSE,
                    metric = "ROC",
                    trControl = trainControl(method="none", classProbs = TRUE, 
                                             seeds = orig_fit4$control$seeds,
                                             sampling = "smote"),
                    tuneGrid = data.frame(usekernel=FALSE,fL=0,adjust=FALSE))

# Examine results for test set

model_list4 <- list(original = orig_fit4,
                   down = down_fit4,
                   SMOTE = smote_fit4)

model_list_roc4 <- model_list4 %>%
  map(test_roc, data = test4)

model_list_roc4 %>%
  map(auc)


########################################################################
# KONFIGURACIJA 5 sa podkonfiguracijama original, down-sampling i SMOTE 
########################################################################

set.seed(1010)
train5.indexes <- createDataPartition(bigrams.tfidf.90$Label, p = .80, list = FALSE)
train5 <- bigrams.tfidf.90[train5.indexes,]
test5 <- bigrams.tfidf.90[-train5.indexes,]
prop.table(table(test5$Label))

# Kontrolna funkcija za 10fold cross-validation
ctrl5 <- trainControl(method = "repeatedcv",
                      number = 10,
                      repeats = 2,
                      verboseIter = TRUE,
                      summaryFunction = twoClassSummary,
                      classProbs = TRUE)

# Treniramo trening set koriscenjem Naive Bayes metode
start.time <- Sys.time()

set.seed(5627)
orig_fit5 <- train(Label ~ .,
                   data = train5,
                   method = "nb",
                   verbose = FALSE,
                   metric = "ROC",
                   trControl = trainControl(method="none", classProbs = TRUE),
                   tuneGrid = data.frame(usekernel=FALSE,fL=0,adjust=FALSE))

total.time <- Sys.time() - start.time
total.time

# Build custom AUC function to extract AUC
# from the caret model object

orig_fit5 %>%
  test_roc(data = test5) %>%
  auc()

# Use the same seed to ensure same cross-validation splits

ctrl5$seeds <- orig_fit5$control$seeds

# Build down-sampled model

ctrl5$sampling <- "down"

down_fit5 <- train(Label ~ .,
                   data = train5,
                   method = "nb",
                   verbose = FALSE,
                   metric = "ROC",
                   trControl = trainControl(method="none", classProbs = TRUE, 
                                            seeds = orig_fit5$control$seeds,
                                            sampling = "down"),
                   tuneGrid = data.frame(usekernel=FALSE,fL=0,adjust=FALSE))


# Build smote model

ctrl5$sampling <- "smote"

smote_fit5 <- train(Label ~ .,
                    data = train5,
                    method = "nb",
                    verbose = FALSE,
                    metric = "ROC",
                    trControl = trainControl(method="none", classProbs = TRUE, 
                                             seeds = orig_fit5$control$seeds,
                                             sampling = "smote"),
                    tuneGrid = data.frame(usekernel=FALSE,fL=0,adjust=FALSE))

# Examine results for test set

model_list5 <- list(original = orig_fit5,
                    down = down_fit5,
                    SMOTE = smote_fit5)

model_list_roc5 <- model_list5 %>%
  map(test_roc, data = test5)

model_list_roc5 %>%
  map(auc)

########################################################################
# KONFIGURACIJA 6 sa podkonfiguracijama original, down-sampling i SMOTE 
########################################################################

set.seed(1234)
train6.indexes <- createDataPartition(trigrams.tfidf.90$Label, p = .80, list = FALSE)
train6 <- trigrams.tfidf.90[train6.indexes,]
test6 <- trigrams.tfidf.90[-train6.indexes,]
prop.table(table(test6$Label))

# Kontrolna funkcija za 10fold cross-validation
ctrl6 <- trainControl(method = "repeatedcv",
                      number = 10,
                      repeats = 2,
                      verboseIter = TRUE,
                      summaryFunction = twoClassSummary,
                      classProbs = TRUE)

# Treniramo trening set koriscenjem Naive Bayes metode
start.time <- Sys.time()

set.seed(5627)
orig_fit6 <- train(Label ~ .,
                   data = train6,
                   method = "nb",
                   verbose = FALSE,
                   metric = "ROC",
                   trControl = trainControl(method="none", classProbs = TRUE),
                   tuneGrid = data.frame(usekernel=FALSE,fL=0,adjust=FALSE))

total.time <- Sys.time() - start.time
total.time

# Build custom AUC function to extract AUC
# from the caret model object

orig_fit6 %>%
  test_roc(data = test6) %>%
  auc()

# Use the same seed to ensure same cross-validation splits

ctrl6$seeds <- orig_fit6$control$seeds

# Build down-sampled model

ctrl6$sampling <- "down"

down_fit6 <- train(Label ~ .,
                   data = train6,
                   method = "nb",
                   verbose = FALSE,
                   metric = "ROC",
                   trControl = trainControl(method="none", classProbs = TRUE, 
                                            seeds = orig_fit6$control$seeds,
                                            sampling = "down"),
                   tuneGrid = data.frame(usekernel=FALSE,fL=0,adjust=FALSE))


# Build smote model

ctrl6$sampling <- "smote"

smote_fit6 <- train(Label ~ .,
                    data = train6,
                    method = "nb",
                    verbose = FALSE,
                    metric = "ROC",
                    trControl = trainControl(method="none", classProbs = TRUE, 
                                             seeds = orig_fit6$control$seeds,
                                             sampling = "smote"),
                    tuneGrid = data.frame(usekernel=FALSE,fL=0,adjust=FALSE))

# Examine results for test set

model_list6 <- list(original = orig_fit6,
                    down = down_fit6,
                    SMOTE = smote_fit6)

model_list_roc6 <- model_list6 %>%
  map(test_roc, data = test6)

model_list_roc6 %>%
  map(auc)


