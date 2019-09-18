#install.packages(c("tm","quanteda"))
#install.packages("tidyr") #koriscena fja unite
#install.packages('SnowballC')
#install.packages('nortest')
#install.packages(c("dplyr","DMwR","purr"))
#install.packages("doSNOW")
#install.packages("klaR")
library(dplyr) # for data manipulation
library(caret) # for model-building
library(DMwR) # for smote implementation
library(purrr) # for functional programming (map)
library(pROC) # for AUC calculations
library(klaR)
library(e1071)
library(doSNOW)
library(nortest)
library(SnowballC)
library(quanteda)
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
#njih izbacujemo iz daljeg razmatranja

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
corpus_review <- tm_map(corpus_review , stemDocument, language = "english")

#####################################################
#KREIRANJE DTM, TF-IDF za unigrame, bigrame, trigrame
#####################################################

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
dataframe <- convert(review.tokens.dfm.tfidf, to="data.frame")
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
# Function to convert the word frequencies to yes (presence) and no (absence) labels
convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}

#top 1000 unigrama
unigrams.top1000 <- dfm_select(review.tokens.dfm.tfidf, names(topfeatures(review.tokens.dfm.tfidf, n = 1000, decreasing = TRUE)))
###########
#provera da li imamo redove koji su svi 0
zeros <- which(rowSums(unigrams.top1000)==0) #nema takvih redova
############
unigrams.top1000.nb <- apply(unigrams.top1000, MARGIN = 2, convert_count)
unigrams.top1000.nb <- as.data.frame(unigrams.top1000.nb) 
unigrams.top1000.nb <- cbind(Label = ecommerce_reviews$Label, unigrams.top1000.nb)

unigrams.top1000.dt <- as.data.frame(convert(unigrams.top1000, to = "matrix")) 
unigrams.top1000.dt <- cbind(Label = ecommerce_reviews$Label, unigrams.top1000.dt)

#top 2000 unigrama
unigrams.top2000 <- dfm_select(review.tokens.dfm.tfidf, names(topfeatures(review.tokens.dfm.tfidf, n = 2000, decreasing = TRUE)))
###########
#provera da li imamo redove koji su svi 0
zeros <- which(rowSums(unigrams.top2000)==0) #nema takvih redova
############
unigrams.top2000.nb <- apply(unigrams.top2000, MARGIN = 2, convert_count)
unigrams.top2000.nb <- as.data.frame(unigrams.top2000.nb) 
unigrams.top2000.nb <- cbind(Label = ecommerce_reviews$Label, unigrams.top2000.nb)

unigrams.top2000.dt <- as.data.frame(convert(unigrams.top2000, to = "matrix")) 
unigrams.top2000.dt <- cbind(Label = ecommerce_reviews$Label, unigrams.top2000.dt)

dim(unigrams.top1000.nb)
dim(unigrams.top2000.nb)

dim(unigrams.top1000.dt)
dim(unigrams.top2000.dt)


##############################
#Kreiramo bigrame
##############################
bigrams <- tokens_ngrams(review.tokens, n = 2:2)
#Prikaz bigrama 123. dokumenta
bigrams[[123]]
#Kreiramo DFM od bigrama
review.bigrams.dfm <- dfm(bigrams, tolower = FALSE)

dim(review.bigrams.dfm)

#TF-IDF od bigrama sa labelama
review.bigrams.dfm.tfidf <- dfm_tfidf(review.bigrams.dfm, scheme_tf = "prop",
                                      scheme_df = "inverse", base = 10)
dim(review.bigrams.dfm.tfidf)

#top 1000 bigrama
bigrams.top1000 <- dfm_select(review.bigrams.dfm.tfidf, names(topfeatures(review.bigrams.dfm.tfidf, n = 1000, decreasing = TRUE)))
###########
#provera da li imamo redove koji su svi 0
zeros <- which(rowSums(bigrams.top1000)==0)
############
bigrams.top1000.nb <- apply(bigrams.top1000, MARGIN = 2, convert_count)
bigrams.top1000.nb <- as.data.frame(bigrams.top1000.nb) 
bigrams.top1000.nb <- cbind(Label = ecommerce_reviews$Label,bigrams.top1000.nb)

bigrams.top1000.dt <- as.data.frame(convert(bigrams.top1000, to = "matrix")) 
bigrams.top1000.dt <- cbind(Label = ecommerce_reviews$Label,bigrams.top1000.dt)
##########
#uklanjanje redova koji su celi 0
bigrams.top1000.nb <- bigrams.top1000.nb[-zeros,]
bigrams.top1000.dt <- bigrams.top1000.dt[-zeros,]
##########
dim(bigrams.top1000.nb)
dim(bigrams.top1000.dt)

#top 2000 bigrama
bigrams.top2000 <- dfm_select(review.bigrams.dfm.tfidf, names(topfeatures(review.bigrams.dfm.tfidf, n = 2000, decreasing = TRUE)))
###########
#provera da li imamo redove koji su svi 0
zeros <- which(rowSums(bigrams.top2000)==0)
############
bigrams.top2000.nb <- apply(bigrams.top2000, MARGIN = 2, convert_count)
bigrams.top2000.nb <- as.data.frame(bigrams.top2000.nb) 
bigrams.top2000.nb <- cbind(Label = ecommerce_reviews$Label,bigrams.top2000.nb)

bigrams.top2000.dt <- as.data.frame(convert(bigrams.top2000, to = "matrix")) 
bigrams.top2000.dt <- cbind(Label = ecommerce_reviews$Label,bigrams.top2000.dt)

##########
#uklanjanje redova koji su celi 0
bigrams.top2000.nb <- bigrams.top2000.nb[-zeros,]
bigrams.top2000.dt <- bigrams.top2000.dt[-zeros,]
##########
dim(bigrams.top2000.nb)
dim(bigrams.top2000.dt)

##############################
#Kreiramo trigrame
##############################

trigrams <- tokens_ngrams(review.tokens, n = 3:3)
#Prikaz trigrama 123. dokumenta
trigrams[[123]]
#Kreiramo DFM od trigrama
review.trigrams.dfm <- dfm(trigrams, tolower = FALSE)
dim(review.trigrams.dfm)

#TF-IDF od trigrama sa labelama
review.trigrams.dfm.tfidf <- dfm_tfidf(review.trigrams.dfm, scheme_tf = "prop",
                                       scheme_df = "inverse", base = 10)
dim(review.trigrams.dfm.tfidf)

#top 1000 trigrama
trigrams.top1000 <- dfm_select(review.trigrams.dfm.tfidf, names(topfeatures(review.trigrams.dfm.tfidf, n = 1000, decreasing = TRUE)))
###########
#provera da li imamo redove koji su svi 0
zeros <- which(rowSums(trigrams.top1000)==0)
############
trigrams.top1000.nb <- apply(trigrams.top1000, MARGIN = 2, convert_count)
trigrams.top1000.nb <- as.data.frame(trigrams.top1000.nb) 
trigrams.top1000.nb <- cbind(Label = ecommerce_reviews$Label,trigrams.top1000.nb)

trigrams.top1000.dt <- as.data.frame(convert(trigrams.top1000, to = "matrix")) 
trigrams.top1000.dt <- cbind(Label = ecommerce_reviews$Label,trigrams.top1000.dt)
##########
#uklanjanje redova koji su celi 0
trigrams.top1000.nb <- trigrams.top1000.nb[-zeros,]
trigrams.top1000.dt <- trigrams.top1000.dt[-zeros,]
##########
dim(trigrams.top1000.nb)
dim(trigrams.top1000.dt)

#top 2000 trigrama
trigrams.top2000 <- dfm_select(review.trigrams.dfm.tfidf, names(topfeatures(review.trigrams.dfm.tfidf, n = 2000, decreasing = TRUE)))
###########
#provera da li imamo redove koji su svi 0
zeros <- which(rowSums(trigrams.top2000)==0)
############
trigrams.top2000.nb <- apply(trigrams.top2000, MARGIN = 2, convert_count)
trigrams.top2000.nb <- as.data.frame(trigrams.top2000.nb) 
trigrams.top2000.nb <- cbind(Label = ecommerce_reviews$Label,trigrams.top2000.nb)

trigrams.top2000.dt <- as.data.frame(convert(trigrams.top2000, to = "matrix")) 
trigrams.top2000.dt <- cbind(Label = ecommerce_reviews$Label,trigrams.top2000.dt)
##########
#uklanjanje redova koji su celi 0
trigrams.top2000.nb <- trigrams.top2000.nb[-zeros,]
trigrams.top2000.dt <- trigrams.top2000.dt[-zeros,]
##########
dim(trigrams.top2000.nb)
dim(trigrams.top2000.dt)

########################################################################
# KONFIGURACIJE
########################################################################
########################################################################
# KONFIGURACIJA 1 sa podkonfiguracijama original i SMOTE 
########################################################################

set.seed(1010)
train.indexes <- createDataPartition(unigrams.top1000.nb$Label, p = .80, list = FALSE)
train1 <- unigrams.top1000.nb[train.indexes,]
test1 <- unigrams.top1000.nb[-train.indexes,]
prop.table(table(test1$Label))

# Kontrolna funkcija za 10fold cross-validation

ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 2,
                     verboseIter = TRUE,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE)


# Treniramo trening set koriscenjem Naive Bayes metode
start.time <- Sys.time()
cl <- makeCluster(7, type = "SOCK")
registerDoSNOW(cl)
set.seed(5627)
orig_fit <- train(x = train1[,-1],
                  y = train1$Label,
                  method = "nb",
                  verbose = FALSE,
                  metric = "ROC",
                  trControl = ctrl,
                  tuneGrid = data.frame(usekernel=FALSE,fL=1,adjust=FALSE))
stopCluster(cl)
total.time <- Sys.time() - start.time
total.time

# Build custom AUC function to extract AUC
# from the caret model object
test_roc <- function(model, data) {
  roc(data$Label,
      predict(model, data, type = "prob")[, "NEG"])
}

# Use the same seed to ensure same cross-validation splits

ctrl$seeds <- orig_fit$control$seeds

# Build smote model

ctrl$sampling <- "smote"
start.time <- Sys.time()
cl <- makeCluster(7, type = "SOCK")
registerDoSNOW(cl)
smote_fit <- train(x = train1[,-1],
                   y = train1$Label,
                   method = "nb",
                   verbose = FALSE,
                   metric = "ROC",
                   trControl = ctrl,
                   tuneGrid = data.frame(usekernel=FALSE,fL=1,adjust=FALSE))
stopCluster(cl)
total.time <- Sys.time() - start.time
total.time
# Examine results for test set

model_list <- list(original = orig_fit,
                   SMOTE = smote_fit)

model_list_roc <- model_list %>%
  map(test_roc, data = test1)

model_list_roc %>%
  map(auc)


########################################################################
# KONFIGURACIJA 2 sa podkonfiguracijama original i SMOTE 
########################################################################

set.seed(1010)
train2.indexes <- createDataPartition(bigrams.top1000.nb$Label, p = .80, list = FALSE)
train2 <- bigrams.top1000.nb[train2.indexes,]
test2 <- bigrams.top1000.nb[-train2.indexes,]
prop.table(table(test2$Label))

# Kontrolna funkcija za 10fold cross-validation
ctrl2 <- trainControl(method = "repeatedcv",
                      number = 10,
                      repeats = 2,
                      verboseIter = TRUE,
                      summaryFunction = twoClassSummary,
                      classProbs = TRUE)

# Treniramo trening set koriscenjem Naive Bayes metode
start.time <- Sys.time()
cl <- makeCluster(7, type = "SOCK")
registerDoSNOW(cl)
set.seed(5627)
orig_fit2 <- train(x = train2[,-1],
                   y = train2$Label,
                   method = "nb",
                   verbose = FALSE,
                   metric = "ROC",
                   trControl = ctrl2,
                   tuneGrid = data.frame(usekernel=FALSE,fL=1,adjust=FALSE))
stopCluster(cl)
total.time <- Sys.time() - start.time
total.time

# Use the same seed to ensure same cross-validation splits

ctrl2$seeds <- orig_fit2$control$seeds

# Build smote model

ctrl2$sampling <- "smote"
start.time <- Sys.time()
cl <- makeCluster(7, type = "SOCK")
registerDoSNOW(cl)
smote_fit2 <- train(x = train2[,-1],
                    y = train2$Label,
                    method = "nb",
                    verbose = FALSE,
                    metric = "ROC",
                    trControl = ctrl2,
                    tuneGrid = data.frame(usekernel=FALSE,fL=1,adjust=FALSE))
stopCluster(cl)
total.time <- Sys.time() - start.time
total.time
# Examine results for test set

model_list2 <- list(original = orig_fit2,
                    SMOTE = smote_fit2)

model_list_roc2 <- model_list2 %>%
  map(test_roc, data = test2)

model_list_roc2 %>%
  map(auc)


########################################################################
# KONFIGURACIJA 3 sa podkonfiguracijama original i SMOTE 
########################################################################

set.seed(1234)
train3.indexes <- createDataPartition(trigrams.top1000.nb$Label, p = .80, list = FALSE)
train3 <- trigrams.top1000.nb[train3.indexes,]
test3 <- trigrams.top1000.nb[-train3.indexes,]
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
cl <- makeCluster(7, type = "SOCK")
registerDoSNOW(cl)
set.seed(5627)
orig_fit3 <- train(x = train3[,-1],
                   y = train3$Label,
                   method = "nb",
                   verbose = FALSE,
                   metric = "ROC",
                   trControl = ctrl3,
                   tuneGrid = data.frame(usekernel=FALSE,fL=1,adjust=FALSE))
stopCluster(cl)
total.time <- Sys.time() - start.time
total.time

# Use the same seed to ensure same cross-validation splits

ctrl3$seeds <- orig_fit3$control$seeds

# Build smote model

ctrl3$sampling <- "smote"
start.time <- Sys.time()
cl <- makeCluster(7, type = "SOCK")
registerDoSNOW(cl)
smote_fit3 <- train(x = train3[,-1],
                    y = train3$Label,
                    method = "nb",
                    verbose = FALSE,
                    metric = "ROC",
                    trControl = ctrl3,
                    tuneGrid = data.frame(usekernel=FALSE,fL=1,adjust=FALSE))
stopCluster(cl)
total.time <- Sys.time() - start.time
total.time
# Examine results for test set

model_list3 <- list(original = orig_fit3,
                    SMOTE = smote_fit3)

model_list_roc3 <- model_list3 %>%
  map(test_roc, data = test3)

model_list_roc3 %>%
  map(auc)


########################################################################
# KONFIGURACIJA 4 sa podkonfiguracijama original i SMOTE 
########################################################################

set.seed(1010)
train4.indexes <- createDataPartition(unigrams.top2000.nb$Label, p = .80, list = FALSE)
train4 <- unigrams.top2000.nb[train4.indexes,]
test4 <- unigrams.top2000.nb[-train4.indexes,]
prop.table(table(test4$Label))

ctrl4 <- trainControl(method = "repeatedcv",
                      number = 10,
                      repeats = 2,
                      verboseIter = TRUE,
                      summaryFunction = twoClassSummary,
                      classProbs = TRUE)

# Treniramo trening set koriscenjem Naive Bayes metode
start.time <- Sys.time()
cl <- makeCluster(7, type = "SOCK")
registerDoSNOW(cl)
set.seed(5627)
orig_fit4 <- train(x = train4[,-1],
                   y = train4$Label,
                   method = "nb",
                   verbose = FALSE,
                   metric = "ROC",
                   trControl = ctrl4,
                   tuneGrid = data.frame(usekernel=FALSE,fL=1,adjust=FALSE))
stopCluster(cl)
total.time <- Sys.time() - start.time
total.time

# Use the same seed to ensure same cross-validation splits

ctrl4$seeds <- orig_fit4$control$seeds

# Build smote model

ctrl4$sampling <- "smote"
start.time <- Sys.time()
cl <- makeCluster(7, type = "SOCK")
registerDoSNOW(cl)
smote_fit4 <- train(x = train4[,-1],
                    y = train4$Label,
                    method = "nb",
                    verbose = FALSE,
                    metric = "ROC",
                    trControl = ctrl4,
                    tuneGrid = data.frame(usekernel=FALSE,fL=1,adjust=FALSE))
stopCluster(cl)
total.time <- Sys.time() - start.time
total.time


# Examine results for test set

model_list4 <- list(original = orig_fit4)#,
#SMOTE = smote_fit4)

model_list_roc4 <- model_list4 %>%
  map(test_roc, data = test4)

model_list_roc4 %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc4){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve for original

custom_col <- c("#009E73")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

predikcije <- predict(orig_fit4, test4)
matricaKonf <- confusionMatrix(predikcije, test4$Label)

########################################################################
# KONFIGURACIJA 5 sa podkonfiguracijama original i SMOTE 
########################################################################

set.seed(1010)
train5.indexes <- createDataPartition(bigrams.top2000.nb$Label, p = .80, list = FALSE)
train5 <- bigrams.top2000.nb[train5.indexes,]
test5 <- bigrams.top2000.nb[-train5.indexes,]
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
cl <- makeCluster(7, type = "SOCK")
registerDoSNOW(cl)
set.seed(5627)
orig_fit5 <- train(x = train5[,-1],
                   y = train5$Label,
                   method = "nb",
                   verbose = FALSE,
                   metric = "ROC",
                   trControl = ctrl5,
                   tuneGrid = data.frame(usekernel=FALSE,fL=1,adjust=FALSE))
stopCluster(cl)
total.time <- Sys.time() - start.time
total.time

# Use the same seed to ensure same cross-validation splits

ctrl5$seeds <- orig_fit5$control$seeds

# Build smote model

ctrl5$sampling <- "smote"
start.time <- Sys.time()
cl <- makeCluster(7, type = "SOCK")
registerDoSNOW(cl)
smote_fit5 <- train(x = train5[,-1],
                    y = train5$Label,
                    method = "nb",
                    verbose = FALSE,
                    metric = "ROC",
                    trControl = ctrl5,
                    tuneGrid = data.frame(usekernel=FALSE,fL=1,adjust=FALSE))
stopCluster(cl)
total.time <- Sys.time() - start.time
total.time
# Examine results for test set

model_list5 <- list(original = orig_fit5,
                    SMOTE = smote_fit5)

model_list_roc5 <- model_list5 %>%
  map(test_roc, data = test5)

model_list_roc5 %>%
  map(auc)

########################################################################
# KONFIGURACIJA 6 sa podkonfiguracijama original i SMOTE 
########################################################################

set.seed(1234)
train6.indexes <- createDataPartition(trigrams.top2000.nb$Label, p = .80, list = FALSE)
train6 <- trigrams.top2000.nb[train6.indexes,]
test6 <- trigrams.top2000.nb[-train6.indexes,]
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
cl <- makeCluster(7, type = "SOCK")
registerDoSNOW(cl)
set.seed(5627)
orig_fit6 <- train(x = train6[,-1],
                   y = train6$Label,
                   method = "nb",
                   verbose = FALSE,
                   metric = "ROC",
                   trControl = ctrl6,
                   tuneGrid = data.frame(usekernel=FALSE,fL=1,adjust=FALSE))
stopCluster(cl)
total.time <- Sys.time() - start.time
total.time

# Use the same seed to ensure same cross-validation splits

ctrl6$seeds <- orig_fit6$control$seeds

# Build smote model

ctrl6$sampling <- "smote"
start.time <- Sys.time()
cl <- makeCluster(7, type = "SOCK")
registerDoSNOW(cl)
smote_fit6 <- train(x = train6[,-1],
                    y = train6$Label,
                    method = "nb",
                    verbose = FALSE,
                    metric = "ROC",
                    trControl = ctrl6,
                    tuneGrid = data.frame(usekernel=FALSE,fL=1,adjust=FALSE))
stopCluster(cl)
total.time <- Sys.time() - start.time
total.time
# Examine results for test set

model_list6 <- list(original = orig_fit6,
                    SMOTE = smote_fit6)

model_list_roc6 <- model_list6 %>%
  map(test_roc, data = test6)

model_list_roc6 %>%
  map(auc)


########################################################################
# KONFIGURACIJA 7 sa podkonfiguracijama original i SMOTE 
########################################################################

set.seed(1010)
train7.indexes <- createDataPartition(unigrams.top1000.dt$Label, p = .80, list = FALSE)
train7 <- unigrams.top1000.dt[train7.indexes,]
test7 <- unigrams.top1000.dt[-train7.indexes,]
prop.table(table(test7$Label))

# Kontrolna funkcija za 10fold cross-validation

ctrl7 <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 2,
                     verboseIter = TRUE,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE)


# Treniramo trening set koriscenjem rpart metode
start.time <- Sys.time()
cl <- makeCluster(7, type = "SOCK")
registerDoSNOW(cl)
set.seed(5627)
orig_fit7 <- train(x = train7[,-1],
                  y = train7$Label,
                  method = "rpart",
                  metric = "ROC",
                  trControl = ctrl7,
                  tuneGrid = expand.grid(.cp=seq(0.001,0.5,0.001)))
stopCluster(cl)
total.time <- Sys.time() - start.time
total.time

# Use the same seed to ensure same cross-validation splits

ctrl7$seeds <- orig_fit7$control$seeds

# Build smote model
ctrl7$sampling <- "smote"
start.time <- Sys.time()
cl <- makeCluster(7, type = "SOCK")
registerDoSNOW(cl)
smote_fit7 <- train(x = train7[,-1],
                   y = train7$Label,
                   method = "rpart",
                   metric = "ROC",
                   trControl = ctrl7,
                   tuneGrid = expand.grid(.cp=seq(0.001,0.5,0.001)))
stopCluster(cl)
total.time <- Sys.time() - start.time
total.time
# Examine results for test set

model_list7 <- list(original = orig_fit7,
                   SMOTE = smote_fit7)

model_list_roc7 <- model_list7 %>%
  map(test_roc, data = test7)

model_list_roc7 %>%
  map(auc)

########################################################################
# KONFIGURACIJA 8 sa podkonfiguracijama original i SMOTE 
########################################################################

set.seed(1010)
train8.indexes <- createDataPartition(bigrams.top1000.dt$Label, p = .80, list = FALSE)
train8 <- bigrams.top1000.dt[train8.indexes,]
test8 <- bigrams.top1000.dt[-train8.indexes,]
prop.table(table(test8$Label))

# Kontrolna funkcija za 10fold cross-validation
ctrl8 <- trainControl(method = "repeatedcv",
                      number = 10,
                      repeats = 2,
                      verboseIter = TRUE,
                      summaryFunction = twoClassSummary,
                      classProbs = TRUE)

# Treniramo trening set koriscenjem rpart metode
start.time <- Sys.time()
cl <- makeCluster(7, type = "SOCK")
registerDoSNOW(cl)
set.seed(5627)
orig_fit8 <- train(Label ~ .,
                   data = train8,
                   method = "rpart",
                   metric = "ROC",
                   trControl = ctrl8,
                   tuneGrid = expand.grid(.cp=seq(0.001,0.5,0.001)))
stopCluster(cl)
total.time <- Sys.time() - start.time
total.time

# Use the same seed to ensure same cross-validation splits

ctrl8$seeds <- orig_fit8$control$seeds

# Build smote model

ctrl8$sampling <- "smote"
start.time <- Sys.time()
cl <- makeCluster(7, type = "SOCK")
registerDoSNOW(cl)
smote_fit8 <- train(Label ~ .,
                    data = train8,
                    method = "rpart",
                    metric = "ROC",
                    trControl = ctrl8,
                    tuneGrid = expand.grid(.cp=seq(0.001,0.5,0.001)))
stopCluster(cl)
total.time <- Sys.time() - start.time
total.time
# Examine results for test set

model_list8 <- list(original = orig_fit8,
                    SMOTE = smote_fit8)

model_list_roc8 <- model_list8 %>%
  map(test_roc, data = test8)

model_list_roc8 %>%
  map(auc)


########################################################################
# KONFIGURACIJA 9 sa podkonfiguracijama original i SMOTE 
########################################################################

set.seed(1234)
train9.indexes <- createDataPartition(trigrams.top1000.dt$Label, p = .80, list = FALSE)
train9 <- trigrams.top1000.dt[train9.indexes,]
test9 <- trigrams.top1000.dt[-train9.indexes,]
prop.table(table(test9$Label))

# Kontrolna funkcija za 10fold cross-validation
ctrl9 <- trainControl(method = "repeatedcv",
                      number = 10,
                      repeats = 2,
                      verboseIter = TRUE,
                      summaryFunction = twoClassSummary,
                      classProbs = TRUE)

# Treniramo trening set koriscenjem rpart metode
start.time <- Sys.time()
cl <- makeCluster(7, type = "SOCK")
registerDoSNOW(cl)
set.seed(5627)
orig_fit9 <- train(x = train9[,-1],
                   y = train9$Label,
                   method = "rpart",
                   metric = "ROC",
                   trControl = ctrl9,
                   tuneGrid = expand.grid(.cp=seq(0.001,0.5,0.001)))
stopCluster(cl)
total.time <- Sys.time() - start.time
total.time

# Use the same seed to ensure same cross-validation splits

ctrl9$seeds <- orig_fit9$control$seeds

# Build smote model

ctrl9$sampling <- "smote"
start.time <- Sys.time()
cl <- makeCluster(7, type = "SOCK")
registerDoSNOW(cl)
smote_fit9 <- train(x = train9[,-1],
                    y = train9$Label,
                    method = "rpart",
                    metric = "ROC",
                    trControl = ctrl9,
                    tuneGrid = expand.grid(.cp=seq(0.001,0.5,0.001)))
stopCluster(cl)
total.time <- Sys.time() - start.time
total.time
# Examine results for test set

model_list9 <- list(original = orig_fit9,
                    SMOTE = smote_fit9)

model_list_roc9 <- model_list9 %>%
  map(test_roc, data = test9)

model_list_roc9 %>%
  map(auc)


########################################################################
# KONFIGURACIJA 10 sa podkonfiguracijama original i SMOTE 
########################################################################

set.seed(1010)
train10.indexes <- createDataPartition(unigrams.top2000.dt$Label, p = .80, list = FALSE)
train10 <- unigrams.top2000.dt[train10.indexes,]
test10 <- unigrams.top2000.dt[-train10.indexes,]
prop.table(table(test10$Label))

ctrl10 <- trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 2,
                       verboseIter = TRUE,
                       summaryFunction = twoClassSummary,
                       classProbs = TRUE)

# Treniramo trening set koriscenjem rpart metode
start.time <- Sys.time()
cl <- makeCluster(7, type = "SOCK")
registerDoSNOW(cl)
set.seed(5627)
orig_fit10 <- train(x = train10[,-1],
                    y = train10$Label,
                    method = "rpart",
                    metric = "ROC",
                    trControl = ctrl10,
                    tuneGrid = expand.grid(.cp=seq(0.001,0.5,0.001)))
stopCluster(cl)
total.time <- Sys.time() - start.time
total.time

# Use the same seed to ensure same cross-validation splits

ctrl10$seeds <- orig_fit10$control$seeds

# Build smote model

ctrl10$sampling <- "smote"
start.time <- Sys.time()
cl <- makeCluster(7, type = "SOCK")
registerDoSNOW(cl)
smote_fit10 <- train(x = train10[,-1],
                     y = train10$Label,
                     method = "rpart",
                     metric = "ROC",
                     trControl = ctrl10,
                     tuneGrid = expand.grid(.cp=seq(0.001,0.5,0.001)))
stopCluster(cl)
total.time <- Sys.time() - start.time
total.time

# Examine results for test set

model_list10 <- list(original = orig_fit10,
                     SMOTE = smote_fit10)

model_list_roc10 <- model_list10 %>%
  map(test_roc, data = test10)

model_list_roc10 %>%
  map(auc)


########################################################################
# KONFIGURACIJA 11 sa podkonfiguracijama original i SMOTE 
########################################################################

set.seed(1010)
train11.indexes <- createDataPartition(bigrams.top2000.dt$Label, p = .80, list = FALSE)
train11 <- bigrams.top2000.dt[train11.indexes,]
test11 <- bigrams.top2000.dt[-train11.indexes,]
prop.table(table(test11$Label))

# Kontrolna funkcija za 10fold cross-validation
ctrl11 <- trainControl(method = "repeatedcv",
                      number = 10,
                      repeats = 2,
                      verboseIter = TRUE,
                      summaryFunction = twoClassSummary,
                      classProbs = TRUE)

# Treniramo trening set koriscenjem rpart metode
start.time <- Sys.time()
cl <- makeCluster(7, type = "SOCK")
registerDoSNOW(cl)
set.seed(5627)
orig_fit11 <- train(x = train11[,-1],
                   y = train11$Label,
                   method = "rpart",
                   metric = "ROC",
                   trControl = ctrl11,
                   tuneGrid = expand.grid(.cp=seq(0.001,0.5,0.001)))
stopCluster(cl)
total.time <- Sys.time() - start.time
total.time

# Use the same seed to ensure same cross-validation splits

ctrl11$seeds <- orig_fit11$control$seeds

# Build smote model

ctrl11$sampling <- "smote"
start.time <- Sys.time()
cl <- makeCluster(7, type = "SOCK")
registerDoSNOW(cl)
smote_fit11 <- train(x = train11[,-1],
                    y = train11$Label,
                    method = "rpart",
                    metric = "ROC",
                    trControl = ctrl11,
                    tuneGrid = expand.grid(.cp=seq(0.001,0.5,0.001)))
stopCluster(cl)
total.time <- Sys.time() - start.time
total.time

# Examine results for test set

model_list11 <- list(original = orig_fit11,
                    SMOTE = smote_fit11)

model_list_roc11 <- model_list11 %>%
  map(test_roc, data = test11)

model_list_roc11 %>%
  map(auc)

########################################################################
# KONFIGURACIJA 12 sa podkonfiguracijama original i SMOTE 
########################################################################

set.seed(1234)
train12.indexes <- createDataPartition(trigrams.top2000.dt$Label, p = .80, list = FALSE)
train12 <- trigrams.top2000.dt[train12.indexes,]
test12 <- trigrams.top2000.dt[-train12.indexes,]
prop.table(table(test12$Label))

# Kontrolna funkcija za 10fold cross-validation
ctrl12 <- trainControl(method = "repeatedcv",
                      number = 10,
                      repeats = 2,
                      verboseIter = TRUE,
                      summaryFunction = twoClassSummary,
                      classProbs = TRUE)

# Treniramo trening set koriscenjem rpart metode
start.time <- Sys.time()
cl <- makeCluster(7, type = "SOCK")
registerDoSNOW(cl)
set.seed(5627)
orig_fit12 <- train(x = train12[,-1],
                   y = train12$Label,
                   method = "rpart",
                   metric = "ROC",
                   trControl = ctrl12,
                   tuneGrid = expand.grid(.cp=seq(0.001,0.5,0.001)))
stopCluster(cl)
total.time <- Sys.time() - start.time
total.time

# Use the same seed to ensure same cross-validation splits

ctrl12$seeds <- orig_fit12$control$seeds

# Build smote model

ctrl12$sampling <- "smote"
start.time <- Sys.time()
cl <- makeCluster(7, type = "SOCK")
registerDoSNOW(cl)
smote_fit12 <- train(x = train12[,-1],
                    y = train12$Label,
                    method = "rpart",
                    metric = "ROC",
                    trControl = ctrl12,
                    tuneGrid = expand.grid(.cp=seq(0.001,0.5,0.001)))
stopCluster(cl)
total.time <- Sys.time() - start.time
total.time
# Examine results for test set

model_list12 <- list(original = orig_fit12,
                    SMOTE = smote_fit12)

model_list_roc12 <- model_list12 %>%
  map(test_roc, data = test12)

model_list_roc12 %>%
  map(auc)

