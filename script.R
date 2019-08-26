#install.packages("tm")
#install.packages("tidyr") #koriscena fja unite
#install.packages("qdap") #koriscena fja za najfrekventnije termine
#install.packages("RWeka")
library(tm)
library(tidyr)
library(ggplot2)
library(corrplot)
library(qdap)
library(RWeka)

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
#corrplot(corr.matrix, method = "number", type = "upper", diag = F, number.cex = 3.5, tl.cex = 1, tl.srt=0,  tl.col="black")
# Nikola: nema potrebe ovo crtati, racunate korelaciju izmedju dve varijable samo 

#sa grafika vidimo da je koef. korelacije 0.79, sto se smatra jakom korelacijom
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
#Proba sa VCorpus
#corpus_review <- VCorpus(VectorSource(ecommerce_reviews))
inspect(corpus_review)

#detalji o 123. dokumentu
inspect(corpus_review[123])
#Primer sadrzaja 123. dokumenta korpusa
inspect(corpus_review[[123]])

# 1. Sve reci u svim dokumentima pretvoriti u reci sa malim slovima.
corpus_review <- tm_map(corpus_review, content_transformer(tolower))

# 2. Uklanjanje stopwords-a iz tm-ove liste stopwords-a
corpus_review <- tm_map(corpus_review, removeWords, stopwords("english"))

#Da li ukloniti jos neke reci? Proverimo opet najfrekventnije reci u korpusu
#freq.terms <- freq_terms(corpus_review, 20)
#plot(freq.terms)
#Mina: koriscenjem VCorpus-a, reci koje su verovatno meta podaci nekako ulaze u razmatranje ovde.
#Da li onda samo ukloniti ove reci vidljive sa grafika(character, year,...,listsec)?
#corpus_review <- tm_map(corpus_review, removeWords, freq.terms$WORD)

#Opet najfrekventnije reci
#freq.terms <- freq_terms(corpus_review, 20)
#plot(freq.terms)
#Mozemo videti da su se reci izmenile - sada su to reci poput: dress, love, size, great, top, fit itd.
#Uklonicemo top3 reci: dress, love i size, jer njihova frekventnost ne utice na prediktivnost sentimenta
#corpus_review <- tm_map(corpus_review, removeWords, c("dress","love","size"))


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

#Kreiranje DTM matrice (po defaultu su ovo unigrami)
#dtm <- DocumentTermMatrix(corpus_review)
#inspect(dtm)

#dtm.trimmed <- removeSparseTerms(dtm, sparse = 0.9875)
#inspect(dtm.trimmed)

#Racunamo frekvencije termina iz DTM
#freq <- colSums(as.matrix(dtm))

#duzina = koliko termina (unigrama) imamo u DTM
#length(freq)
#Mina: da li ipak treba ovo, posto smo umanjili sparsity matrice dtm i napravili novu matricu dtm.trimmed?
#length(colSums(as.matrix(dtm.trimmed)))

#Sortiranje frekvencija termina opadajuce 
#ord <- order(freq, decreasing=TRUE)

#Najcesci termini
#freq[head(ord)]
#Najredji termini
#freq[tail(ord)]

#Kreiramo TF-IDF matricu
#dtm.tfidf <- DocumentTermMatrix(corpus_review, control = list(weighting = weightTfIdf))
#dtm.tfidf <- removeSparseTerms(dtm.tfidf, 0.9999)
#rowTotals <- slam::row_sums(dtm.tfidf)
#dtm.tfidf <- dtm.tfidf[rowTotals > 0, ]
#inspect(dtm.tfidf)

#Kreiranje bigrama
#BigramTokenizer <- function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
#dtm.bigram <- TermDocumentMatrix(corpus_review, control = list(tokenize = BigramTokenizer))
#inspect(dtm.bigram)

#Mina: Prijavljuje mi ovo kao gresku prilikom kreiranja bigrama:
# Error in names(out) <- grouping : 
#'names' attribute [1] must be the same length as the vector [0] 

#Kreiranje trigrama
#TrigramTokenizer <- function(x) { unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)}
#dtm.trigram <- DocumentTermMatrix(corpus_review, control = list(tokenize = TrigramTokenizer))
#inspect(dtm.trigram)
#Mina: Prijavljuje mi ovo kao gresku prilikom kreiranja trigrama:
# Error in names(out) <- grouping : 
#'names' attribute [1] must be the same length as the vector [0]


#####################################################
#KREIRANJE DTM, TF-IDF za unigrame, bigrame, trigrame
#####################################################

library(quanteda)
if (requireNamespace("tm", quietly = TRUE)) {
  qCorpus <- corpus(corpus_review)
  summary(qCorpus, showmeta=TRUE)
}


#Kreiranje tokena za svaki od dokumenata u korpusu, (token = unigram)
review.tokens <- tokens(qCorpus$documents$texts, what = "word")
#Prikaz tokena 123. dokuemnta
review.tokens[[123]]          

#Kreiranje DcoumentTermMatrix (DocumentFeatureMatrix)
review.tokens.dfm <- dfm(review.tokens, tolower = FALSE)
#Prebacivanje DFM u matricu
#review.tokens.matrix <- as.matrix(review.tokens.dfm)
View(review.tokens.dfm[1:20, 1:100])
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

#Vektor sa ukupnim tf-idf skorovima za svaki unigram 
total.tfidf.scores.unigram <- as.vector(colSums(review.tokens.dfm.tfidf))

#75. i 90. percentil
percentil75 <- quantile(total.tfidf.scores.unigram, 0.75)
percentil90 <- quantile(total.tfidf.scores.unigram, 0.9)

review.tokens.dfm.tfidf.df <- convert(review.tokens.dfm.tfidf, to="data.frame")
review.tokens.dfm.tfidf.df <- cbind(Label = ecommerce_reviews$Label,review.tokens.dfm.tfidf.df)
#Prikaz prvih 25 dokumenata TF-IDF matrice sa labelama
review.tokens.dfm.tfidf.df[1:25,1:4]

#UNIGRAMI sa total TF-IDF skorom > 75 percentila
unigram.index.removals.75 <- which(total.tfidf.scores.unigram < percentil75)+2 #dodajem +2 zbog prve dve nove kolone
unigrams.tfidf.75 <- review.tokens.dfm.tfidf.df[,-unigram.index.removals.75]

#UNIGRAMI sa total TF-IDF skorom > 90 percentila
unigram.index.removals.90 <- which(total.tfidf.scores.unigram < percentil90)+2 #dodajem +2 zbog prve dve nove kolone
unigrams.tfidf.90 <- review.tokens.dfm.tfidf.df[,-unigram.index.removals.90]



##############################
#Kreiramo bigrame
##############################



bigrams <- tokens_ngrams(review.tokens, n = 2:2)
#Prikaz bigrama 123. dokumenta
bigrams[[123]]
#Kreiramo DFM od bigrama
review.bigrams.dfm <- dfm(bigrams, tolower = FALSE)

View(review.bigrams.dfm[1:20, 1:50])
dim(review.bigrams.dfm)
review.bigrams.dfm

#TF-IDF od bigrama sa labelama
review.bigrams.dfm.tfidf <- dfm_tfidf(review.bigrams.dfm, scheme_tf = "prop",
                                     scheme_df = "inverse", base = 10)
dim(review.bigrams.dfm.tfidf)

#Vektor sa ukupnim tf-idf skorovima za svaki bigram 
total.tfidf.scores.bigram <- as.vector(colSums(review.bigrams.dfm.tfidf))

#75. i 90. percentil
percentil75 <- quantile(total.tfidf.scores.bigram, 0.75)
percentil90 <- quantile(total.tfidf.scores.bigram, 0.9)

#BIGRAMI sa total TF-IDF skorom > 75 percentila
bigram.index.removals.75 <- which(total.tfidf.scores.bigram < percentil75)
bigrams.tfidf.75 <- dfm_select(review.bigrams.dfm.tfidf, pattern = colnames(review.bigrams.dfm.tfidf)[bigram.index.removals.75],
                               selection = "remove")
bigrams.tfidf.75 <- convert(bigrams.tfidf.75, to = "data.frame") #Ne moze da se pretvori u dataframe,
#out of memory greska
#Da li ovde primeniti smanjenje tfidf na osnovu praga za sparsity?
#glavni problem: ne mogu da oznacim dokumenta onda

#BIGRAMI sa total TF-IDF skorom > 90 percentila
bigram.index.removals.90 <- which(total.tfidf.scores.bigram < percentil90)
bigrams.tfidf.90 <- dfm_select(review.bigrams.dfm.tfidf, pattern = colnames(review.bigrams.dfm.tfidf)[bigram.index.removals.90],
                               selection = "remove")
bigrams.tfidf.90 <- convert(bigrams.tfidf.90, to = "data.frame") #Error: cannot allocate vector of size 3.2 Gb
#Ne moze da se pretvori u dataframe,
#Da li i ovde primeniti smanjenje tfidf na osnovu praga za sparsity?
#glavni problem: ne mogu da oznacim dokumenta onda



##############################
#Kreiramo trigrame
##############################



trigrams <- tokens_ngrams(review.tokens, n = 3:3)
#Prikaz trigrama 123. dokumenta
trigrams[[123]]
#Kreiramo DFM od trigrama
review.trigrams.dfm <- dfm(trigrams, tolower = FALSE)

View(review.trigrams.dfm[1:20, 1:50])
dim(review.trigrams.dfm)
review.trigrams.dfm

#TF-IDF od trigrama sa labelama
review.trigrams.dfm.tfidf <- dfm_tfidf(review.trigrams.dfm, scheme_tf = "prop",
                                      scheme_df = "inverse", base = 10)
dim(review.trigrams.dfm.tfidf)

#Vektor sa ukupnim tf-idf skorovima za svaki trigram 
total.tfidf.scores.trigram <- as.vector(colSums(review.trigrams.dfm.tfidf))

#75. i 90. percentil
percentil75 <- quantile(total.tfidf.scores.trigram, 0.75)
percentil90 <- quantile(total.tfidf.scores.trigram, 0.9)

#TRIGRAMI sa total TF-IDF skorom > 75 percentila
trigram.index.removals.75 <- which(total.tfidf.scores.trigram < percentil75)
trigrams.tfidf.75 <- dfm_select(review.trigrams.dfm.tfidf, pattern = colnames(review.trigrams.dfm.tfidf)[trigram.index.removals.75],
                               selection = "remove")

trigrams.tfidf.75 <- convert(trigrams.tfidf.75, to = "data.frame") #Ne moze da se pretvori u dataframe,
#"problem too large" greska
#Da li ovde primeniti smanjenje tfidf na osnovu praga za sparsity?
#glavni problem: ne mogu da oznacim dokumenta onda

#TRIGRAMI sa total TF-IDF skorom > 90 percentila
trigram.index.removals.90 <- which(total.tfidf.scores.trigram < percentil90)
trigrams.tfidf.90 <- dfm_select(review.trigrams.dfm.tfidf, pattern = colnames(review.trigrams.dfm.tfidf)[trigram.index.removals.90],
                               selection = "remove")

bigrams.tfidf.90 <- convert(bigrams.tfidf.90, to = "data.frame") #Error: cannot allocate vector of size 3.2 Gb
#Ne moze da se pretvori u dataframe,
#Da li i ovde primeniti smanjenje tfidf na osnovu praga za sparsity?
#glavni problem: ne mogu da oznacim dokumenta onda




#install.packages(c("dplyr","DMwR","purr"))
library(dplyr) # for data manipulation
library(caret) # for model-building
library(DMwR) # for smote implementation
library(purrr) # for functional programming (map)
library(pROC) # for AUC calculations
###################################################################
# KONFIGURACIJA 1
###################################################################

set.seed(1010)
train.indexes <- createDataPartition(unigrams.tfidf.75$Label, p = .80, list = FALSE)
train1a <- unigrams.tfidf.75[train.indexes,]
test1a <- unigrams.tfidf.75[-train.indexes,]
prop.table(table(test1a$Label))

# Podkonfiguracija A 
# Kontrolna funkcija za 10fold cross-validation
install.packages("LiblineaR")
library(LiblineaR)
ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 5,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE)

# Build a standard classifier using a gradient boosted machine

set.seed(5627)

orig_fit <- train(Label ~ .,
                  data = train1a,
                  method = "svmRadialWeights",
                  verbose = FALSE,
                  metric = "ROC",
                  trControl = ctrl)
################################################
#ovde sam stala, jer je trening run-ovalo preko 6h, dakle mora da se redukuje dataset
################################################
# Build custom AUC function to extract AUC
# from the caret model object

#test_roc <- function(model, data) {
  
#  roc(data$Label,
#      predict(model, data, type = "prob")[, "NEG"])
  
#}

#orig_fit %>%
#  test_roc(data = test1a) %>%
#  auc()

