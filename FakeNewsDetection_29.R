library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidytext)
library(caTools)
library(syuzhet)
library(here)
library(skimr)
library(janitor)
library(stringr)
library(SnowballC)
library(tm)
library(ggthemes)
library( wordcloud)
library(ggwordcloud)
library(ISLR)
library(class)
library(e1071)
library(caret)
library(gmodels)
library(randomForest)

library(DAAG)
library(party)
library(rpart)
library(rpart.plot)
library(mlbench)
library(caret)
library(pROC)
library(tree)

############ READ DATA ############
news_data <- read_csv("news.csv")
View(news_data)

head(news_data)
dim(news_data)

news_data %>%
  group_by(label) %>%
  summarise(count = n())


############ RENAME FIRST COLUMN ############
news_data %>%
  rename(id = ...1) #
colnames(news_data)[1] = "id" ##


############ REMOVE NULL ############
news_data$label[is.na(news_data$label)] <- "FAKE"
news_data <- na.omit(news_data)
news_data <- news_data[ !(is.na(news_data$text) | news_data$text == ""),]
news_data <- unique(news_data)
as.factor(news_data$label)

###############################################################
############ VISUALIZATION ############
   ######### exclamations #########
news_data$exc <- sapply(news_data$text, function(x) length(unlist(strsplit(as.character(x), "\\!+"))))
dfE <- news_data %>%
  group_by(label) %>%
  summarise(exclamations=sum(exc))

boxplot(exc ~ label,news_data,ylim=c(0,30),col=c("red","orange"))
pie(dfE$exclamations , label = c("FAKE" , "REAL") , main = "Exclamations")


    ######### questions #########
news_data$que <- sapply(news_data$text, function(x) length(unlist(strsplit(as.character(x), "\\?+"))))
dfQ <- news_data %>%
    group_by(label) %>%
    summarise(questions=sum(que))
boxplot(que ~ label,news_data,ylim=c(0,30),col=c("red","orange")) 
pie(dfQ$questions , label = c("FAKE" , "REAL") , main = "Questions")


   ######## Number of Words ########
#text
news_data$nwords <- sapply(gregexpr("[[:alpha:]]+", news_data$text), function(x) sum(x > 0))
dfn <-news_data%>%
  group_by(label) %>%
  summarise(numWords = sum(nwords))
pie(dfn$numWords , label = c("FAKE" , "REAL") , main = "numWords")

##in title column
news_data$nwordsT <- sapply(gregexpr("[[:alpha:]]+", news_data$title), function(x) sum(x > 0))
dfnt <-news_data%>%
  group_by(label) %>%
  summarise(numWordsT = sum(nwordsT))
pie(dfnt$numWordsT , label = c("FAKE" , "REAL") , main = "num of Words in title")


df_new <- data.frame(Label=c("FAKE" , "REAL") , Exclamations=dfE$exclamations , Questions=dfQ$questions , nWords = dfn$numWords , wordsTitle = dfnt$numWordsT)


############# MOST USED WORDS ###############
news2 <- news_data
news = data.frame(doc_id=seq(1:nrow(news2)),text=news2$text)

#create a function to change case to lower
tryTolower <- function( x){ 
  y = NA 
  try_error = tryCatch( tolower( x), error = function( e) e) 
  if (! inherits( try_error, 'error')) 
    y = tolower( x) 
  return( y) 
}

#add to stopwords
custom.stopwords = c(stopwords("english") , "lol" , "smh" , "delta" , "amp")

clean.corpus <- function( corpus){ 
  corpus <- tm_map( corpus, content_transformer( tryTolower)) 
  corpus = tm_map( corpus, removeWords, custom.stopwords) 
  corpus = tm_map( corpus, removePunctuation) 
  corpus = tm_map( corpus, stripWhitespace) 
  corpus = tm_map( corpus, removeNumbers) 
  return( corpus) 
}

#create the frequency dataframe
corpus <- VCorpus( DataframeSource( news))
corpus <- clean.corpus( corpus)
tdm <- TermDocumentMatrix( corpus, control = list( weighting = weightTf))
tdm.news.m <- as.matrix( tdm)
term.freq <- rowSums( tdm.news.m) 
freq.df <- data.frame( word = names( term.freq), frequency = term.freq) 
freq.df <- freq.df[ order( freq.df[, 2], decreasing = T),]

# plot the terms by frequency
freq.df$ word <- factor( freq.df $ word, 
                         levels = unique( as.character( freq.df $ word)))

ggplot( freq.df[ 1: 20,], aes( x = word, y = frequency)) + 
  geom_bar( stat ="identity", fill ='darkred') + 
  coord_flip() + theme_gdocs() + 
  geom_text( aes( label = frequency), colour ="white", hjust = 1.25, size = 5.0)



######+++++++++++++++++++++++++++++++++++++++######

# custom.stopwords <- c( stopwords('english'), 'sorry', 'amp', 'delta', 'amazon')
# clean.vec <- function( text.vec){ text.vec <- tryTolower( text.vec)
# text.vec <- removeWords( text.vec, custom.stopwords)
# text.vec <- removePunctuation( text.vec)
# text.vec <- stripWhitespace( text.vec)
# text.vec <- removeNumbers( text.vec)
# return( text.vec)
# }
# 
# fake_news <- subset(news_data, label == 0)
# real_news <- subset(news_data, label == 1)
# fake_news.vec <- clean.vec(fake_news$text)
# real_news.vec <- clean.vec(real_news$text)
# fake_news.vec <- paste(fake_news.vec , collapse = " ")
# real_news.vec <- paste(real_news.vec , collapse = " ")
# all <- c(fake_news.vec , real_news.vec)
# corpus1 <- VCorpus(VectorSource(all))
# tdm = TermDocumentMatrix(corpus1)
# tdm.m = as.matrix(tdm)
# colnames(tdm.m) <- c("Fake", "Real")
# tdm.m[3480:3490,]
# 
# 
# library( plotrix)
# common.words <- subset( tdm.m, tdm.m[, 1] > 0 & tdm.m[, 2] > 0)
# tail( common.words)
# #calculate the differences between the two columns of common words
# 
# difference <- abs( common.words[, 1] - common.words[, 2])
# 
# #combine the differences with the common words
# common.words <- cbind( common.words, difference)
# #sort by the difference column in decreasing order
# common.words <- common.words[ order( common.words[, 3], decreasing = TRUE), ]
# 
# #select the top 25 words and create a data frame
# top25.df <- data.frame( x = common.words[ 1: 25, 1],
#                         y = common.words[ 1: 25, 2],
#                         labels = rownames(common.words[ 1: 25, ]))
# pyramid.plot(top25.df$x, top25.df$y,
#              labels = top25.df$labels,
#              #change gap to show longer words
#              gap = 30,
#              top.labels = c("Fake", "Words", "Real"),
#              main = "Words in Common",
#              laxlab = NULL, raxlab = NULL, unit = NULL)
# 
# #pick purples can be any color
# pal <- brewer.pal( 8, "Purples")
# #use the darker colors
# pal <- pal[-( 1: 4)]
# #generate the commonality cloud
# commonality.cloud( tdm.m, max.words = 200, random.order = FALSE, colors = pal)
# 
# comparison.cloud( tdm.m, max.words = 200, random.order = FALSE, title.size = 1.0,
#                   colors = brewer.pal( ncol(tdm.m),"Dark2"))



######+++++++++++++++++++++++#############

#################################################################


############ CONVERT DATA ############
news_data$label = factor(news_data$label,levels = c('FAKE', 'REAL'),labels = c(0, 1))
summary(news_data)



############ SPLIT DATA ############
sample <- sample.split(news_data$text, SplitRatio = 0.7)
data_train<- subset(news_data, sample == TRUE)
data_test <- subset(news_data, sample == FALSE)
data_train_label <- data_train$label



##################### Naive Bayes ##############################
#convert text to corpus
news1 <- read.csv("news.csv")
news1$text <- factor(news1$text)
news1 <- na.omit(news1)
new_corpus <- VCorpus(VectorSource(news1$text))
print(new_corpus)
#examine text in corpus
as.character(new_corpus[[3]])
#clean corpus
news_clean<-tm_map(new_corpus,content_transformer(tolower))
news_clean<-tm_map(news_clean,removeNumbers)
news_clean<-tm_map(news_clean,removeWords,stopwords())
news_clean<-tm_map(news_clean,removePunctuation)
news_clean<-tm_map(news_clean,stripWhitespace)

as.character(news_clean[[3]])

#tokenize the words using the clean corpus
dataF <- DocumentTermMatrix(news_clean)
#slpit DS into traning &test
#data_train
dataTR<-dataF[1:4751,] #75%
dataTS<-dataF[4752:6335,] 

#distrubute all 
dataTrain_Label<-news1[1:4751,]$label #75%
dataTest_Label<-news1[4752:6335,]$label #25%

#check if fake ana real been distruvuted simialr
prop.table(table(dataTrain_Label))
prop.table(table(dataTest_Label))

#find item the appear 5 times 
news_freq_words <- findFreqTerms(dataTR , 5)
str(news_freq_words)

#include only the freq
data_pre_frq_TR<-dataTR[,news_freq_words]
data_pre_frq_TS<-dataTS[,news_freq_words]

#check if words yes or no
conv_count<-function(x){
  x<-ifelse(x>0,"Yes","No")
}

data_TR<-apply(data_pre_frq_TR,MARGIN=2,conv_count)
data_TS<-apply(data_pre_frq_TS,MARGIN=2,conv_count)

#THE MODEL
data_class<-naiveBayes(data_TR,dataTrain_Label)
#test Model
data_pred<-predict(data_class,data_TS)

cm <- table(dataTest_Label , data_pred)
confusionMatrix(cm)





#################### Decision Tree ##########################
set.seed(1234)
numOfWordsData <- news_data[,c("label" , "nwords" , "que" , "exc" )]
ind <- sample(2, nrow(numOfWordsData), replace = T, prob = c(0.5, 0.5))
train <- numOfWordsData[ind == 1,]
test <- numOfWordsData[ind == 2,]
#Tree Classification
tree <- rpart(label ~.,  train)
rpart.plot(tree)
printcp(tree)
rpart(formula = label ~ ., data = train)
tree <- rpart(label ~., data = train,cp=0.07444)
p <- predict(tree, test, type = 'class')
confusionMatrix(p, test$label)




#################### KNN ###################################
library(e1071)
library(caTools)
library(class)
# Fitting KNN Model
numOfWordsData <- news_data[,c("label" , "nwords" ,"que" , "exc" )]
set.seed(222)
ind <- sample(2, nrow(numOfWordsData), replace = TRUE, prob = c(0.7, 0.3))
train <- numOfWordsData[ind==1,]
test <- numOfWordsData[ind==2,]
# to training dataset
classifier_knn <- knn(train = train,test = test,cl = train$label,k = 1)
# Confusiin Matrix
cmk <- table(test$label, classifier_knn)
confusionMatrix(cmk)
# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(classifier_knn != test$label)
print(paste('Accuracy =', 1-misClassError))




############### logistic regression #########################
#not working :(
# set.seed(100)
# #fit logistic regression model
# model <- glm(label~., family="binomial", data=train_cl)
# pred <- predict(model , train_cl)
# cm1 <- table(test_cl , pred)
# confusionMatrix(pred , as.factor(train_cl$label))
# #disable scientific notation for model summary
# options(scipen=999)
# 
# #view model summary
# summary(model)





############### SVM ########################################
set.seed(222)
classifier = svm(formula = label ~ .,
                 data = train,
                 type = 'C-classification',
                 kernel = 'linear')
pr <- predict(classifier , test)
confusionMatrix(pr, test$label)




################## Random Forest ###################################
# Splitting data in train and test data
set.seed(222)
rf <- randomForest(label~., data=train, proximity=TRUE)
p1 <- predict(rf, train)
confusionMatrix(p1, train$label)
