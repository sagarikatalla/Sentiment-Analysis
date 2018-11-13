library(twitteR)
library(stringr)
library(tm)
library(ggplot2)
consumerKey <- ########################### #put the Consumer Key from Twitter Application
consumerSecret <- ##########################  #put the Consumer Secret from Twitter Application

setup_twitter_oauth(consumer_key = consumerKey,consumer_secret =
                      consumerSecret)

tweets = searchTwitter('trump',n=100)
Tweets.text = lapply(tweets,function(t)t$getText()) # gets text from Tweets
write.csv(Tweets.text,"tweets.csv")
trump_tweets = read.csv("tweets.csv")
t.tweets<-as.matrix(trump_tweets[trump_tweets$Tag
                                 %in% c("Positive","Negative")
                                 ,])


####### Splitting data into train and test data
library(caret)
indexes <- createDataPartition(t.tweets[,2], p=0.78, list = FALSE)
train.data <- t.tweets[indexes,]
test.data <- t.tweets[-indexes,]

##Naive Bayes classifier
library(e1071)
classifier = naiveBayes(train.data, as.factor(train.data[,2]) )

library(RTextTools)
# test the validity
predicted = predict(classifier, test.data[,1]); predicted
table(test.data[, 2], predicted)


classifier = naiveBayes(train.data, as.factor(train.data[,2]) )

result = confusionMatrix(test.data[, 2], predicted)

library(caret)
prec = result$byClass["Precision"]; prec
recall_acc = result$byClass["Recall"]; recall_acc


########### MAXENT, SVM, RF, TREE CLASSIFIERS ###########
tweets_1 = rbind(train.data,test.data)
matrix= create_matrix(tweets_1, language="english", 
                      removeStopwords=FALSE, removeNumbers=TRUE, 
                      stemWords=FALSE)
matrix =as.matrix(matrix)
# build the data to specify response variable, training set, testing set.
container = create_container(matrix, as.numeric(as.factor(matrix[,2])),
                             trainSize=1:56, testSize=57:70,virgin=FALSE)
# Second, to train the model with multiple machine learning algorithms:
  
  models = train_models(container, algorithms=c("MAXENT" , "SVM", "RF",  "TREE"))
# Now, we can classify the testing set using the trained models.

results = classify_models(container, models)
# How about the accuracy?

# accuracy table
res_rf = confusionMatrix( results[,"FORESTS_LABEL"],ifelse(factor(tweets_1[57:70, 2])=="Positive",1,0))
res_maxent = confusionMatrix( results[,"MAXENTROPY_LABEL"],ifelse(factor(tweets_1[57:70, 2])=="Positive",1,0))
res_tree = confusionMatrix( results[,"TREE_LABEL"],ifelse(factor(tweets_1[57:70, 2])=="Positive",1,0))
res_svm = confusionMatrix( results[,"SVM_LABEL"],ifelse(factor(tweets_1[57:70, 2])=="Positive",1,0))

# recall accuracy
res_rf$table[4]/(res_rf$table[4]+res_rf$table[2]) 
res_maxent$table[4]/(res_maxent$table[4]+res_maxent$table[2])
res_tree$table[4]/(res_tree$table[4]+res_tree$table[2])
res_svm$table[4]/(res_svm$table[4]+res_svm$table[2])

res_rf$byClass["Recall"]
res_maxent$byClass["Recall"] 
res_tree$byClass["Recall"]
res_svm$byClass["Recall"] 
