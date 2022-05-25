# Source of data and code: Dimitris Bertsimas

MusicRecord<-read.csv(file.choose()) #load data

#install.packages("dplyr")
library(dplyr)

# we want to exclude some of the variables in our dataset from being used as independent variables 
# ("year", "songtitle", "artistname", "songID", and "artistID").
# First define a vector of variable names called nonvars - these are the variables that we won't use in our model.
library(MASS)
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")

# first use the filter function to split the data into a training set "SongsTrain" 
# consisting of all the observations up to and including 2009 song releases, and a testing set "SongsTest", 
# consisting of the 2010 song releases.
SongsTrain = MusicRecord %>% filter(year <= 2009)
SongsTest = MusicRecord %>% filter(year == 2010)

# To remove these variables from your training and testing sets:
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]


library(factoextra)
library(FactoMineR)
MusicRecord.pca <- PCA(SongsTrain[,1:33], graph = FALSE, ncp = 33) 
summary(MusicRecord.pca)
score <- as_tibble(get_pca_ind(MusicRecord.pca)$coord)
mod <- cbind(SongsTrain[34], score[,c(1:3, 5:6, 8:11, 13:14, 17, 21, 23, 26, 31, 33)])

SongsTest.trans <- PCA(SongsTest[,1:33], graph =FALSE, ncp = 33)
score_songstest <- as_tibble(get_pca_ind(SongsTest.trans)$coord)
mod_songstest <- cbind(SongsTest[34], score_songstest[,,c(1:3, 5:6, 8:11, 13:14, 17, 21, 23, 26, 31, 33)])
SongsLog1 = glm(Top10 ~ ., data=mod, family=binomial)
summary(SongsLog1)

# True or False?
# 1. The higher our confidence about time signature, key and tempo, the more likely the song is to be in the Top 10
# 2. In general, if the confidence is low for the time signature, tempo, and key, then the song is more likely to be complex. What does our model suggest in terms of complexity?

# You can make predictions on the test set by using the command:
testPredict = predict(SongsLog1, newdata=mod_songstest, type="response")
# Then, you can create a confusion matrix with a threshold of 0.15 by using the table command:
confusion.matrix<-table(SongsTest$Top10, testPredict >= 0.15)
Count.correct<-confusion.matrix[1,1]+confusion.matrix[2,2]
Count.wrong<-confusion.matrix[1,2]+confusion.matrix[2,1]
Accuracy.rate<-Count.correct/(Count.correct+Count.wrong)
Accuracy.rate
# What is the prediction accuracy of the model?

# To generate the ROC curve
#install.packages("pROC")
library(pROC)
test_prob = predict(SongsLog1, newdata = mod_songstest, type = "response")
test_roc = roc(SongsTest$Top10 ~ test_prob, plot = TRUE, print.auc = TRUE)

