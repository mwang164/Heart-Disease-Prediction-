#read csv file (cardio_data.csv)
cardio.data <- read.csv("https://raw.githubusercontent.com/mwang164/Heart-Disease-Prediction-/main/cardio_data.csv", header = TRUE)

########################KNN Section##########################
#############################################################
cardio.data.knn <- cardio.data
View(cardio.data.knn)
cardio.data.knn[,12]<-factor(cardio.data.knn[,12])

# First, we set out lovely seeds first
set.seed(7)
# And, split data into two subsets
train=sample(1:nrow(cardio.data.knn), nrow(cardio.data.knn)*0.8,0)
# split data into two subsets
test=(-train)
# Second, we create the first part that will be our training data and second part be our testing data.
# Then, we will call these cardio.data.train and cardio.data.test
cardio.data.knn.train=cardio.data.knn[train,c(1,2,3,4,5,6,7,8,9,10,11)]
#for all the columns
cardio.data.knn.test=cardio.data.knn[test,c(1,2,3,4,5,6,7,8,9,10,11)]

number.train=cardio.data.knn[train,c(12)]
number.test=cardio.data.knn[test,c(12)]
library(class)

#Finally, we run same thing for different k value.

set.seed(8)
predict.knn = knn(cardio.data.knn.train,cardio.data.knn.test,number.train,k=1)
table(predict.knn,number.test)
mean(predict.knn==number.test)

set.seed(8)
predict.knn = knn(cardio.data.knn.train,cardio.data.knn.test,number.train,k=2)
table(predict.knn,number.test)
mean(predict.knn==number.test)

set.seed(8)
predict.knn = knn(cardio.data.knn.train,cardio.data.knn.test,number.train,k=3)
table(predict.knn,number.test)
mean(predict.knn==number.test)

set.seed(8)
predict.knn = knn(cardio.data.knn.train,cardio.data.knn.test,number.train,k=4)
table(predict.knn,number.test)
mean(predict.knn==number.test)

set.seed(8)
predict.knn = knn(cardio.data.knn.train,cardio.data.knn.test,number.train,k=5)
table(predict.knn,number.test)
mean(predict.knn==number.test)

set.seed(8)
predict.knn = knn(cardio.data.knn.train,cardio.data.knn.test,number.train,k=6)
table(predict.knn,number.test)
mean(predict.knn==number.test)

set.seed(8)
predict.knn = knn(cardio.data.knn.train,cardio.data.knn.test,number.train,k=7)
table(predict.knn,number.test)
mean(predict.knn==number.test)

set.seed(8)
predict.knn = knn(cardio.data.knn.train,cardio.data.knn.test,number.train,k=8)
table(predict.knn,number.test)
mean(predict.knn==number.test)

set.seed(8)
predict.knn = knn(cardio.data.knn.train,cardio.data.knn.test,number.train,k=9)
table(predict.knn,number.test)
mean(predict.knn==number.test)

set.seed(8)
predict.knn = knn(cardio.data.knn.train,cardio.data.knn.test,number.train,k=10)
table(predict.knn,number.test)
mean(predict.knn==number.test)

set.seed(8)
predict.knn = knn(cardio.data.knn.train,cardio.data.knn.test,number.train,k=11)
table(predict.knn,number.test)
mean(predict.knn==number.test)

#K=(9), we have the maximum mean value = 0.676259
set.seed(8)
predict.knn.9 = knn(cardio.data.knn.train,cardio.data.knn.test,number.train,k=9)
table(predict.knn.9,number.test)
mean(predict.knn.9==number.test)

#Plot ROC & AUC
cardio.test.ROC <- number.test
cardio.test.ROC <- as.numeric(cardio.test.ROC)
predict.knn.9 <- as.numeric(predict.knn.9)

pred <- prediction(predict.knn.9,cardio.test.ROC)
performance(pred,'auc')@y.values 
perf <- performance(pred,'tpr','fpr')
plot(perf)

##estimation of model
precision=44/(44+18)
precision

recall=44/(44+27)
recall

F1_score=2*precision*recall/(precision+recall)
F1_score

MSE=sqrt(mean((predict.knn.9-cardio.test.ROC)^2))
MSE

####################End of KNN Section#######################
#############################################################
