#read csv file (cardio_data.csv)
cardio.data <- read.csv("https://raw.githubusercontent.com/mwang164/Heart-Disease-Prediction-/main/cardio_data.csv", header = TRUE)

################Logistic Regression Section##################
#############################################################
lr=cardio.data
View(lr)

lr[,2]<-factor(lr[,2])
lr[,3]<-factor(lr[,3])
lr[,6]<-factor(lr[,6])
lr[,7]<-factor(lr[,7])
lr[,9]<-factor(lr[,9])
lr[,11]<-factor(lr[,11])
lr[,12]<-factor(lr[,12])

lr.fit <- glm(HeartDisease~.,family=binomial,data=lr)
summary(lr.fit)

#take factors having p-value less than 5% and remodel logistic regression below
lr.fit2 <- glm(HeartDisease~Age+Sex+ChestPainType+ExerciseAngina+Oldpeak,family=binomial,data=lr)
summary(lr.fit2)

#set train data
cardio.lol<-lr
set.seed(10)
train=sample(1:nrow(cardio.lol), nrow(cardio.lol)*0.8) 
cardio.train=cardio.lol[train,]
cardio.test=cardio.lol[-train,]

cardio.test.result=cardio.test$HeartDisease
lr.fit3 <- glm(HeartDisease~Age+Sex+ChestPainType+ExerciseAngina+Oldpeak,family=binomial,data=cardio.train)
summary(lr.fit3)

lr.fit3.prob=predict(lr.fit3,cardio.test,type="response")
head(lr.fit3.prob)
length(lr.fit3.prob)
nrow(cardio.test)
lr.fit3.pred=rep(0,nrow(cardio.test))
lr.fit3.pred[lr.fit3.prob>.5]=1
table(lr.fit3.pred,cardio.test.result)
mean(lr.fit3.pred==cardio.test.result)

#Plot ROC & AUC
cardio.test.ROC <- cardio.test
cardio.test.ROC [,12] <- as.numeric(cardio.test.ROC [,12])
lr.fit3.pred.num <- as.numeric(lr.fit3.pred)

pred <- prediction(lr.fit3.pred.num,cardio.test.ROC [,12])
performance(pred,'auc')@y.values 
perf <- performance(pred,'tpr','fpr')
plot(perf)

##estimation of model
precision=49/(49+11)
precision

recall=49/(49+12)
recall

F1_score=2*precision*recall/(precision+recall)
F1_score

MSE=sqrt(mean((lr.fit3.pred.num-cardio.test.ROC [,12])^2))
MSE
############End of Logistic Regression Section###############
#############################################################




