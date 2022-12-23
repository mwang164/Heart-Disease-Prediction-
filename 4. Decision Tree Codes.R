#read csv file (cardio_data.csv)
cardio.data <- read.csv("https://raw.githubusercontent.com/mwang164/Heart-Disease-Prediction-/main/cardio_data.csv", header = TRUE)
summary(cardio.data)

install.packages("ROCR")
install.packages("randomForest")
install.packages("tidyverse")
library(randomForest)
library(ROCR)
library(tree)
library(ggplot2)

###################Decision Tree Section#####################
#############################################################
#use decision trees to predict cardio
heartdisease <- as.factor(cardio.data$HeartDisease)
heart.data.dt <- data.frame(cardio.data[, -12], heartdisease)
class(heart.data.dt$heartdisease)

#
tree.heart.data= tree(heartdisease ~ ., data = heart.data.dt)
summary(tree.heart.data)
plot(tree.heart.data)
text(tree.heart.data, pretty = 0)

#use test and train data
set.seed(100)
train.tree <- sample(1:nrow(heart.data.dt), round(nrow(heart.data.dt)*0.8,0))
heart.test <- heart.data.dt[-train.tree, ]
cardio.test <- heart.test$heartdisease

tree.heart.test <- tree(heartdisease ~ ., data = heart.data.dt, subset = train.tree)
summary(tree.heart.test)

tree.pred <- predict(tree.heart.test, newdata = heart.test, type = "class")
table(tree.pred, cardio.test)
mean(tree.pred == cardio.test)

#plot confusion matrix
TrueClass <- factor(c(0, 0, 1, 1))
PredClass <- factor(c(0, 1, 0, 1))
Y      <- c(69, 9, 9, 51)
df <- data.frame(TrueClass, PredClass, Y)

ggplot(data =  df, mapping = aes(x = TrueClass, y = PredClass)) +
  geom_tile(aes(fill = Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_bw() + theme(legend.position = "none")


#use cross-validation to find the size with smallest deviance, increasing the prediction accuracy
set.seed(1)
cv.heart.data.dt <- cv.tree(tree.heart.test, FUN=prune.misclass)
names(cv.heart.data.dt)
cv.heart.data.dt
plot(cv.heart.data.dt$size, cv.heart.data.dt$dev, xlab="Number of Nodes", type = "b")

# prune the tree with 8 nodes with classifications
prune.heart.test <- prune.misclass(tree.heart.test, best=8)
plot(prune.heart.test)
text(prune.heart.test, pretty = 0)

#use test and train data with pruned tree
tree.pred.prune <- predict(prune.heart.test, newdata = heart.test, type = "class")
table(tree.pred.prune, cardio.test)

#accuracy
mean(tree.pred.prune == cardio.test)

#plot confusion matrix
TrueClass <- factor(c(0, 0, 1, 1))
PredClass <- factor(c(0, 1, 0, 1))
Y      <- c(71, 7, 9, 51)
df <- data.frame(TrueClass, PredClass, Y)

ggplot(data =  df, mapping = aes(x = TrueClass, y = PredClass)) +
  geom_tile(aes(fill = Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_bw() + theme(legend.position = "none")

#plot ROC & AUC
heart.test.ROC <- heart.test
heart.test.ROC [,12] <- as.numeric(heart.test.ROC [,12])
tree.pred.prune.num <- as.numeric(tree.pred.prune)
class(heart.test.ROC$heartdisease)

pred <- prediction(tree.pred.prune.num,heart.test.ROC$heartdisease)
performance(pred,'auc')@y.values 
perf <- performance(pred,'tpr','fpr')
plot(perf)


##estimation of model
precision=51/(51+7)
precision

recall=51/(51+9)
recall

F1_score=2*precision*recall/(precision+recall)
F1_score

MSE=sqrt(mean((tree.pred.prune.num-heart.test.ROC [,12])^2))
MSE

###############End of Decision Tree Section#################
############################################################



##################Random Forest Section#####################
############################################################
heartdisease <- as.factor(cardio.data$HeartDisease)
heart.data.rf <- data.frame(cardio.data[, -12], heartdisease)
class(heart.data.rf$heartdisease)

#use test and train data
set.seed(100)
train.rf <- sample(1:nrow(heart.data.rf), round(nrow(heart.data.rf)*0.8,0))
heart.test.rf <- heart.data.rf[-train.rf, ]
cardio.test.rf <- heart.test.rf$heartdisease

#mtry=11 indicates all 11 predictors should be considered for each tree
set.seed(5)
rf.cardio= randomForest(heartdisease~.,data=heart.data.rf,subset=train.rf,mtry=11,importance=T)
rf.cardio

rf.pred = predict(rf.cardio, newdata=heart.test.rf)

table(rf.pred, cardio.test.rf)
mean(rf.pred == cardio.test.rf)

#plot confusion matrix
TrueClass <- factor(c(0, 0, 1, 1))
PredClass <- factor(c(0, 1, 0, 1))
Y      <- c(70, 8, 7, 53)
df <- data.frame(TrueClass, PredClass, Y)

ggplot(data =  df, mapping = aes(x = TrueClass, y = PredClass)) +
  geom_tile(aes(fill = Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_bw() + theme(legend.position = "none")

#Select mtry(number of predictors at each split) value with minimum out of bag(OOB) error
set.seed(9)
heart.train.rf <- heart.data.rf[train.rf, ]
mtry <- tuneRF(heart.train.rf[-12],heart.train.rf$heartdisease, 
               mtryStart=2,step=0.9,ntreeTry = 500,trace = TRUE,improve=1e-5)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
mtry
best.m

#use test and train randomForest with mtry=2
set.seed(9)
rf.cardio.2= randomForest(heartdisease~.,data=heart.data.rf,subset=train.rf,mtry=2,importance=T)
rf.cardio.2

rf.pred.2 = predict(rf.cardio.2, newdata=heart.test.rf)

table(rf.pred.2, cardio.test.rf)
mean(rf.pred.2 == cardio.test.rf)

#plot confusion matrix
TrueClass <- factor(c(0, 0, 1, 1))
PredClass <- factor(c(0, 1, 0, 1))
Y      <- c(71, 7, 5, 55)
df <- data.frame(TrueClass, PredClass, Y)

ggplot(data =  df, mapping = aes(x = TrueClass, y = PredClass)) +
  geom_tile(aes(fill = Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_bw() + theme(legend.position = "none")

#ROC plot & AUC 
rf.test.ROC <- heart.test.rf
rf.test.ROC [,12] <- as.numeric(rf.test.ROC [,12])
rf.pred.num <- as.numeric(rf.pred.2)
class(rf.test.ROC$heartdisease)

pred <- prediction(rf.pred.num,rf.test.ROC$heartdisease)
performance(pred,'auc')@y.values 
perf <- performance(pred,'tpr','fpr')
plot(perf)

##estimation of model
precision=55/(55+7)
precision

recall=55/(55+5)
recall

F1_score=2*precision*recall/(precision+recall)
F1_score

MSE=sqrt(mean((rf.pred.num-rf.test.ROC [,12])^2))
MSE

##############End of Random Forest Section##################
############################################################

