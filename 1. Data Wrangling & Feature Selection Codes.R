#######Data Wrangling & Feature Selection Section############
#############################################################
# read csv file from cleaned data (heart_cleaned.csv)
cardio.data <- read.csv("https://raw.githubusercontent.com/mwang164/Heart-Disease-Prediction-/main/heart_cleaned.csv", header = TRUE)
View(cardio.data)


#remove the missing value
dim(cardio.data)
sum(is.na(cardio.data))
summary(cardio.data)
# there is no missing value

# correlation & cause
#Categorical & Categorical
#chi-squre test

#gender
gender=table(cardio.data[,2], cardio.data[,12])
chisq.test(gender)
plot(gender)
# p-value=3e-15, smaller than 0.05, significantly related with cardio

#FastingBS
FastingBS=table(cardio.data[,6], cardio.data[,12])
chisq.test(FastingBS)
plot(FastingBS)
# p-value< 2e05, smaller than 0.05, siginifcant

#RestingECG
RestingECG=table(cardio.data[,7], cardio.data[,12])
chisq.test(RestingECG)
plot(RestingECG)
# p-value=0.001, smaller than 0.05, signifcant

#ExerciseAngina
ExerciseAngina=table(cardio.data[,9], cardio.data[,12])
chisq.test(ExerciseAngina)
plot(ExerciseAngina)
# p-value< 2e05, smaller than 0.05, significant

#ST_Slope
ST_Slope=table(cardio.data[,11], cardio.data[,12])
chisq.test(ST_Slope)
plot(ST_Slope)
# p-value< 2e-16, smaller than 0.05, significant

#ChestPaintype
ChestPaintype=table(cardio.data[,3], cardio.data[,12])
chisq.test(ChestPaintype)
plot(ChestPaintype)
# p-value< 2e-16, smaller than 0.05, significant


#Categorical & Continuous:
# one-way anova

#age
age = aov(cardio.data[,1]~cardio.data[,12], data=cardio.data)
summary(age)
# p-value <2e-16 ***,significant

#restingBP
restingBP= aov(cardio.data[,4]~cardio.data[,12], data=cardio.data)
summary(restingBP)
# p-value=1.9e-06 ***,significant

#Cholesterol
Cholesterol = aov(cardio.data[,5]~cardio.data[,12], data=cardio.data)
summary(Cholesterol)
# p-value=0.0045 **,significant

#MaxHR
MaxHR = aov(cardio.data[,8]~cardio.data[,12], data=cardio.data)
summary(MaxHR)
# p-value <2e-16 ***,significant

#oldpeak
oldpeak = aov(cardio.data[,10]~cardio.data[,12], data=cardio.data)
summary(oldpeak)
# p-value <2e-16 ***,significant

# correlation of variables
cor(cardio.data, cardio.data,method=c("pearson"))
#they all have a relatively strong relationship with heartdiease, chestpaintype, exerciseangina, oldpeak,st_slope



# remove outliers
#outliers in age
par(mfrow = c(1, 1))
boxplot(cardio.data[,1])
#no ourliers

#outliers in restingbp
par(mfrow = c(1, 1))
boxplot(cardio.data[,4])
boxplot(cardio.data[,4], plot=FALSE)$out->out1
cardio.data[which(cardio.data[,4] %in% out1),]
out1

cardio.data <- cardio.data[-which(cardio.data[,4] %in% out1),]
boxplot(cardio.data[,4])
dim(cardio.data)

#outliers in cholesterol
par(mfrow = c(1, 1))
boxplot(cardio.data[,5])
boxplot(cardio.data[,5], plot=FALSE)$out->out2
out2

cardio.data[which(cardio.data[,5] %in% out2),]
cardio.data <- cardio.data[-which(cardio.data[,5] %in% out2),]
boxplot(cardio.data[,5])
dim(cardio.data)

#outliers in maxhr
par(mfrow = c(1, 1))
boxplot(cardio.data[,8])


#outliers in oldpeak
par(mfrow = c(1, 1))
boxplot(cardio.data[,10])
boxplot(cardio.data[,10], plot=FALSE)$out->out3
out3

cardio.data[which(cardio.data[,10] %in% out3),]
cardio.data <- cardio.data[-which(cardio.data[,10] %in% out3),]
boxplot(cardio.data[,10])
dim(cardio.data)

View(cardio.data)

write.csv(cardio.data,file="cardio_data.csv",row.names = F)

#######################End of  Section#######################
#############################################################




