#################Value Information###############
#https://www.kaggle.com/datasets/fedesoriano/heart-failure-prediction
#
#Age:Age
#Sex: M:Male F:Female
#M=0, F=1
#ChestPainType: TA: Typical Angina, ATA: Atypical Angina, NAP: Non-Anginal Pain, ASY: Asymptomatic 
#ASY=1, NAP=2, ATA=3, TA=4
#RestingBP: resting blood pressure [mm Hg]
#Cholesterol: serum cholesterol [mm/dl]
#FastingBS: fasting blood sugar [1: if FastingBS > 120 mg/dl, 0: otherwise]
#RestingECG: resting electrocardiogram results [Normal: Normal, ST: having ST-T wave abnormality (T wave inversions and/or ST elevation or depression of > 0.05 mV), LVH: showing probable or definite left ventricular hypertrophy by Estes' criteria] 
#Normal=0, ST=1, LVH=2
#MaxHR: maximum heart rate achieved [Numeric value between 60 and 202]
#ExerciseAngina: exercise-induced angina [Y: Yes, N: No]
#N=0, Y=1
#Oldpeak: oldpeak = ST [Numeric value measured in depression]
#ST_Slope: the slope of the peak exercise ST segment [Up: upsloping, Flat: flat, Down: downsloping] 
#Down=0, Flat=1, Up=2
#HeartDisease: output class [1: heart disease, 0: Normal]
#############################################################

###################Data Cleaning Section#####################
#############################################################
#data cleaning (read file heart.csv)
cardio.data.raw <- read.csv("https://raw.githubusercontent.com/mwang164/Heart-Disease-Prediction-/main/heart.csv", header = TRUE)
sex <- ifelse(cardio.data.raw$Sex=="M",0,1)
ChestPainType <- ifelse(cardio.data.raw$ChestPainType=="ASY",1,
                        ifelse(cardio.data.raw$ChestPainType=="NAP",2,
                               ifelse(cardio.data.raw$ChestPainType=="ATA",3,4)))
Cholesterol <- ifelse(cardio.data.raw$Cholesterol==0,NA,cardio.data.raw$Cholesterol)
RestingECG <- ifelse(cardio.data.raw$RestingECG=="Normal",0,
                     ifelse(cardio.data.raw$RestingECG=="ST",1,2))
ExerciseAngina <- ifelse(cardio.data.raw$ExerciseAngina=="N",0,1)
ST_Slope <- ifelse(cardio.data.raw$ST_Slope=="Down",0,
                   ifelse(cardio.data.raw$ST_Slope=="Flat",1,2))
cardio.data <- data.frame(cardio.data.raw$Age, sex,ChestPainType, 
                          cardio.data.raw$RestingBP,Cholesterol, 
                          cardio.data.raw$FastingBS, RestingECG, 
                          cardio.data.raw$MaxHR, ExerciseAngina, 
                          cardio.data.raw$Oldpeak, ST_Slope,
                          cardio.data.raw$HeartDisease)
names(cardio.data)<-c( "Age", "Sex", "ChestPainType", "RestingBP", "Cholesterol","FastingBS", 
                       "RestingECG","MaxHR","ExerciseAngina", "Oldpeak","ST_Slope", "HeartDisease")

#remove the missing value
dim(cardio.data)
sum(is.na(cardio.data))

cardio.data <- na.omit(cardio.data)
sum(is.na(cardio.data))
dim(cardio.data)

#write csv
write.csv(cardio.data, file="heart_cleaned.csv",row.names=F)
################End of Data Cleaning Section#################
#############################################################