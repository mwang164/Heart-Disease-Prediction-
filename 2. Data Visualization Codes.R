#read csv file (cardio_data.csv)
cardio.data <- read.csv("https://raw.githubusercontent.com/mwang164/Heart-Disease-Prediction-/main/cardio_data.csv", header = TRUE)

#################Data Visualization Section##################
#############################################################
library(leaps)
library(ggplot2)
library(dplyr)
library(reshape2)
library(ggrepel)
library(tidyr)
library(hrbrthemes)
library(ggpubr)
library(GGally)
library(tidyverse)


data.vis <-cardio.data
data.vis[,2]<-factor(data.vis[,2])
data.vis[,3]<-factor(data.vis[,3])
data.vis[,6]<-factor(data.vis[,6])
data.vis[,7]<-factor(data.vis[,7])
data.vis[,9]<-factor(data.vis[,9])
data.vis[,11]<-factor(data.vis[,11])
data.vis[,12]<-factor(data.vis[,12])

summary(data.vis)

#Age
Age <- ggplot(data = data.vis,aes(x=Age,y = ..density..))+
  geom_density(adjust=1.1,alpha= 0.5,position = "identity",)
#Sex
Sex <- data.frame(
  Group = c("Male", "Female"),
  value = c(527,165))%>% ggplot(aes(x='', y=value, fill=Group)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)


#Sex <- ggplot(data.vis, aes(x="", y=Sex, fill=Sex)) +
 # geom_bar(stat="identity", width=1) +
  #coord_polar("y", start=0)+
  #scale_fill_manual(values=c('0' = "#00BFFF", '1' = "#FF3030"),labels=c("Male", "Female"))
#HeartDisease

H <- data.frame(
  Group = c("Normal", "Heart Disease"),
  value = c(372,320))%>% ggplot(aes(x='', y=value, fill=Group)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)

ggarrange(Age,Sex,H,
          labels = c("Age", "Sex", "HeartDisease"),
          ncol = 3, nrow = 1)




#-------------------------------------------------------------------------------#
### All the data review

data.pair <- data.vis[c(1,9,10,12)]
data.pair$ExerciseAngina <- fct_recode(
  data.pair$ExerciseAngina,
  "Yes"="0", "No"="1")
data.pair$HeartDisease <- fct_recode(
  data.pair$HeartDisease,
  "Normal"="0", "HeartDisease"="1")

ggpairs(data.pair, ggplot2::aes(colour=HeartDisease))+
  scale_color_manual(values=c('Normal' = "grey", 'HeartDisease' = "red"),labels=c("Normal", "Heart disease"))+
  scale_fill_manual(values=c('Normal' = "grey", 'HeartDisease' = "red"),labels=c("Normal", "Heart disease"))
#-------------------------------------------------------------------------------#
### Correlation plot
ggcorr(data.vis, method = c("everything", "pearson"))

###density&box plot for three continous variables: BP, MaxHR and Cholesterol
#-------------------------------------------------------------------------------#
#BP
p1 <- ggplot(data = data.vis,aes(x=RestingBP,y = ..density.., group=Sex,fill = Sex))+
  scale_fill_manual(values=c('0' = "blue", '1' = "red"),labels=c("Male", "Female"))+
  geom_density(adjust=1.3,alpha= 0.5,position = "identity",)+
  geom_histogram(aes(x=RestingBP,y = ..density..,group=Sex,fill=Sex),position = "identity",bins=9,alpha= 0.2)+
  theme_ipsum()

p12 <- ggplot(data=data.vis, aes(x=RestingBP, group=Sex,fill = Sex))+
  scale_fill_manual(values=c('0' = "blue", '1' = "red"),labels=c("Male", "Female"))+
  geom_boxplot(alpha = 0.8)+  theme_ipsum()+theme(legend.position="none")


#Cholesterol
p2 <- ggplot(data = data.vis,aes(x=Cholesterol,y = ..density.., group=Sex,fill = Sex))+
  scale_fill_manual(values=c('0' = "blue", '1' = "red"),labels=c("Male", "Female"))+
  geom_density(adjust=1.1,alpha= 0.5,position = "identity",)+
  geom_histogram(aes(x=Cholesterol,y = ..density..,group=Sex,fill=Sex),position = "identity", 
                 bins=15,alpha= 0.3)+theme_ipsum()
p22 <- ggplot(data=data.vis, aes(x=Cholesterol, group=Sex,fill = Sex))+
  scale_fill_manual(values=c('0' = "blue", '1' = "red"),labels=c("Male", "Female"))+
  geom_boxplot(alpha = 0.8)+  theme_ipsum()+theme(legend.position="none")

#MaxHR
p3 <- ggplot(data = data.vis,aes(x=MaxHR,y = ..density.., group=Sex,fill = Sex))+
  scale_fill_manual(values=c('0' = "blue", '1' = "red"),labels=c("Male", "Female"))+
  geom_density(adjust=1,alpha= 0.5,position = "identity",)+
  geom_histogram(aes(x=MaxHR,y = ..density..,group=Sex,fill=Sex),position = "identity",bins=12,alpha= 0.3)+
  theme_ipsum()
p32 <- ggplot(data=data.vis, aes(x=MaxHR, group=Sex,fill = Sex))+
  scale_fill_manual(values=c('0' = "blue", '1' = "red"),labels=c("Male", "Female"))+
  geom_boxplot(alpha = 0.8)+  theme_ipsum()+theme(legend.position="none")


ggarrange(p1, p2,p3,p12,p22,p32,
          labels = c("A", "B", "C"),
          ncol = 3, nrow = 2)

#-------------------------------------------------------------------------------#

###Scatter plot

#BP

#Cholesterol
ggplot(data = data.vis,aes(x=Age,y =Cholesterol,group=HeartDisease,color=HeartDisease,
                           shape=Sex))+
  scale_shape_manual(values=c('0' = "+", '1' = "o"),labels=c("Male", "Female"))+
  scale_color_manual(values=c('0' = "grey", '1' = "red"),labels=c("Normal", "Heart disease"))+
  geom_point(size=8)+
  theme_ipsum()

#MaxHR
ggplot(data = data.vis,aes(x=Age,y =MaxHR,group=HeartDisease,color=HeartDisease,
                           shape=Sex))+
  scale_shape_manual(values=c('0' = "+", '1' = "o"),labels=c("Male", "Female"))+
  scale_color_manual(values=c('0' = "grey", '1' = "red"),labels=c("Normal", "Heart disease"))+
  geom_point(size=8)+
  theme_ipsum()

#-------------------------------------------------------------------------------#

##############End of Data Visualization Section##############
#############################################################






