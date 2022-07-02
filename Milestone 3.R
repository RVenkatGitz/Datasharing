#install packages & libraries
install.packages("dplyr")
library('dplyr')
library('ggplot2')
install.packages("ggplot2")
library('ggplot2')
install.packages("tidyr")
library('tidyr')
#import the csv file Stud_surv 
fifa<-read.csv("fifa21.csv")

#EDA
str(fifa)
#to determine number of na values in each column
colSums(is.na(fifa))
#


colSums(is.na(fifa$Vision))

#to determine unique values in the columns of Interest
unique((fifa$Age))
mean(fifa$Age)
mean(fifa$Stamina)
unique(fifa$foot)
#to determine mean median and values 
#using psych library
fifac<-psych::describe(fifa)
fifac

#Data visualization to plot age, stamina related values for the points of interest

boxplot(fifa$Stamina, 
        main ='Box Plots ',  col=(c("gold")),
        xlab="Stamina")

boxplot(Age ~ Stamina, data = fifa, 
        xlab = "Age",
        ylab = "Stamina", 
        main = "Age vs Stamina",
        notch = FALSE, 
        varwidth = TRUE, 
        col = c("green","yellow","purple")
)

plot(fifa$Age, fifa$Stamina, main="Scatterplot",
     col=(c("blue")),pch=10)

#subset for analysis of stamina of players from Italy vs Argentina
fifa_2 <- subset(fifa, Nationality == "Italy", select = c("Age","Nationality","Stamina","foot"))
head(fifa_2)
fifa_3 <- subset(fifa, Nationality == "Argentina", select = c("Age","Nationality","Stamina","foot"))
head(fifa_3)

t.test(fifa_2$Stamina,fifa_3$Stamina,var.equal = TRUE,conf=0.95)


#one sample t-test
mean(fifa$Age)
t.test(fifa$Age,mu=23,conf=0.95)
t.test(fifa$Age,mu=23,alternative="two.sided", conf=0.95)
         
library(FSA) 
library(FSAdata) 
library(magrittr) 
library(plyr)
library(tidyr)
library(dplyr)  
library(tidyr) 
library(tidyverse) 
library(plotly)  
library(ggplot2) 
library(ggpubr)
library(RColorBrewer) 
library(leaps) 
library(rstatix)
library(corrplot)
library(datarium) 
library(fGarch) 
library(gmodels) 

alymile<- read.csv("fifa.csv") 
aly1 <- subset(fifa, select = c("Age","Nationality","Stamina","foot","ID","Strength","Height","Weight"))
head(aly1)
colSums(is.na(aly1))

#Regression of age vs Stamina of players
aly_reg<-lm(formula=aly1$Stamina~aly1$Age,data=aly1)
plot(aly1$Age,aly1$Stamina, xlab = 'Players Age', ylab = 'Stamina',col="purple",xlim=c(min(aly1$Age),max(aly1$Age)),ylim = c(min(aly1$Stamina),max(aly1$Stamina)))
abline(a=aly_reg$coefficients[1],b=aly_reg$coefficients[2],col="Black")

#Multiple Regression with foot 
lfoot <- subset(aly1,aly1$foot == "Left")
summary(lfoot)
rfoot <- subset(aly1,aly1$foot == "Right")
summary(rfoot)

#To understand the data set
str(aly1) 
summary(aly1)

# Creating subset with dummy variable for Gender
#Assumptions made left= 1 and the right by default is 0
#The categories k=2 and hence k-1 dummy variables are sufficient
mile1<-aly1
mile1$Dummy <- ifelse(mile1$foot=="Left", 1, 0)
head(mile1)
tail(mile1)


#Multiple Regression

dummy_aly <- lm(formula = Stamina ~ Age+ Dummy, data = mile1) 
summary(dummy_aly)

# Visualization plot for Male dummy

plot(mile1$Age[mile1$Dummy==1],mile1$Stamina[mile1$Dummy==1], xlab = 'Players Age', ylab = 'Stamina',col="purple",xlim=c(min(mile1$Age),max(mile1$Age)),ylim = c(min(mile1$Stamina),max(mile1$Stamina)))

plot(mile1$Age[mile1$Dummy==0],mile1$Stamina[mile1$Dummy==0], xlab = 'Players Age', ylab = 'Stamina',col="black",xlim=c(min(mile1$Age),max(mile1$Age)),ylim = c(min(mile1$Stamina),max(mile1$Stamina)))


#regression model
dummy_aly$coefficients

#plot

points(mile1$Stamina[mile1$Dummy==1], mile1$Stamina[mile1$Dummy==1],col="green")

# Multiple regression lines 
abline(a=dummy_aly$coefficients[1],b=dummy_aly$coefficients[2],col="Black")
abline(a=dummy_aly$coefficients[1]+dummy_aly$coefficients[3],b=dummy_aly$coefficients[2],col="Blue")












