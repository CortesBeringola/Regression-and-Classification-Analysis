#Libraries
library(DiscriMiner)
library(readr)
library(caret)
library(lattice)
library(ggplot2)

#Upload Datasheet
adultdata <- read_csv("adult.data", col_names = FALSE)
View(adultdata)

#Factor
adultdata$X15 <- factor(adultdata$X15)

#Eliminate missing values
adultdata[adultdata=="?"]<-NA
adultdata<-na.omit(adultdata)
View(adultdata)

#Create dummies
aux.matrix<- model.matrix(~ 0 + X9 + X10, data = adultdata)
aux<- as.data.frame(aux.matrix)
t(t(names(aux)))

#Create Data with Columns that are not Categorical
aux1<-adultdata[,c(1,3,5,11,12,13,15)]

#Now we concatenate both and create final Datasheet
adultdata.Definite<-cbind(aux,aux1)
View(adultdata.Definite)

#Discriminant Analysis Model
selected.var <- c(5,6,7,8,9,10,11,12) 

da.reg <- linDA(adultdata.Definite[,selected.var], adultdata.Definite[,13])
da.reg
da.reg$functions

confusionMatrix(da.reg$classification, adultdata.Definite$X15)

#Class clasification probabilities
propensity <- exp(da.reg$scores[,1:2])/
  (exp(da.reg$scores[,1])+exp(da.reg$scores[,2]))

res <- data.frame(Classification = da.reg$classification, 
                  Actual = adultdata.Definite$X15,
                  Score = round(da.reg$scores,2), 
                  Propensity = round(propensity,2))
head(res)
