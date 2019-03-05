#Libraries
library(readr)
library(e1071)

#Upload Datasheet
adultdata <- read_csv("adult.data", col_names = FALSE)
View(adultdata)

#Eliminate missing values
adultdata[adultdata=="?"]<-NA
adultdata<-na.omit(adultdata)
View(adultdata)

#Convert to factor
adultdata$X1 <- factor(adultdata$X1)
adultdata$X5 <- factor(adultdata$X5)
adultdata$X11 <- factor(adultdata$X11)
adultdata$X12 <- factor(adultdata$X12)
adultdata$X13 <- factor(adultdata$X13)
adultdata$X15 <- factor(adultdata$X15)

#Select variables
selected.var <- c(1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)
View(selected.var)

# Divide Data
set.seed(1)
train.index <- sample(c(1:dim(adultdata)[1]), dim(adultdata)[1]*0.6)
train.df <- adultdata[train.index, selected.var]
valid.df <- adultdata[-train.index, selected.var]

# Nb model
adultdata.nb <- naiveBayes(X15 ~ ., data = train.df)
adultdata.nb
summary(adultdata.nb)

# Calculation of the accuracy of the model
library(caret)

# Training
pred.class <- predict(adultdata.nb, newdata = train.df)
confusionMatrix(pred.class, train.df$X15)

# Validation
pred.class <- predict(adultdata.nb, newdata = valid.df)
confusionMatrix(pred.class, valid.df$X15)

#Create new row to predict his status
newdata <- data.frame("X1"= 39, "X2"= "Private","X4"="Assoc-voc","X5"=9,"X6"="Married-civ-spouse","X7"="Exec-managerial","X8"="Husband","X9"="White","X10"="Male","X11"=5000, "X12"=0, "X13"=40, "X14"="United-States")
newdata[,c(2,3,5,6,7,8,9,13)] <- as.factor(as.character(newdata[,c(2,3,5,6,7,8,9,13)]))
newdata <- data.frame("X1"= 39, "X2"= "Private","X4"="Assoc-voc","X5"=9,"X6"="Married-civ-spouse","X7"="Exec-managerial","X8"="Husband","X9"="White","X10"="Male","X11"=5000, "X12"=0, "X13"=40, "X14"="United-States")


prediction <- predict(adultdata.nb, newdata)
summary(prediction)