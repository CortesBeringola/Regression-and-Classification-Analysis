#Upload Datasheet
library(readr)
adultdata <- read_csv("adult.data", col_names = FALSE)

#Eliminate missing values
adultdata[adultdata=="?"]<-NA
adultdata<-na.omit(adultdata)
View(adultdata)

#Create Dummies
aux.matrix<- model.matrix(~ 0 + X2 + X4 + X6 + X7 + X8 + X9 + X10 + X14 + X15, data = adultdata)
aux<- as.data.frame(aux.matrix)
aux$X16<-ifelse(aux$`X15>50K`>0,1,0)
aux<-aux[,-92]
t(t(names(aux)))

#Create Data with Columns that are not Categorical
aux1<-adultdata[,c(1,3,5,11,12,13)]

#Now we concatenate both and create final Datasheet
adultdata.Definite<-cbind(aux1,aux)

#View(adultdata.Definite)
adultdata.Definite$X16<-factor(adultdata.Definite$X16)
t(t(names(adultdata.Definite)))

#Newdatasheet
adultdata.Definite<-adultdata.Definite[,c(1,2,3,4,5,6,29,30,32,37,38,39,41,43,44,45,46,52,57,98)]
colnames(adultdata.Definite) <- c ("X1", "X3", "X5", "X11", "X12", "X13",  "X6Married.AF.spouse", "X6Married.civ.spouse", "X6Never.married", "X7Exec.managerial", "X7Farming.fishing", "X7Handlers.cleaners","X7Other.service","X7Prof.specialty", "X7Protective.serv", "X7Sales", "X7Tech.support", "X8Wife", "X10Male", "X16")
t(t(names(adultdata.Definite)))

# partition data
set.seed(2)
train.index <- sample(row.names(adultdata.Definite), 0.6*dim(adultdata.Definite)[1])
valid.index <- setdiff(rownames(adultdata.Definite), train.index)
train.df <- adultdata.Definite[train.index, ]
valid.df <- adultdata.Definite[valid.index, ]

#Once we have our new datasheet adequately we create the logistic model with the Training partition
log.fit <- glm(X16 ~., data=train.df, family = "binomial",control = list(maxit = 500))

#data.frame(summary(log.fit)$coefficients, odds = exp(coef(log.fit))) 
round(data.frame(summary(log.fit)$coefficients, odds = exp(coef(log.fit))), 5)
summary(log.fit)

#Predicting
library(caret)
library(lattice)
library(ggplot2)
pred <- predict(log.fit, valid.df)
confusionMatrix(as.factor(ifelse(pred > 0.5, 1, 0)), as.factor(valid.df$X16))

# Predict a value from the validation set

newdata <- data.frame("X1"= 39, "X3"= 215646, "X5"= 11, "X11"= 5000, "X12"= 0, "X13"= 40,  "X6Married-AF-spouse"= 0, "X6Married-civ-spouse"= 1, "X6Never-married"=0, "X7Exec-managerial" = 1, "X7Farming-fishing"=0, "X7Handlers-cleaners"=0,"X7Other-service"=0,"X7Prof-specialty"=0, "X7Protective-serv"=0, "X7Sales"=0, "X7Tech-support"=0, "X8Wife"=0, "X10Male"=0)


pred.prob <- predict(log.fit, newdata = newdata)
pred.prob
