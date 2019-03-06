library(plyr)
library(ggplot2)
library(caTools)
library(dplyr)
library(caret)
library(rpart.plot)

data <- read.csv("bank-additional/bank-additional-full.csv",sep=';')

#Factor To numeric
data <- select(data,-duration) # duration removed because irrelevant(explained in readMe)
data$job <- as.numeric(data$job)
data$marital <- as.numeric(data$marital)
data$education <- as.numeric(data$education)
data$default <- as.numeric(data$default)
data$housing <- as.numeric(data$housing)
data$loan <- as.numeric(data$loan)
data$contact <- as.numeric(data$contact)
data$month <- as.numeric(data$month)
data$day_of_week <- as.numeric(data$day_of_week)
data$poutcome <- as.numeric(data$poutcome)

#plots
ggplot(data,aes(x=age)) + geom_histogram()
ggplot(data,aes(x=duration,y=y)) + geom_point()

#decisionTree
data$y <- as.factor(data$y) #conversion to 2-factor

#Diving in Train and test sets
smp_size<-floor(nrow(data)*0.8)
trainind<-sample(seq_len(nrow(data)),size=smp_size)
train<-data[trainind,]
test<-data[-trainind,]

#training by rpart
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
dtree_fit <- train(y~., data = train, method = "rpart",
                   parms = list(split = "information"),
                   trControl=trctrl,
                   tuneLength = 10) #The parameter tuneLength is tuned using CV.

pred <- predict(dtree_fit,test)
#plot of the desicion tree
prp(dtree_fit$finalModel, box.palette = "Reds", tweak = 1.2)

confusion.matrix <- prop.table(table(pred, test$y))
accuracy <- confusion.matrix[1,1] + confusion.matrix[2,2] 
#Accuracy of 90.56%
#mae <- MAE(pred = pred,test$y)
