rm(list=ls())

Data<-read_excel("C:/Users/gokul/OneDrive/Desktop/DA PROJECT/Hospital_final.xlsx")

# install.packages(c(Hmisc","caret","tree","randomForest","gbm","ada","readxl","plyr","nnet","rpartScore","vcd","hash"))
library(Hmisc)
library(caret)
library(tree)
library(randomForest)
library(gbm)
library(ada)
library(readxl)
library(plyr)
library(nnet)
library(rpartScore)
library(vcd)
library(hash)
library(magrittr)

Data <- na.omit(Data)
# write.csv(Data, file = 'Hospital.csv')
D <- Data[,13:20]
D<-na.omit(D)

# replacing the strings with numeric characters
map_from <- c("Below the national average","Same as the national average","Above the national average")
map_to <- c("1","2","3")
D$`Mortality national comparison`<-mapvalues(D$`Mortality national comparison`,from=map_from,to=map_to)
D$`Safety of care national comparison`<-mapvalues(D$`Safety of care national comparison`,from=map_from,to=map_to)
D$`Readmission national comparison`<-mapvalues(D$`Readmission national comparison`,from=map_from,to=map_to)
D$`Patient experience national comparison`<-mapvalues(D$`Patient experience national comparison`,from=map_from,to=map_to)
D$`Effectiveness of care national comparison`<-mapvalues(D$`Effectiveness of care national comparison`,from=map_from,to=map_to)
D$`Timeliness of care national comparison`<-mapvalues(D$`Timeliness of care national comparison`,from=map_from,to=map_to)
D$`Efficient use of medical imaging national comparison`<-mapvalues(D$`Efficient use of medical imaging national comparison`,from=map_from,to=map_to)

# converting_to_factors
D$`Mortality national comparison`<-as.factor(D$`Mortality national comparison`)
D$`Safety of care national comparison`<-as.factor(D$`Safety of care national comparison`)
D$`Readmission national comparison`<-as.factor(D$`Readmission national comparison`)
D$`Patient experience national comparison`<-as.factor(D$`Patient experience national comparison`)
D$`Effectiveness of care national comparison`<-as.factor(D$`Effectiveness of care national comparison`)
D$`Timeliness of care national comparison`<-as.factor(D$`Timeliness of care national comparison`)
D$`Efficient use of medical imaging national comparison`<-as.factor(D$`Efficient use of medical imaging national comparison`)
D$`Hospital overall rating` <- as.factor(D$`Hospital overall rating`)

# changing the column names
colnames(D)=c("Hospital","Mortality","Safety","Readmission","Experience","Effectiveness","Timelines","Efficient")

set.seed(10)
# Vaidation set approach
train.idx<-sample(x = 1:nrow(D), size = (0.8*nrow(D)))
tr.D<-D[train.idx,]
te.D<-D[-train.idx,]

# mosaic plot
RatingvsSafety <- table(D$Hospital, D$Safety)
RatingvsExperience <- table(D$Hospital, D$Experience)

par(mfrow=c(1,1))

mosaicplot(RatingvsSafety, color = "lightblue", main = 'Rating vs Safety', xlab = 'Hospital Ratings', ylab = 'Safety')
mosaicplot(RatingvsExperience, color = "lightblue", main = 'Rating vs Experience', xlab = 'Hospital Ratings', ylab = 'Experience')

# Hospital Rating Distribution
plot(D$Hospital, main = "Hospital Rating Distribution", xlab = "Hospital Ratings", ylab = "Observations", col = "lightblue", ylim = c(0,800))

# state data
# Topclass
topclass <- Data[Data$`Hospital overall rating` == '5', ]

states = c(unique(topclass$State))
Best <- data.frame(State = character(),Best.hospital = numeric())

h = hash()
for(state in states)
{
  hospital.state = topclass[topclass$State == state,]
  Best.hospital = 0
  Best.hospital = nrow(hospital.state[hospital.state$`Hospital overall rating` == 5, ])
  Best <- rbind(Best, data.frame(state,Best.hospital))
}

# Top 10 Best Performing Hospitals
Best <- Best %>% arrange(desc(Best.hospital))
Best.sub = Best[1:10,]
barplot(Best.sub$Best.hospital,names = Best.sub$state, xlab="Name of the states", ylab ="Top ranked Hospitals", main="Top 10 states with Highest Rated Hospitals", col = "lightblue", ylim = c(0,20))

# Lowclass
lowclass <- Data[Data$`Hospital overall rating` == '1', ]

states = c(unique(topclass$State))
poor <- data.frame(State = character(),poor.hospital = numeric())

h = hash()
for(state in states)
{
  hospital.state = lowclass[lowclass$State == state,]
  poor.hospital = 0
  poor.hospital = nrow(hospital.state[hospital.state$`Hospital overall rating` == 1, ])
  poor <- rbind(poor, data.frame(state, poor.hospital))
}

# Top 10 Best Performing Hospitals
poor <- poor %>% arrange(desc(poor.hospital))
poor.sub = poor[1:10,]
barplot(poor.sub$poor.hospital,names = poor.sub$state, xlab="Name of the states", ylab ="Least ranked Hospitals", main="Top 10 states with Least Rated Hospitals", col = "lightblue", ylim = c(0,40))

# Sampling

acc <- data.frame()
set.seed(24)
for(i in 1:5){
  
  # Vaidation set approach
  train.idx<-sample(x = 1:nrow(D), size = (0.8*nrow(D)))
  tr.D<-D[train.idx,]
  te.D<-D[-train.idx,]
  
  # null model
  null.D = nullModel(tr.D, as.factor(tr.D$Hospital))
  null.pred = predict(null.D, newdata = te.D, type = "class")
  acc[i,1] <- sum(diag(table(null.pred, te.D$Hospital)))/nrow(te.D)
  
  
  # Multinomial logistic regression
  multinom.D = multinom(Hospital~., data = tr.D)
  
  multinom.pred = predict(multinom.D, newdata = te.D)
  multinom.pred <- as.factor(multinom.pred)
  acc[i,2] <- sum(diag(table(multinom.pred, te.D$Hospital)))/nrow(te.D)
  
  # Rpart(gini index)
  rpart.D = rpart(Hospital~., data = tr.D, method = "class")
  rpart.pred = predict(rpart.D, te.D, type =  "class")
  rpart.pred <- as.factor(rpart.pred)
  acc[i,3] <- sum(diag(table(rpart.pred, te.D$Hospital)))/nrow(te.D)
  
  # Classification tree
  tree.D = tree(Hospital~., data=tr.D)
  tree.pred = predict(tree.D, te.D, type = "class")
  acc[i,4] <- sum(diag(table(tree.pred, te.D$Hospital)))/nrow(te.D)
  
  # Randomforest
  bag.rf = randomForest(Hospital~., data = tr.D, mtry = ncol(tr.D) - 1, importance = TRUE, ntree=400)
  
  bag.rf = randomForest(Hospital~., data = tr.D, mtry = ncol(tr.D) - 1, importance = TRUE, ntree=200)
  
  bag.rf.pred = predict(bag.rf, te.D, type = "class")
  acc[i,5] <- sum(diag(table(bag.rf.pred, te.D$Hospital)))/nrow(te.D)
  
  rf = randomForest(Hospital~., data = tr.D, mtry = 3, importance=TRUE, ntree=200)
  rf.pred = predict(rf, te.D, type = "class")
  acc[i,6] <- sum(diag(table(rf.pred, te.D$Hospital)))/nrow(te.D)
  
  # Boosting
  boost.grad = gbm(Hospital~., data = tr.D, distribution = "multinomial"  , n.trees = 5000)
  boost.grad.pred = predict(boost.grad, newdata = te.D, n.trees = 5000)
  boost.grad.pred <- (apply(boost.grad.pred,1,which.max))
  acc[i,7] <- sum(diag(table(as.factor(boost.grad.pred), te.D$Hospital)))/nrow(te.D)
}


for(i in 1:7){
  acc[6,i] = sum(acc[1:5,i])/5
}

rownames(acc) <- c('1','2','3','4','5','Average Accuracy')
colnames(acc) <- c("Null model", "Multinomial Regression ", "Rpart", "CART", "Bagging", "RandomForest", "Gradient Boosting")

summary(boost.grad)
