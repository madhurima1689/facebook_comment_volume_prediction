####################  CAPSTONE PROJECT  ########################


#Setting up working directory
setwd("C:/Users/mchoudhu/Documents/PGP-BABI")

#Installing and loading necessary packages
install.packages("readxl")
#install.packages("car")
install.packages("caret")
install.packages("class")
install.packages("devtools")
install.packages("e1071")
install.packages("ggplot2")
install.packages("Hmisc")
install.packages("klaR")
install.packages("MASS")
install.packages("nnet")
install.packages("dplyr")
install.packages("plyr")
install.packages("pROC")
install.packages("psych")

install.packages("SDMTools")
install.packages("dplyr")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("randomForest")
install.packages("neuralnet")

library(readxl)
library(car)
library(caret)
library(class)
library(devtools)
library(e1071)
library(ggplot2)
library(Hmisc)
library(klaR)
library(MASS)
library(nnet)
library(plyr)
library(pROC)
library(psych)

library(SDMTools)
library(dplyr)

library(rpart)
library(rpart.plot)
library(randomForest)
library(neuralnet)


mydata = read_excel("FB_Training.xlsx")

attach(mydata)

head(mydata)
dim(mydata)
names(mydata)
str(mydata)
summary(mydata)

hist(`Target Variable`)
boxplot(`Target Variable`)

describe(`Target Variable`)
scatterplot(`Target Variable`,CC1)

describe(mydata)
hist(`Page Category` )



#Removing redundant column Post Promotion Status
mydata=mydata[-c(1,39)]



#MISSING VALUE IDENTIFICATION

for (i in (1:40)) 
{
  sum(is.na(mydata[i]))
  cat("Number of missing values in",colnames(mydata[i]), "=", sum(is.na(mydata[i])),"\n")
}


#MISSING VALUE TREATMENT

mydata$CC5=mydata$CC2-mydata$CC3

mydata=as.data.frame(mydata)
for(i in 1:38)
{
  mydata[,i] <- as.numeric(mydata[,i])
  mydata[is.na(mydata[,i]), i] <- median(mydata[,i], na.rm = TRUE)
}

par(mfrow=c(3,4),pty="m")

#OUTLIER DETECTION
for (i in (1:38)) 
{
  boxplot(mydata[,i],col = "pink", main=colnames(mydata[i]))
}


#OUTLIER TREATMENT

for (i in (1:38)) 
{
    mydata[,i] = ifelse(mydata[,i]<=quantile(mydata[,i],0.05),quantile(mydata[,i],0.05),mydata[,i]) 
    mydata[,i] = ifelse(mydata[,i]>=quantile(mydata[,i],0.95),quantile(mydata[,i],0.95),mydata[,i]) 
}   

#UNIVARIATE ANALYSIS

#Histograms
for (i in (2:39)) 
{
  h = ifelse(max(mydata[,i])==1,1,round(max(mydata[,i]),0)+1)
  l = ifelse(min(mydata[,i])==0,0,round(min(mydata[,i]),0)-1)
  #n = variablenames[i]
  hist(mydata[,i],breaks=seq(l,h,((h-l)/6)),labels=TRUE,
       include.lowest=TRUE,right=TRUE, 
       col="pink",border=1,
       main = colnames(mydata[i]), ylab=NULL, 
       cex.lab=1, cex.axis=1, cex.main=1, cex.sub=1,xlim = c(l,h), 
       ylim = c(0,5000))
}

table(`Post published weekday`)

#MULTIVARIATE ANALYSIS

ggplot(mydata,aes(x=`Target Variable`,y=`Page likes`))+geom_point(col="steelblue", size=3)+
  coord_flip() + geom_smooth(method="lm")


ggplot(mydata,aes(x=`Target Variable`,y=`Page likes`))+
  geom_point(aes(col=`Post published weekday`))+
  coord_flip() + geom_smooth(method="lm")

ggplot(mydata,aes(x=`Target Variable`,y=`Page Checkins`))+
  geom_point(aes(col=`Post published weekday`))+
  coord_flip() + geom_smooth(method="lm")

ggplot(mydata,aes(x=`Target Variable`,y=`Post Length`))+geom_point(col="steelblue", size=1)+
  geom_smooth(method="lm")

ggplot(mydata,aes(x=`Post Length`,y=`Target Variable`))+
  geom_point(col="steelblue")+geom_smooth(method = "lm")

ggplot(mydata,aes(x=`Post Share Count`,y=`Target Variable`))+
  geom_point(col="steelblue")+geom_smooth(method = "lm")

ggplot(mydata, aes(`Page likes`))+ geom_histogram(bins = 10) 



corrplot(cor(mydata[,-c(10,15,38,39,40)]), method = "pie", type = "upper")


corMtrx= cor(mydata[,-c(10,15,38,39,40)])
corMtrx
dev.off()

install.packages("corrplot")
library(corrplot)


# Checking multicollinearity

# Kaiser-Meyer-Olkin Test (KMO) is a measure of how appropriate the 
# given dataset is for Factor Analysis

KMO(corMtrx)
#The Overall MSA value is more than 0.5, hence it is an evidence of 
#the presence of multicollinearity

####################  MULTIPLE LINEAR REGRESSION  ###################################


#Splitting dataset into training and test sets
install.packages("caTools")
library(caTools)

set.seed(1000)
sample=sample.split(mydata,SplitRatio = 0.8)
sample

mydata=mydata[-c(10,15,38)]

data_train=subset(mydata[,-c(36,37)],sample==TRUE)
data_test=subset(mydata[,-c(36,37)],sample==FALSE)

mydata$`Page Category`=as.numeric(mydata$`Page Category`)



#Using all variables
LinearModel=lm(`Target Variable`~.,data=data_train)
LinearModel
summary(LinearModel)
anova(LinearModel)

#install.packages("Rcpp")
library(car)
vif(LinearModel)

anova(LinearModel, test="Chisq")


step = stepAIC(LinearModel, direction="both")
summary(step)



#Using variables selected by StepAIC

LinearModel2=lm(`Target Variable` ~ `Page Checkins` + `Feature 5` + 
                  `Feature 7` + `Feature 8` + `Feature 9` + `Feature 11` + 
                  `Feature 12` + `Feature 13` + `Feature 14` + `Feature 17` + 
                  `Feature 18` + `Feature 19` + `Feature 20` + `Feature 23` + 
                  `Feature 24` + `Feature 27` + CC1 + CC2 + CC3 + CC4 + CC5 + 
                  `Base Time` + `Post Length` + `Post Share Count`,data=data_train)

summary(LinearModel2)
anova(LinearModel2)
vif(LinearModel2)

#After removing heavily correlated variables

names(data_train) <- make.names(names(data_train))
names(data_test) <- make.names(names(data_test))

LinearModel3=lm(`Target Variable` ~ `Page Checkins` + `Feature 5`+
                  `Feature 7` + `Feature 11` + `Feature 13` + 
                  `Feature 18` +  
                  `Feature 27` + CC1 + CC2 + CC3 + CC4  + 
                  `Base Time` + `Post Length` + `Post Share Count`,data=data_train)
summary(LinearModel3)
anova(LinearModel3)
vif(LinearModel3)

str(data_test)

#train RMSE
rmse(actual = data_train$`Target Variable`, predicted = predict(LinearModel3, data_train))
#30.67

#test RMSE
rmse(actual = data_test$`Target Variable`, predicted = predict(LinearModel3, data_test))
#30.98


#ROC curve
library(ROCR)

pred_ROC = predict(LinearModel3, newdata = data_test, type = "response")


library(pROC)
#Plot
glm_response_scores <- predict(LinearModel3,data_test, type="response")
plot(roc(data_test$`Target Variable`, glm_response_scores, direction="<"),
     col="blue", lwd=3, main="Linear Regression",print.auc=TRUE)

#0.67

#Gini
library(ineq)

gini_train = ineq(data_train$`Target Variable`, type="Gini")
gini_train  #0.8986

#Gini is more than 0.6, hence it is a good model
#Higher the gini coefficient, better is the model

gini_test = ineq(data_test$`Target Variable`, type="Gini")
gini_test  #0.8963



#Checking the importance of variables.
varImp(LinearModel3)


#10 fold repeated cross validation with Linear Regression

install.packages("caret")
library(caret)


set.seed(400)
ctrl <- trainControl(method="repeatedcv",number = 10, repeats = 3, classProbs=TRUE) 
lmfit <- train(`Target Variable`~ ., data = data_train, method = "lm", 
                trControl = ctrl, preProcess = c("center","scale"),
                tuneLength = 20, verbose = FALSE)

summary(lmfit)

pred3=predict(lmfit,newdata=data_test)


#train RMSE
rmse(actual = data_train$`Target Variable`, predicted = predict(lmfit, data_train))
#30.54

#test RMSE
rmse(actual = data_test$`Target Variable`, predicted = predict(lmfit, data_test))
#30.96



library(pROC)
#Plot
glm_response_scores <- predict(lmfit,data_test, type="raw")
plot(roc(data_test$`Target Variable`, glm_response_scores, direction="<"),
     col="blue", lwd=3, main="Multiple Linear Regression",print.auc=TRUE)






#######################  RANDOM FOREST  #######################################


library(caTools)

set.seed(1000)
sample=sample.split(mydata,SplitRatio = 0.8)
sample

names(mydata) <- make.names(names(mydata))
str(data_train_rf)
data_train_rf=subset(mydata[,-c(36,37)],sample==TRUE)
data_test_rf=subset(mydata[,-c(36,37)],sample==FALSE)


#mydata$`Page Category`=as.numeric(mydata$`Page Category`)


# Build the regression model using randomForest

data_rf=randomForest(data_train_rf$Target.Variable~.,data=data_train_rf,
                     keep.forest=TRUE, ntree=50)



# View results and understand important attributes
print(data_rf)
print(data_rf$mse) #error rates in all the trees

summary(data_rf)

plot(data_rf)

data_rf$predicted 
data_rf$importance 

varImpPlot(data_rf)



#train RMSE
rmse(actual = data_train_rf$Target.Variable, predicted = predict(data_rf, data_train_rf))
#11.38

#test RMSE
rmse(actual = data_test_rf$Target.Variable, predicted = predict(data_rf, data_test_rf))
#26.27


#ROC curve
library(ROCR)

pred_ROC = predict(data_rf, newdata = data_test_rf, type = "response")


library(pROC)
#Plot
glm_response_scores <- predict(data_rf,data_test_rf, type="response")
plot(roc(data_test_rf$Target.Variable, glm_response_scores, direction="<"),
     col="blue", lwd=3, main="Random Forest",print.auc=TRUE)

#0.777

#Gini
library(ineq)

gini_train = ineq(data_train_rf$Target.Variable, type="Gini")
gini_train    #0.8964

#Gini is more than 0.6, hence it is a good model
#Higher the gini coefficient, better is the model

gini_test = ineq(data_test_rf$Target.Variable, type="Gini")
gini_test     #0.9037




#10 fold repeated cross validation with Random Forest

install.packages("caret")
library(caret)


set.seed(400)
ctrl <- caret::trainControl(method="repeatedcv",number = 10, repeats = 3, classProbs=TRUE) 
rffit <- train(Target.Variable ~ ., data = data_train_rf, method = "rf", 
               trControl = ctrl, preProcess = c("center","scale"),
               tuneLength = 20, verbose = FALSE)

rf.cv <- rf.crossValidation(data_rf, data_train_rf, 
                            p=0.10, n=99, ntree=501) 


?rf.cro

rfcv(data_train_rf, data_train_rf$Target.Variable, cv.fold=5, scale="log", step=0.5)


summary(rffit)

pred3=predict(lmfit,newdata=data_test)


#train RMSE
rmse(actual = data_train_rf$`Target Variable`, predicted = predict(lmfit, data_train))

#test RMSE
rmse(actual = data_test$`Target Variable`, predicted = predict(lmfit, data_test))




library(pROC)
#Plot
glm_response_scores <- predict(lmfit,data_test, type="response")
plot(roc(data_test$`Target Variable`, glm_response_scores, direction="<"),
     col="blue", lwd=3, main="Linear Regression",print.auc=TRUE)



#########################  DECISION TREE  ######################################



library(caTools)

set.seed(1000)
sample=sample.split(mydata,SplitRatio = 0.8)
sample

names(mydata) <- make.names(names(mydata))

data_train_dt=subset(mydata[,-c(36,37)],sample==TRUE)
data_test_dt=subset(mydata[,-c(36,37)],sample==FALSE)

#Setting the control parameters
r.ctrl = rpart.control(minsplit=2, minbucket = 1, cp = 0, maxdepth = 4)

#Building the CART model
data_dt=rpart(formula=data_train_dt$Target.Variable~.,data = data_train_dt, 
              method = "class", control = r.ctrl)
summary(data_dt)

draw.tree(data_dt,cex=1)

printcp(data_dt)
plotcp(data_dt)

#Postpruning of decision tree
ptree<- prune(data_dt,cp= data_dt$cptable[which.min(data_dt$cptable[,"xerror"]),"CP"])

summary(ptree)

printcp(ptree)
plotcp(ptree)

library(maptree)
draw.tree(ptree,cex=1)


data_dt$variable.importance

#train RMSE
rmse(actual = data_train_dt$Target.Variable, predicted = predict(ptree, data_train_dt))
#36.17

#test RMSE
rmse(actual = data_test_dt$Target.Variable, predicted = predict(ptree, data_test_dt))
#35.12


#ROC curve
library(ROCR)

library(pROC)

#Plot
glm_response_scores <- predict(ptree,data_test_dt, type="vector")
plot(roc(data_test_dt$Target.Variable, glm_response_scores, direction="<"),
     col="blue", lwd=3, main="Decision Tree",print.auc=TRUE)
#0.589


#Gini
library(ineq)

gini_train = ineq(data_train_dt$Target.Variable, type="Gini")
gini_train  #0.899

#Gini is more than 0.6, hence it is a good model
#Higher the gini coefficient, better is the model

gini_test = ineq(data_test_dt$Target.Variable, type="Gini")
gini_test    #0.894




#10 fold repeated cross validation with Linear Regression

install.packages("caret")
library(caret)
??trainControl

set.seed(400)
ctrl <- trainControl(method="repeatedcv",number = 5, repeats = 3, classProbs=FALSE) 
dtfit <- train(Target.Variable ~ ., data = data_train_dt, method = "rpart", 
               trControl = ctrl, preProcess = c("center","scale"),
               tuneLength = 20)

summary(dtfit)
plot(dtfit)

varImp(dtfit)

pred3=predict(dtfit,newdata=data_test_dt)

#ROC curve
library(ROCR)

library(pROC)

#Plot
dt_response_scores <- predict(dtfit,data_test_dt, type="raw")
plot(roc(data_test_dt$Target.Variable, dt_response_scores, direction="<"),
     col="blue", lwd=3, main="Decision Tree",print.auc=TRUE)
#0.5

#train RMSE
rmse(actual = data_train_dt$Target.Variable, predicted = predict(dtfit, data_train_dt))
#21.07


#test RMSE
rmse(actual = data_test_dt$Target.Variable, predicted = predict(dtfit, data_test_dt))
#29.51


###################  KNN MODEL  ########################################################

#For KNN model, we need to normalize the variables

normalize<-function(x){
  +return((x-min(x))/(max(x)-min(x)))}


library(caTools)

set.seed(1000)
sample=sample.split(mydata,SplitRatio = 0.8)
sample

names(mydata) <- make.names(names(mydata))

data_train_knn=subset(mydata[,-c(36,37)],sample==TRUE)
data_test_knn=subset(mydata[,-c(36,37)],sample==FALSE)

for (i in (1:36))
{
  data_test_knn[,i]=normalize(data_test_knn[,i])
  data_train_knn[,i]=normalize(data_train_knn[,i])
}

library(class)

#Determining optimal k using Cross Validation
trControl <- caret::trainControl(method  = "cv", number  = 10)
fit.knn.cv <- train(Target.Variable ~ .,
                    method     = "knn",
                    tuneGrid   = expand.grid(k = 2:20),
                    trControl  = trControl,
                    preProcess = c("center","scale"),
                    data       = data_train_knn)
?train
fit.knn.cv
#best k = 19
class(fit.knn.cv)

ctrl <- trainControl(method="repeatedcv",number = 10, repeats = 3, classProbs=TRUE) 
knnFit.rcv <- train(Target.Variable ~ ., data = data_train_knn, method = "knn", 
                    trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)

knnFit.rcv
knnFit.rcv$bestTune
??knn
knn_19<-knn(train=data_train_knn[,-36],test=data_test_knn[,-36], cl=data_train_knn[,36],k=19)
knn_19
plot(knn_19)



ACC.19 <- 100 * sum(data_test_knn$Target.Variable == knn_19)/NROW(data_test_knn$Target.Variable)
ACC.19    #54.39

#i=1
#k.optm=1
#for (i in 1:50){
 # knn.mod <- knn(train=data_train_knn[,-36],test=data_test_knn[,-36], cl=data_train_knn[,36],k=i)
  #k.optm[i] <- 100 * sum(data_train_knn[,36] == knn.mod)/NROW(data_train_knn[,36])
  #k=i
  #cat(k,'=',k.optm[i],'')
  #}

#plot(k.optm)

library(ModelMetrics)
#train RMSE
rmse(actual = data_train_knn$Target.Variable, predicted = predict(fit.knn.cv, data_train_knn))
#0.02434

#test RMSE
rmse(actual = data_test_knn$Target.Variable, predicted = predict(fit.knn.cv, data_test_knn))
#0.0218

predictions <- fit.knn.cv %>% predict(data_test_knn)
head(predictions)
RMSE(predictions, data_test_knn$Target.Variable)
#0.0218



#ROC curve
library(ROCR)

library(pROC)
library(caret)

library(dplyr)
predictions <- fit.knn.cv %>% predict(data_test_knn)
head(predictions)


plot(roc(data_test_knn$Target.Variable, predictions, direction="<"),
     col="blue", lwd=3, main="KNN",print.auc=TRUE)
#0.756


#Gini
library(ineq)

gini_train = ineq(data_train_knn$Target.Variable, type="Gini")
gini_train
#0.896


#Gini is more than 0.6, hence it is a good model
#Higher the gini coefficient, better is the model

gini_test = ineq(data_test_knn$Target.Variable, type="Gini")
gini_test
#0.9026


################################  BAGGING  ############################################################


#BAGGING 

install.packages("gbm")
library(gbm)
install.packages("xgboost")
library(xgboost)
library(ipred)



library(caTools)

set.seed(1000)
sample=sample.split(mydata,SplitRatio = 0.8)
sample

names(mydata) <- make.names(names(mydata))

data_train_bag=subset(mydata[,-c(36,37)],sample==TRUE)
data_test_bag=subset(mydata[,-c(36,37)],sample==FALSE)

data_bagging=bagging(Target.Variable~., data = data_train_bag, 
                     control=rpart.control(maxdepth = 5, minsplit = 4))
data_bagging

summary(data_bagging)

data_test_bag$pred.bag = predict(data_bagging, data_test_bag)

#data_test$pred.bag<- ifelse(data_test$pred.bag<0.5,0,1)
data_test_bag$pred.bag


#train RMSE
rmse(actual = data_train_bag$Target.Variable, predicted = predict(data_bagging, data_train_bag))
#21.27

#test RMSE
rmse(actual = data_test_bag$Target.Variable, predicted = predict(data_bagging, data_test_bag))
#26.78


#ROC curve
library(ROCR)

library(pROC)

#Plot
bag_response_scores <- predict(data_bagging,data_test_bag, type="vector")
plot(roc(data_test_bag$Target.Variable, bag_response_scores, direction="<"),
     col="blue", lwd=3, main="Bagging",print.auc=TRUE)
#0.53


#Gini
library(ineq)

gini_train_bag = ineq(data_train_bag$Target.Variable, type="Gini")
gini_train_bag
#0.898
#Gini is more than 0.6, hence it is a good model
#Higher the gini coefficient, better is the model

gini_test_bag = ineq(data_test_bag$Target.Variable, type="Gini")
gini_test_bag
#0.9

#10 fold repeated cross validation with Bagging

#install.packages("caret")
library(caret)


set.seed(400)
ctrl <- trainControl(method="repeatedcv",number = 5, repeats = 3, classProbs=FALSE) 
bagfit <- train(Target.Variable ~ ., data = data_train_bag, method = "rpart", 
               trControl = ctrl, preProcess = c("center","scale"),
               tuneLength = 20)

summary(bagfit)
plot(bagfit)

varImp(bagfit)


#train RMSE
rmse(actual = data_train_bag$Target.Variable, predicted = predict(bagfit, data_train_bag))
#22.31

#test RMSE
rmse(actual = data_test_bag$Target.Variable, predicted = predict(bagfit, data_test_bag))
#26.78

bag_response_scores <- predict(bagfit,data_test_bag, type="raw")
plot(roc(data_test_bag$Target.Variable, bag_response_scores, direction="<"),
     col="blue", lwd=3, main="Bagging",print.auc=TRUE)



################################  XGBOOST  ############################################

data_features_train = as.matrix(data_train_bag[,1:35])
data_label_train = as.matrix(data_train_bag[,36])
data_features_test = as.matrix(data_test_bag[,1:35])

xgb.fit=xgboost(data = data_features_train, label = data_label_train, eta=
                  0.001, max_depth=3, nrounds = 10000, nfold=5, verbose = 0, 
                early_stopping_rounds = 10)

xgb.fit

data_test_bag$xgb.pred.test<- predict(xgb.fit, data_features_test)

data_train_bag$xgb.pred.train<- predict(xgb.fit, data_features_train)



#train RMSE
rmse(actual = data_train_bag$Target.Variable, predicted = data_train_bag$xgb.pred.train)
#19.58

#test RMSE
rmse(actual = data_test_bag$Target.Variable, predicted = data_test_bag$xgb.pred.test)
#21.587

#Plot
bag_response_scores <- predict(data_bagging,data_test_bag, type="vector")
plot(roc(data_test_bag$Target.Variable, data_test_bag$xgb.pred.test, direction="<"),
     col="blue", lwd=3, main="XGBoost",print.auc=TRUE)
#0.699



###################################  THE END  #############################################



