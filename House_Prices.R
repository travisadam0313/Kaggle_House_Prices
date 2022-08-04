#Kaggle House Prices Competition Submission
library(ggplot2)
library(magrittr)
library(dplyr)
library(rpart)
library(rattle)
library(randomForest)
library(tidyr)

#set directory, stage data
setwd("~/R/Classification")

house_train<-read.csv('train.csv')
house_test<-read.csv('test.csv')

#Testing integer columns for statistical significance
int_columns<-c(2,4,5,18,19,20,21,27,35,37,38,39,44,45,46,47,48,49,50,51,52,53,55,67,68,69,77,78)

cor_results<-matrix(nrow=0,ncol=2)
colnames(cor_results)<-c('Column_Name','P_Value')
for(i in int_columns){
  ct<-cor.test(house_train[,i],house_train$SalePrice)
  print(colnames(house_train[i]))
  print(ct$p.value)
  cor_results<-rbind(cor_results,c(colnames(house_train[i]),ct$p.value))
}

cor_results<-as.data.frame(cor_results)
cor_results$P_Value<-as.numeric(cor_results$P_Value)
cor_results <- cor_results[order(cor_results$P_Value),]
View(cor_results)

#Factor Analysis
factor_columns<-c(3,6,7,8,9,10,11,12,13,14,15,16,17,23,24,25,26,27,28,29,30,31,32,33,34,40,41,42,43,52,53,54,55)

for(i in factor_columns){
  house_train[,i]<-as.factor(house_train[,i])
}



#Plots

#p <- ggplot(house_train, aes(LotArea, SalePrice))
#p + geom_point(aes(colour = factor(MSZoning)))

l <- ggplot(house_train, aes(OverallQual,SalePrice))
l + geom_point()

m <- ggplot(house_train, aes(GrLivArea,SalePrice))
m + geom_point()

house_train_pairs<-house_train[,c(18,47,39,44,81)] #these are the best correlations 
pairs(house_train_pairs)

house_train_pairs_2<-house_train[,c(78,46,49,37,81)]#these are the worst
pairs(house_train_pairs_2)

#Bar Avg
ggplot(house_train, aes(MSZoning, LotArea)) + 
  geom_bar(position = "dodge",
           stat = "summary",
           fun = "mean")

#Random Forest for Variable Importance:
fit <- randomForest(SalePrice ~ 
                      MSZoning +
                      Street+
                      #Alley
                      LotShape+
                      Foundation+
                      LandContour+
                      Utilities+
                      LotConfig+
                      LandSlope+
                      Neighborhood+
                      Condition1+
                      Condition2+
                      BldgType+
                      HouseStyle+
                      RoofMatl+
                      Heating
                    # HeatingQC+
                    # CentralAir+
                    #Electrical
                    ,data=house_train, 
                    importance=TRUE, 
                    ntree=2000)
varImpPlot(fit)

#Test Set Prep
factor_columns<-c(13,16,17,3,30)
for(i in factor_columns){
  house_test[,i]<-as.factor(house_test[,i])
}

house_test$MSZoning[456]<-'RL'
house_test$MSZoning[661]<-'RL'
house_test$MSZoning[757]<-'RL'
house_test$MSZoning[791]<-'RL'
house_test$MSZoning[1445]<-'RL'
house_test$MSZoning<-as.factor(house_test$MSZoning)

#Apply Linear Regression
lmFitAllPredictors<-lm(SalePrice~
                         OverallQual+
                         GrLivArea+
                         TotalBsmtSF+
                         X1stFlrSF+
                         #Neighborhood+
                         #BldgType+
                         Foundation,
                       data = house_train)
summary(lmFitAllPredictors)

prediction<-predict(lmFitAllPredictors, newdata = house_test)

submit <- data.frame(Id = house_test$Id, SalePrice = prediction)

#write submission csv for kaggle
write.csv(submit, file = "thirdlm.csv", row.names = FALSE)