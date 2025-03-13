library(rpart)
library(rpart.plot)
library(pROC)
library(randomForest)
library(tidyverse)
library(randomForestExplainer)
library(caret)
library(party)

df = read.csv("C:\\Users\\wangj\\Desktop\\dataset30.csv")

#clear the data

df = df %>%
  mutate(across(everything(), ~str_remove_all(., ","))) %>%  
  mutate(Hours_PW = as.numeric(Hours_PW),  
         Age = as.numeric(Age))  
df = df %>% filter(Occupation != "?" & Nationality != "?")
df = df %>% select(Age, Education, Marital_Status, Occupation, Sex, Hours_PW, Income)
df = df %>% mutate(across(where(is.character), as.factor))
x = df[,1:6]
y = df[,7]   

##Use K-fold to do the re-sample,which have 5 fold
K = 5
set.seed(1111)
folds = cut(1:1409, breaks=K, labels=FALSE)
sen = sep = acc =numeric(K)
for(k in 1:K){
  x.train = x[which(folds!=k),]
  x.text = x[which(folds==k),]
  y.train = y[which(folds!=k)]
  y.text = y[which(folds==k)]
  pre = cbind(y.train,x.train)
  rf_fit = randomForest(y.train~.,data = pre,ntree = 100,importance = T)
  predictions = predict(rf_fit,newdata =x.text)
  tb = confusionMatrix(y.text,predictions)
  tb.class = tb$byClass
  tb.overall = tb$overall
  sen[k] = tb.class[1]
  sep[k] = tb.class[2]
  acc[k] = tb.overall[1]
}
##the mean of 5 time predict-text outcome
sen_m = sum(sen)/5
sep_m = sum(sep)/5
acc_m =sum(as.numeric(acc))/5
