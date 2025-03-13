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


#normal randomforest
set.seed(1111)

##choose 1:130 to be the predict set else are text
x_pre = x[1:839,]
x_te = x[840:1409,]
y_pre = y[1:839]
y_te = y[840:1409]
pre = cbind(y_pre,x_pre)

##do rf  
rf_model = randomForest(y_pre~.,data = pre,ntree = 100,importance = T)
predictions <- predict(rf_model, newdata = x_te)

##T:TRUE F:False P:Pos. N:Nei.
###positive is <=50k
tb = table(predictions,y_te)
TP = tb[1,1]
FP = tb[1,2]
FN = tb[2,1]
TN = tb[2,2]

###acc:the prob of predict right
acc = (TP + TN)/(TP + FP + FN + TN)
TPR = TP/(TP + FN)
TNR = TN/(FP + TN)

###main data Sensitivity, Specificity, 'Positive' Class, Accuracy
confusionMatrix(predictions,y_te)

##roc is the plot of Sensitivity and Specificity ,auc is the area under the roc curve
##higher AUC means that higher prob that detect the TP infront of TN ,which mean are more possible to be good fit
pred_probs = predict(rf_model,newdata = x_te, type = "prob")[,2]
roc_c = roc(y_te,pred_probs)
plot(roc_c)
auc(roc_c)

#plot the rf
#more minimal depth have high weight in predict
explain_forest(rf_model, data = pre)
plot_min_depth_distribution(rf_model)
##decition tree
rpart_tree <- rpart(y_pre ~ ., data=pre)
plot(rpart_tree,compress = TRUE)
text(rpart_tree, use.n = TRUE)

