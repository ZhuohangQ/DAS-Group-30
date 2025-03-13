# 加载必要的包
library(tidyverse)
library(ggplot2)
library(caret)
library(pROC)
library(GGally)

# 读取数据
df <- read.csv("C:\\Users\\Lenovo\\Desktop\\data analysis skills\\dataset30.csv", stringsAsFactors = FALSE)

# 数据清理
df <- df %>%
  mutate(across(everything(), ~str_remove_all(., ","))) %>%  
  mutate(Hours_PW = as.numeric(Hours_PW),  
         Age = as.numeric(Age),  
         Income = ifelse(Income == ">50K", 1, 0))  

df <- df %>% filter(Occupation != "?" & Nationality != "?")

df <- df %>% select(Age, Education, Marital_Status, Occupation, Sex, Hours_PW, Income)

df <- df %>% mutate(across(where(is.character), as.factor))

# 导出处理后的数据为 CSV 格式
write.csv(df, "C:\\Users\\Lenovo\\Desktop\\data analysis skills\\processed_data.csv", row.names = FALSE)

# 查看数据结构
str(df)

# 数据标准化（Age）
df$Age <- scale(df$Age)

# 划分训练集和测试集
set.seed(1111)
trainIndex <- createDataPartition(df$Income, p = 0.8, list = FALSE)
train_data <- df[trainIndex, ]
test_data <- df[-trainIndex, ]

# 描述性统计
summary(df[, sapply(df, is.numeric)])
summary(train_data)

# 构建完整的回归模型
full_model <- glm(Income ~ ., data = train_data, family = binomial)

# 执行逐步回归（后向选择）
stepwise_model <- step(full_model, direction = "backward")

# 测试集评估
test_predictions <- predict(stepwise_model, newdata = test_data, type = "response")
test_predicted_classes <- ifelse(test_predictions > 0.5, 1, 0)
confusionMatrix(factor(test_predicted_classes), factor(test_data$Income))
conf_matrix <- confusionMatrix(factor(test_predicted_classes), factor(test_data$Income))
accuracy <- conf_matrix$overall["Accuracy"]
print(accuracy)

# ROC AUC曲线
roc_curve_test <- roc(test_data$Income, test_predictions)
plot(roc_curve_test, col = "red", main = "ROC Curve for Test Data", xlim = c(1, 0), ylim = c(0, 1), asp = 1)
auc(roc_curve_test)
