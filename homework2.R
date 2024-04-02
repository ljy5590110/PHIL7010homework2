#Probelm 1 (a)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(caret)
library(class)
library(pls)

set.seed(123)

#(b)
my_data <- obesity_cleaned

#(c)
categorical_cols <- c("Gender", "ObesityCategory", "Race", "MaritalStatus")
my_data <- mutate_at(my_data, categorical_cols, as.factor)

#(d)
obesity_counts <- table(my_data$ObesityCategory)
print(obesity_counts)

#(e)
scatter_plot <- ggplot(my_data, aes(x = Height, y = Weight, color = ObesityCategory))+
  geom_point() +
  labs(title = "Correlation between Weight and Height by Obesity Category",
       x = "Height",
       y = "Weight")

print(scatter_plot)

#(f)
variables_to_scale <- c("Age", "Height", "Weight", "PhysicalActivityLevel", "DailyCalorieIntake", "SleepHours", "BMI")
my_data[variables_to_scale] <- scale(my_data[variables_to_scale])

#(g)
my_data$Gender <- as.numeric(factor(my_data$Gender))
my_data$Race <- as.numeric(factor(my_data$Race))
my_data$MaritalStatus <- as.numeric(factor(my_data$MaritalStatus))


#(h)
my_data$normal <- ifelse(my_data$ObesityCategory == "Normal weight", 1, 0)
my_data <- subset(my_data, select = -c(ObesityCategory))

#(i)
my_data_rows <- floor(0.7 * nrow(my_data))
train_indices <- sample(c(1:nrow(my_data)), my_data_rows)
train_data <- my_data[train_indices,]
test_data <- my_data[-train_indices,]

#Problem2(a)
glm_model <- glm(normal ~ ., data = train_data, family = "binomial")
summary(glm_model)

#(b)
#Height and Weight, less significant code is SleepHours

#(c)
predictions <- predict(glm_model, newdata = test_data, type = "response")
binary_predictions <- ifelse(predictions >= 0.5, 1, 0)
#(d)
confusion_matrix <- confusionMatrix(factor(binary_predictions), factor(test_data$normal))
accuracy <- confusion_matrix$overall['Accuracy']
sensitivity <- confusion_matrix$byClass['Sensitivity']
specificity <- confusion_matrix$byClass['Specificity']
print(confusion_matrix)
print(paste("Accuracy:", accuracy))
print(paste("Sensiticity:", sensitivity))
print(paste("Specificity:",specificity))

#Problem3 (a)
train_knn <- train_data %>% select(-normal)
test_knn <- test_data %>% select(-normal)
knn_model <- knn(train = train_knn, test = test_knn, cl = train_data$normal, k = 10)

#(b)
misClassError <- mean(knn_model != test_data$normal)
print(paste('Accuracy =', 1-misClassError))

confusionMatrix2 <- confusionMatrix(as.factor(knn_model), as.factor(test_data$normal))
print(confusionMatrix2)

#knn model performed better than logistic regression model in this case, because knn model in this case has higher accuracy, and the sensitivity and sepecificity also performed better than the logistic regression model.

#Problem4 (a)
predictor_variables <- c("Age", "Height", "Weight", "BMI", "DailyCalorieIntake", "SleepHours")

pcr_model <- pcr(train_data$normal ~ ., data = train_data[, c(predictor_variables)], scale = FALSE, validation = "CV")
summary(pcr_model)
#(b) 2 principle components

#(c)
pcr_pred <- predict(pcr_model, newdata = test_data[, c(predictor_variables)], ncomp=2)

#(d)
binary_predictions2 <- ifelse(pcr_pred >= 0.5, 1,0)

confusion_matrix3 <- confusionMatrix(factor(binary_predictions2), factor(test_data$normal))

pcr_accuracy <- confusion_matrix3$overall['Accuracy']
pcr_sensitivity <- confusion_matrix3$byClass['Sensitivity']
pcr_specificity <- confusion_matrix3$byClass['Specificity']

print(confusion_matrix3)
print(paste("Accuracy:", pcr_accuracy))
print(paste("Sensitivity:", pcr_sensitivity))
print(paste("Specificity:", pcr_specificity))
