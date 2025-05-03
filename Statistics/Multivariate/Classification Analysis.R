library(stats) 
library(psych)  # Statistik deskriptif, validitas, reliabilitas l
library(car)  # VIF, uji asumsi 
library(lmtest)  # Breusch-Pagan Test 
library(MASS)  # Canonical Correlation, Regresi Robust, Diskriminan Analysis l
library(corrplot)  # Visualisasi korelasi 
library(moments)  # Uji Mardia 
library(factoextra)  # KMO test 
library(nortest)  # Uji normalitas tambahan 
library(ggplot2)  # Visualisasi tambahan 
library(glmnet)  # Regresi LASSO dan Ridge 
library(quantreg)  # Regresi Kuantil 
library(MASS)  # Regresi Poisson dan Negatif Binomial 
library(rpart)  # Decision Tree l
library(rpart.plot)  # Visualisasi Decision Tree 
library(e1071)  # Naive Bayes dan SVM

set.seed(123) 
data <- data.frame(
  Material_Quality = rnorm(100, mean = 5, sd = 2), 
  Operator_Skill = rnorm(100, mean = 3, sd = 1), 
  Machine_Speed = rnorm(100, mean = 100, sd = 15), 
  Defect_Rate = rpois(100, lambda = 5), 
  Production_Time = rnorm(100, mean = 100, sd = 15), 
  Output_Quality= rnorm(100, mean = 80, sd = 10) )

data$Defect_Binary <- factor(ifelse(data$Defect_Rate > median(data$Defect_Rate), "High", "Low"))


model_tree <- rpart(Defect_Binary ~ Machine_Speed + Material_Quality + Operator_Skill, data = data, method = "class") 
rpart.plot(model_tree)

logistic_model <- glm(Defect_Binary ~ Machine_Speed + Material_Quality + Operator_Skill, family = binomial, data = data) 
summary(logistic_model)

lda_model <- lda(Defect_Binary ~ Machine_Speed + Material_Quality + Operator_Skill, data = data) 
lda_model

qda_model <- qda(Defect_Binary ~ Machine_Speed + Material_Quality + Operator_Skill, data = data) 
qda_model
