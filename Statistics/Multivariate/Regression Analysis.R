library(stats) 
library(psych)  # Statistik deskriptif, validitas, reliabilitas 
library(car)  # VIF, uji asumsi 
library(lmtest)  # Breusch-Pagan Test 
library(MASS)  # Canonical Correlation, Regresi Robust 
library(corrplot)  # Visualisasi korelasi 
library(moments)  # Uji Mardia
library(factoextra)  # KMO test 
library(nortest)  # Uji normalitas tambahan 
library(ggplot2)  # Visualisasi tambahan 
library(glmnet)  # Regresi LASSO dan Ridge 
library(quantreg)  # Regresi Kuantil 
library(MASS)  # Regresi Poisson dan Negatif Binomial
library(survival)

set.seed(123) 
data <- data.frame(
  Material_Quality = rnorm(100, mean = 5, sd = 2), 
  Operator_Skill = rnorm(100, mean = 3, sd = 1), 
  Machine_Speed = rnorm(100, mean = 100, sd = 15), 
  Defect_Rate = rpois(100, lambda = 5), 
  Production_Time = rnorm(100, mean = 100, sd = 15), 
  Output_Quality = rnorm(100, mean = 80, sd = 10) )


lm_result <- lm(Defect_Rate ~ Machine_Speed + Material_Quality + Operator_Skill, data = data) 
summary(lm_result)

lm_multi <- lm(Output_Quality ~ Machine_Speed + Material_Quality + Operator_Skill + Production_Time, data = data) 
summary(lm_multi)

data$Defect_Binary <- ifelse(data$Defect_Rate > median(data$Defect_Rate), 1, 0) 
logistic_model <- glm(Defect_Binary ~ Machine_Speed + Material_Quality + Operator_Skill, family = binomial, data = data) 
summary(logistic_model)

poisson_model <- glm(Defect_Rate ~ Machine_Speed + Material_Quality + Operator_Skill, family = poisson, data = data) 
summary(poisson_model)


nb_model <- glm.nb(Defect_Rate ~ Machine_Speed + Material_Quality + Operator_Skill, data = data) 
summary(nb_model)


quant_model <- rq(Output_Quality ~ Machine_Speed + Material_Quality + Operator_Skill, tau = 0.5, data = data) 
summary(quant_model)


x <- model.matrix(Output_Quality ~ Machine_Speed + Material_Quality + Operator_Skill + Production_Time, data = data)[,-1] 
y <- data$Output_Quality 
ridge_model <- glmnet(x, y, alpha = 0)  # Ridge Regression lasso_model <- glmnet(x, y, alpha = 1)  # LASSO Regression

plot(ridge_model, xvar = "lambda", label = TRUE) 
plot(lasso_model, xvar = "lambda", label = TRUE)


robust_model <- rlm(Output_Quality ~ Machine_Speed + Material_Quality + Operator_Skill + Production_Time, data = data) 
summary(robust_model)


probit_model <- glm(Defect_Binary ~ Machine_Speed + Material_Quality + Operator_Skill, family = binomial(link = "probit"), data = data) 
summary(probit_model)

tobit_model <- survreg(Surv(Output_Quality) ~ Machine_Speed + Material_Quality + Operator_Skill + Production_Time, dist = "gaussian", data = data) 
summary(tobit_model)
