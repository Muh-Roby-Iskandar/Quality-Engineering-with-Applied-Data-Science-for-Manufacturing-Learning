library(stats) 
library(psych)  # Statistik deskriptif, validitas, reliabilitas 
library(car)# VIF, uji asumsi 
library(lmtest)  # Breusch-Pagan Test 
library(MASS)  # Canonical Correlation 
library(corrplot)  # Visualisasi korelasi 
library(moments)  # Uji Mardia 
library(factoextra)  # KMO test 
library(nortest)  # Uji normalitas tambahan 
library(ggplot2)  # Visualisasi tambahan
library(MVN)


set.seed(123) 

data <- data.frame(
  Material_Quality = rnorm(100, mean = 5, sd = 2), 
  Operator_Skill = rnorm(100, mean = 3, sd = 1), 
  Machine_Speed = rnorm(100, mean = 100, sd = 15), 
  Defect_Rate = rnorm(100, mean = 5, sd = 2), 
  Production_Time = rnorm(100, mean = 100, sd = 15), 
  Output_Quality = rnorm(100, mean = 80, sd = 10) )


summary(data)
describe(data)

hist(data$Defect_Rate, main = "Histogram Defect Rate", col = "blue") 
boxplot(data$Defect_Rate, main = "Boxplot Defect Rate", col = "red")


shapiro.test(data$Defect_Rate) 
ks.test(data$Defect_Rate, "pnorm", mean = mean(data$Defect_Rate), sd = sd(data$Defect_Rate)) 
lillie.test(data$Defect_Rate)  # Uji Lilliefors ad.test(data$Defect_Rate)  # Anderson-Darling Test cvm.test(data$Defect_Rate)  # Cramer-von Mises Test

cor_matrix <- cor(data) 
print(cor_matrix) 
corrplot(cor_matrix, method = "circle")

anova_result <- aov(Defect_Rate ~ as.factor(Material_Quality), data = data) 
summary(anova_result)

t.test(data$Defect_Rate ~ as.factor(data$Operator_Skill > median(data$Operator_Skill)))

z_value <- (mean(data$Defect_Rate) - 5) / (sd(data$Defect_Rate) / sqrt(nrow(data))) 
z_value

can_cor <- cancor(data[,1:3], data[,4:6]) 
print(can_cor)


cor.test(data$Machine_Speed, data$Defect_Rate, method = "pearson")

cor.test(data$Machine_Speed, data$Defect_Rate, method = "spearman")

cor.test(data$Machine_Speed, data$Defect_Rate, method = "kendall")


lm_model <- lm(Defect_Rate ~ Material_Quality + Operator_Skill + Machine_Speed, data = data) 
vif(lm_model)

lm_result <- lm(Defect_Rate ~ Machine_Speed, data = data) 
summary(lm_result)


ggplot(data, aes(x = Machine_Speed, y = Defect_Rate)) + geom_point(color = 'blue') + geom_smooth(method = 'lm', col = 'red') + ggtitle("Scatter Plot dengan Regresi")

boxplot.stats(data$Defect_Rate)$out

cooksd <- cooks.distance(lm_result) 
outlier_threshold <- 4 / length(data$Defect_Rate) 
outliers <- which(cooksd > outlier_threshold) 
print(outliers)

table(data$Material_Quality)


bartlett.test(data$Defect_Rate ~ as.factor(Material_Quality), data = data)
                  
leveneTest(data$Defect_Rate ~ as.factor(Material_Quality), data = data)

mardia_test <- mardiaTest(data, qqplot = TRUE) 
mardia_test

kmo_result <- KMO(cor_matrix) 
kmo_result

bptest(lm_result)

dwtest(lm_result)


chi_data <- matrix(sample(1:50, 9, replace = TRUE), nrow = 3) 
chisq.test(chi_data)

ks.test(data$Defect_Rate, "pnorm", mean = mean(data$Defect_Rate), sd = sd(data$Defect_Rate))
