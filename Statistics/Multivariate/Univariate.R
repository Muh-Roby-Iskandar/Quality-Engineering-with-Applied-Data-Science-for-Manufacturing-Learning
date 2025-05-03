library(stats) 
library(psych)  # Untuk statistik deskriptif lanjutan library(FactoMineR)  # Untuk PCA dan Correspondence Analysis (CA) library(factoextra)  # Untuk visualisasi PCA dan CA library(lavaan)  # Untuk confirmatory factor analysis (CFA) library(MASS)  # Untuk MDS library(conjoint)  # Untuk Conjoint Analysis
library(moments)

set.seed(123) 
data <- data.frame(
  Material_Quality = rnorm(100, mean = 5, sd = 2), 
  Operator_Skill = rnorm(100, mean = 3, sd = 1), 
  Machine_Speed = rnorm(100, mean = 100, sd = 15), 
  Defect_Rate = rnorm(100, mean = 5, sd = 2), 
  Production_Time = rnorm(100, mean = 100, sd = 15), 
  Output_Quality = rnorm(100, mean = 80, sd = 10) )


summary(data)  

mean(data$Defect_Rate)  # Rata-rata dmedian <- median(data$Defect_Rate)  # Median mode_func <- function(x) {ux <- unique(x); ux[which.max(tabulate(match(x, ux)))]}  # Fungsi modus mode_func(data$Defect_Rate)

var(data$Defect_Rate)  # Varians sd(data$Defect_Rate)  # Simpangan baku IQR(data$Defect_Rate)  # Interquartile Range range(data$Defect_Rate)  # Rentang

skewness(data$Defect_Rate)  # Skewness kurtosis(data$Defect_Rate)  # Kurtosis

shapiro.test(data$Defect_Rate)  # Uji Shapiro-Wilk ks.test(data$Defect_Rate, "pnorm", mean = mean(data$Defect_Rate), sd = sd(data$Defect_Rate))  # Uji Kolmogorov-Smirnov


boxplot(data$Defect_Rate, main = "Boxplot Defect Rate", col = "lightblue")


hist(data$Defect_Rate, main = "Histogram Defect Rate", col = "lightgreen", breaks = 10)

density_plot <- density(data$Defect_Rate) 
plot(density_plot, main = "Density Plot Defect Rate", col = "red")

mu_hypothesis <- 5  # Hipotesis rata-rata single_t_test <- t.test(data$Defect_Rate, mu = mu_hypothesis) single_t_test


single_wilcox_test <- wilcox.test(data$Defect_Rate, mu = mu_hypothesis) 
single_wilcox_test
