library(stats) 
library(psych)  # Untuk analisis faktor eksploratori (EFA) dan common factor analysis library(FactoMineR)  # Untuk PCA dan Correspondence Analysis (CA) library(factoextra)  # Untuk visualisasi PCA dan CA library(lavaan)  # Untuk confirmatory factor analysis (CFA) library(MASS)  # Untuk MDS library(conjoint)  # Untuk Conjoint Analysis


set.seed(123) 

data <- data.frame(
  Material_Quality = rnorm(100, mean = 5, sd = 2), 
  Operator_Skill = rnorm(100, mean = 3, sd = 1), 
  Machine_Speed = rnorm(100, mean = 100, sd = 15), 
  Defect_Rate = rnorm(100, mean = 5, sd = 2), 
  Production_Time = rnorm(100, mean = 100, sd = 15), 
  Output_Quality = rnorm(100, mean = 80, sd = 10) )


profiles <- expand.grid(Material = c("Low", "Medium", "High"), Speed = c("Slow", "Moderate", "Fast"), Cost = c("Low", "Medium", "High")) 
ratings <- sample(1:10, nrow(profiles), replace = TRUE)  # Preferensi acak conjoint_data <- cbind(profiles, Rating = ratings) conjoint_result <- caModel(y = conjoint_data$Rating, x = conjoint_data[, c("Material", "Speed", "Cost")]) summary(conjoint_result) caImportance(conjoint_result)  # Kepentingan relatif atribut


category_data <- matrix(sample(1:50, 9, replace = TRUE), nrow = 3, dimnames = list(c("Low", "Medium", "High"), c("Defect_Low", "Defect_Medium", "Defect_High"))) 
ca_result <- CA(category_data, graph = FALSE) 
summary(ca_result) 
fviz_ca_biplot(ca_result, repel = TRUE)  # Visualisasi mirip dengan output SPSS
