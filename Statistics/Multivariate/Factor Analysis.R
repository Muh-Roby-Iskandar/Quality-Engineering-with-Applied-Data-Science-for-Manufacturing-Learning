library(stats) 
library(psych)  # Untuk analisis faktor eksploratori (EFA) dan common factor analysis 
library(FactoMineR)  # Untuk PCA library(factoextra)  # Untuk visualisasi PCA 
library(lavaan)  # Untuk confirmatory factor analysis (CFA) 
library(MASS)  # Untuk MDS
# Load the package
library(factoextra)

set.seed(123) 
data <- data.frame(
  Material_Quality = rnorm(100, mean = 5, sd = 2), 
  Operator_Skill = rnorm(100, mean = 3, sd = 1), 
  Machine_Speed = rnorm(100, mean = 100, sd = 15), 
  Defect_Rate = rnorm(100, mean = 5, sd = 2), 
  Production_Time = rnorm(100, mean = 100, sd = 15), 
  Output_Quality = rnorm(100, mean = 80, sd = 10) )


pca_result <- PCA(data, graph = FALSE) 
fviz_screeplot(pca_result, addlabels = TRUE, ylim = c(0, 50)) 
summary(pca_result)

fa_parallel <- fa.parallel(data, fa = "fa", fm = "ml", plot=TRUE)  # Menentukan jumlah faktor optimal 
efa_result <- fa(data, nfactors = 1, rotate = "varimax") 
print(efa_result)


cfa_model <- 'Factor1 =~ Material_Quality + Operator_Skill + Machine_Speed 
Factor2 =~ Defect_Rate + Production_Time + Output_Quality' 
cfa_result <- cfa(cfa_model, data = data) 
summary(cfa_result, fit.measures = TRUE, standardized = TRUE)


cfa_common_result <- factanal(data, factors = 1, rotation = "varimax") 
print(cfa_common_result)


distance_matrix <- dist(scale(data))  # Menghitung jarak antar variabel mds_result <- isoMDS(distance_matrix, k = 2) plot(mds_result$points, type = "n") text(mds_result$points, labels = colnames(data), cex = 0.7)

eigenvalues <- pca_result$eig 
optimal_n_components <- sum(eigenvalues[, 1] > 1)  # Kaiser's rule: pilih eigenvalue > 1 cat("Optimal number of components: ", optimal_n_components, "\n")
