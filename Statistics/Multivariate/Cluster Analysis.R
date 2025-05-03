library(ggplot2) 
library(cluster) 
library(factoextra)

set.seed(123) 
data <- data.frame(
  Efficiency = rnorm(50, mean = 80, sd = 5),   # Efficiency in % 
  Defects = rnorm(50, mean = 5, sd = 2),       # Defects per 100 units 
  CycleTime = rnorm(50, mean = 30, sd = 5) 
  )    # Cycle time in minutes )

scaled_data <- scale(data)
                                  
fviz_nbclust(scaled_data, kmeans, method = "wss")
kmeans_result <- kmeans(scaled_data, centers = 3, nstart = 25) 
data$Cluster_KMeans <- as.factor(kmeans_result$cluster)
fviz_cluster(kmeans_result, data = scaled_data)

dist_matrix <- dist(scaled_data, method = "euclidean") 
hc <- hclust(dist_matrix, method = "ward.D2") 
plot(hc, main = "Dendrogram", xlab = "Observation", sub = "") 
abline(h = 6, col = "red")  # Example cut height
hc_clusters <- cutree(hc, k = 3) 
data$Cluster_Hierarchical <- as.factor(hc_clusters)
fviz_cluster(list(data = scaled_data, cluster = hc_clusters))

head(data)
