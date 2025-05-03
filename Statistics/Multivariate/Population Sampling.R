library(dplyr)

data <- data.frame(
  ID = 1:1000, 
  Age = sample(20:60, 1000, replace = TRUE), 
  Salary = round(rnorm(1000, mean = 50000, sd = 10000), 2), 
  Department = sample(c("Production", "QA", "Logistics", "HR"), 1000, replace = TRUE)
  )

srs_sample <- data %>% sample_n(100)

stratified_sample <- data %>% group_by(Department) %>% sample_n(25)

k <- 10 
systematic_sample <- data[seq(1, nrow(data), by = k), ]

selected_clusters <- sample(unique(data$Department), 2) 
cluster_sample <- data %>% filter(Department %in% selected_clusters)

selected_clusters_multi <- sample(unique(data$Department), 2) 
multistage_sample <- data %>% filter(Department %in% selected_clusters_multi) %>% sample_n(50)

convenience_sample <- head(data, 100)

purposive_sample <- data %>% filter(Salary > 60000)

quota_sample <- data %>% group_by(Department) %>% slice_head(n = round(0.5 * n()))

initial_sample <- data %>% filter(Age > 50) %>% sample_n(5) snowball_sample <- bind_rows(initial_sample, data %>% filter(ID %in% sample(data$ID, 15)))

head(srs_sample)
