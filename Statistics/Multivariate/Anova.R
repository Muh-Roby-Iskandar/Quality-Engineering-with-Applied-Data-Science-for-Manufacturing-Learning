library(stats) ;
library(car);  # Untuk uji ANOVA yang lebih rinci library(ggplot2)  # Untuk visualisasi library(emmeans)  # Untuk estimasi marginal means seperti di SPSS
library(emmeans)

set.seed(123) 
data <- data.frame(Material_Quality = 
                     factor(sample(1:10, 100, replace = TRUE)), 
                   Operator_Skill = factor(sample(1:5, 100, replace = TRUE)), 
                   Machine_Speed = runif(100, 50, 150), Defect_Rate = rnorm(100, mean = 5, sd = 2),
                   Production_Time = rnorm(100, mean = 100, sd = 15), 
                   Output_Quality = rnorm(100, mean = 80, sd = 10));

anova_result <- aov(Defect_Rate ~ Material_Quality, data = data) 
summary(anova_result)
Anova(anova_result, type = "III")  # Seperti SPSS (Type III SS)


manova_result <- manova(cbind(Output_Quality, Production_Time) ~ Machine_Speed, data = data)
summary(manova_result, test = "Wilks")  # Menggunakan uji Wilks' Lambda seperti di SPSS

ancova_result <- aov(Defect_Rate ~ Operator_Skill + Machine_Speed, data = data)
summary(ancova_result)
Anova(ancova_result, type = "III")


mancova_result <- manova(cbind(Output_Quality, Production_Time) ~ Operator_Skill + Machine_Speed, data = data)
summary(mancova_result, test = "Wilks")


lm_result <- lm(Defect_Rate ~ Machine_Speed, data = data)
summary(lm_result)
anova(lm_result)


mlm_result <- lm(Defect_Rate ~ Material_Quality + Operator_Skill + Machine_Speed, data = data)
summary(mlm_result) 
anova(mlm_result)


glm_result <- glm(Defect_Rate ~ Material_Quality + Operator_Skill + Machine_Speed, data = data, family = gaussian())
summary(glm_result) 
Anova(glm_result, type = "III")


emmeans(anova_result, pairwise ~ Material_Quality, adjust = "bonferroni")
