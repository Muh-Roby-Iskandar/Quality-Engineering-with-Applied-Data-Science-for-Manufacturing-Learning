library(brms)

set.seed(123)
n <- 100
data_prod <- data.frame(
  Workers = rnorm(n, mean=50, sd=10),
  Complexity = rnorm(n, mean=3, sd=1),
  ProductionTime = 50 + 2 * rnorm(n, mean=50, sd=10) - 5 * rnorm(n, mean=3, sd=1) + rnorm(n, 0, 5)
)

model_bayes <- brm(ProductionTime ~ Workers + Complexity, 
                   data = data_prod, 
                   family = gaussian(),
                   prior = c(set_prior("normal(0,10)", class = "b")),
                   chains = 4, iter = 2000, warmup = 1000)

summary(model_bayes)



set.seed(123)
n <- 200
data_machine <- data.frame(
  Temperature = rnorm(n, mean=70, sd=10),
  Load = rnorm(n, mean=80, sd=15),
  Failure = rbinom(n, 1, prob=0.3)
)

model_bayes_logit <- brm(Failure ~ Temperature + Load, 
                         data = data_machine, 
                         family = bernoulli(),
                         prior = c(set_prior("normal(0,5)", class = "b")),
                         chains = 4, iter = 2000, warmup = 1000)

summary(model_bayes_logit)


library(BayesFactor)

set.seed(123)
n <- 150
data_anova <- data.frame(
  Factory = factor(rep(1:3, each = n/3)),
  Output = rnorm(n, mean = c(100, 95, 90)[rep(1:3, each = n/3)], sd = 5)
)

bf_model_anova <- anovaBF(Output ~ Factory, data = data_anova)

bf_model_anova



set.seed(123)
n <- 150
data_manova <- data.frame(
  Factory = factor(rep(1:3, each = n/3)),
  Efficiency = rnorm(n, mean = c(90, 85, 80)[rep(1:3, each = n/3)], sd = 5),
  Speed = rnorm(n, mean = c(300, 290, 280)[rep(1:3, each = n/3)], sd = 10)
)

bf_model_manova <- manovaBF(cbind(Efficiency, Speed) ~ Factory, data = data_manova)

bf_model_manova


library(bsts)

set.seed(123)
n <- 100
time_series_data <- ts(rnorm(n, mean=500, sd=50))

ss <- AddLocalLevel(list(), time_series_data)
model_bayes_time <- bsts(time_series_data ~ 1, state.specification = ss, niter = 2000)

summary(model_bayes_time)



library(bayesm)

set.seed(123)
n <- 200
data_cluster <- data.frame(
  Temperature = rnorm(n, mean=70, sd=10),
  Load = rnorm(n, mean=80, sd=15)
)

nmix <- list(ncomp = 3)  # Menentukan 3 cluster
mcmc <- list(R = 1000, keep = 1)

model_bayes_cluster <- rnmixGibbs(nmix, mcmc, X = as.matrix(data_cluster))

summary(model_bayes_cluster)



install.packages("Fpca")
library(Fpca)

set.seed(123)
n <- 100
data_pca <- matrix(rnorm(n * 5, mean = 0, sd = 1), ncol = 5)

model_bayes_pca <- bpca(data_pca, iter = 2000, burnin = 1000, thin = 10)

summary(model_bayes_pca)



install.packages("rstanarm")
library(rstanarm)

set.seed(123)
n <- 150
data_survival <- data.frame(
  Lifetime = rexp(n, rate = 1/100),
  Temperature = rnorm(n, mean=70, sd=10),
  Load = rnorm(n, mean=80, sd=15)
)

model_bayes_surv <- stan_surv(Surv(Lifetime) ~ Temperature + Load, 
                              data = data_survival, 
                              basehaz = "exp",
                              chains = 4, iter = 2000, warmup = 1000)

summary(model_bayes_surv)

