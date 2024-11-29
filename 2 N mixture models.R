# Load necessary library
library(unmarked)
library(arm)
library(AICcmodavg)

# Simulate data
set.seed(123)
n_sites <- 78  # Number of sites
n_visits <- 5   # Number of visits per site

# True abundance (N) follows a Poisson distribution
lambda <- 0.5  # Mean abundance
N <- rpois(n_sites, lambda)

# Detection probability (p)
p <- 4/208 # 0.09

# Observed counts (y)
y <- matrix(NA, nrow = n_sites, ncol = n_visits)
for (i in 1:n_sites) {
  for (j in 1:n_visits) {
    y[i, j] <- rbinom(1, N[i], p)
  }
}

# Survey-level covariates (e.g., temperature and observer)
temp <-rep( c(14.9, 15.2, 12, 9.7, 10.2), times =78)

temperature <- matrix(temp, ncol = 5, byrow = TRUE)

# combine survey-level covariates into a list
survey_covs <- list(
  temperature = temperature
)

# Combine into a unmarked frame
umf <- unmarkedFramePCount(
  y = y,                    # Count matrix
  # siteCovs = site_covs,     # Site-level covariates
  obsCovs = survey_covs     # Survey-level covariates
)

# Fit an N-mixture model 
# K = maximum estimate
fit1 <- pcount(~1 ~1, data = umf, K = 150)
fit2 <- pcount(~1 ~temperature, data = umf, K = 150)

# Compare models
model_list <- list(fit1, fit2)
model_names <- c("Null", "Temp")

aictable <- aictab(model_list, model_names)

# Summary of the model
summary(fit)

# diagnostics
Nmix.gof.test(fit1, nsim = 100)

exp( -0.932 ) * 78
exp( -0.932 + 0.59) * 78
exp( -0.932 - 0.59) * 78


