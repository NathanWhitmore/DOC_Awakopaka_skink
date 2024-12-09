# Load necessary library
library(unmarked)
library(arm)
library(AICcmodavg)

# Simulate data
# set.seed(123)
n_sites <- 36  # Number of sites
n_visits <- 10   # Number of visits per site

# True abundance (N) follows a Poisson distribution
lambda <- 25 /n_sites   # Mean abundance
N <- rpois(n_sites, lambda)

# Detection probability (p)
p <- 0.09


store <- NULL

for(f in 1:10000){

# Observed counts (y)
y <- matrix(NA, nrow = n_sites, ncol = n_visits)
for (i in 1:n_sites) {
  for (j in 1:n_visits) {
    y[i, j] <- rbinom(1, N[i], p)
  }
}

# Combine into a unmarked frame
umf <- unmarkedFramePCount(
  y = y,                    # Count matrix

)

# Fit an N-mixture model 
# K = maximum estimate
fit1 <- pcount(~1 ~1, data = umf, K = 10)



# Summary of the model
# Get summary and extract SE
model_summary <- summary(fit1)

my.N <- model_summary[1] %>% unlist()
my.N <- data.frame(est = my.N[1], se = my.N[2])

store[[f]] <- my.N

print(f)

}


my.est <- bind_rows(store)
my.est$real <- exp(my.est$est) *36

median(my.est$real)

quantile(my.est$real, c(0.025, 0.975))

ggplot()+
  geom_histogram(data = my.est, aes(x = real), binwidth = 1)
  
