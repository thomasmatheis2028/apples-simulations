set.seed(100)

nsim <- 1000
num_treat <- 8
num_blocks <- 7

generate_data <- function(
    blocks = num_blocks,
    treat = num_treat,
    mu = 0,
    sigma_block = 2,
    sigma_error = 1
) {
  
  dat <- expand.grid(
    Rep = factor(1:blocks),
    Rootstock = factor(1:treat)
  )
  
  beta_j <- rnorm(blocks, 0, sigma_block)
  
  # Treatment effects with alpha-tilde = 0.8
  base_tau <- seq(-(treat - 1)/2, (treat - 1)/2, by = 1)
  
  tau <- sqrt(0.8 / mean(base_tau^2)) * base_tau
  
  eps <- rnorm(nrow(dat), 0, sigma_error)
  
  rep_idx <- as.integer(dat$Rep)
  trt_idx <- as.integer(dat$Rootstock)
  
  dat$y <- mu +
    beta_j[rep_idx] +
    tau[trt_idx] +
    eps
  
  dat
}

p_values <- numeric(nsim)

for (s in 1:nsim) {
  
  dat <- generate_data()
  
  fit <- aov(y ~ Rootstock + Rep, data = dat)
  
  p_values[s] <- summary(fit)[[1]]["Rootstock", "Pr(>F)"]
}

power_estimate <- mean(p_values < 0.05)

cat("Estimated power =", power_estimate, "\n")

print("done!")