# Required Libraries
library(MASS)
library(ggplot2)
library(tidyr)
library(latex2exp) # latex in ggplot labs
set.seed(88)

# ---------------- Data Generation -------------------------
# Define parameters - currently following ritvik values to avoid having to do math
gen_length <- 20 # chose smaller value for realism
true_means <- c(2, 8)
true_cov_matrix <- matrix(c(1, 0.5, 0.5, 1), ncol = 2)

# Generate bivariate normal data
dataset <- mvrnorm(
  n = gen_length,
  mu = true_means,
  Sigma = true_cov_matrix
)
# ----------------- Estimation of Parameters with Bayesian --------------

## Defining the functions for the conditional updates
# Followed derivation from page 277 - uniform prior
# Goal: Finding the means theta1 and theta2

# Important Variables - look at page 103
rho <- 0.5 # known
y1 <- mean(dataset[,1]) # fixed
y2 <- mean(dataset[,2])# fixed

gibbs_sample <- function(last_theta1, last_theta2 ) { 
  
  #the actual conditionals
  new_theta1 <- rnorm(1, mean=(y1 + rho*(last_theta2 - y2)),
                      sd=(1-(rho**2)))
  new_theta2 <- rnorm(1, mean=(y2 + rho*(new_theta1 - y1)),
                      sd=(1-(rho**2))) 
  
  return(c(new_theta1, new_theta2)) # the new sample
}

## Setting up the simulations
init_theta1 <- -1000 # where we start
init_theta2 <- -1000

runs <- 200
samples <- matrix(NA, nrow=runs, ncol=2)

for (i in 1:runs) {
  #sampling and saving
  gibber <- gibbs_sample(init_theta1, init_theta2)
  samples[i,1] <- gibber[1]
  samples[i,2] <-gibber[2]
  
  # Updating the initalization values
  init_theta1 <- gibber[1]
  init_theta2 <- gibber[2]
}

df_samples <- data.frame(samples)

#---------------- Plotting Relevant Information -------------
## plotting the simulation
ggplot(df_samples, aes(x = X1, y = X2)) +
                       # xmin = -4, xmax=4,
                       # ymin= -4, ymax=4)) +
  geom_point(color = "black", alpha = 0.4, size = 0.75) +
  labs(
    title = "Gibbs samples",
    x = TeX(r"(Sampled $\theta_{1}$)"),
    y = TeX(r"(Sampled $\theta_{2}$)")
  ) +
  theme_minimal()


# -------------- Trace Plot --------------------
# i did get this from chat, but will look more closeslt at it
# the weird indexing is for burnin skipping
# trace_data <- as.data.frame(samples[seq.int(500 + 1, runs), , drop = FALSE])
# trace_data$iteration <- seq_len(500) # will have to explore this more
trace_data <- as.data.frame(samples)
trace_data$iteration <- seq_len(100)

trace_data <- pivot_longer(
  trace_data,
  cols = c(V1, V2),
  names_to = "parameter",
  values_to = "draw"
)

trace_data$parameter <- factor( # I'm confused on why this fixes plots
  trace_data$parameter,
  levels = c("V1", "V2"),
  labels = c("theta1", "theta2")
)
reference <- data.frame(
  parameter = c("theta1", "theta2"),
  posterior_mean = true_means
)

ggplot(trace_data, aes(x = iteration, y = draw)) +
  geom_line(linewidth = 0.3, alpha = 0.7) +
  geom_hline(
    data = reference,
    aes(yintercept = posterior_mean),
    colour = "firebrick",
    linetype = "dashed"
  ) +
  facet_wrap(~ parameter, ncol = 1) +
  labs(x = "Gibbs iteration", y = "Sampled value") +
  theme_minimal()





