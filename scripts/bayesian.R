
# counting elephants ------------------------------------------------------
elephants <- data.frame(n = numeric(142), prior = numeric(142), like_p = numeric(142), posterior = numeric(142))

x <- 15
k <- 74
m <- 27
elephants$n <- seq(59,200)
# calculate the maximum likelihood estimate for the total number of elephants in the park
log_like_p <- dhyper(x, m, elephants$n, k, log = TRUE)
plot(log_like_p)

# maximum likelihood estimate 
elephants$n[log_like_p == max(log_like_p)] + m

# 95% confidence interval
min(elephants$n[log_like_p >= max(log_like_p) - (1.92) & log_like_p <= max(log_like_p) + (1.92)] + m)
max(elephants$n[log_like_p >= max(log_like_p) - (1.92) & log_like_p <= max(log_like_p) + (1.92)] + m)

# Calculate the likelihoods of each of these values for n. 
elephants$like_p <- dhyper(x, m, elephants$n, k)

# Plot the likelihoods against n.
plot(elephants$like_p ~ n)

# Create a vector containing the prior probabilities for each of the possible values for n 
elephants$prior <- rep(1/length(elephants$n), length(elephants$n))

# confirm that the prior probabilities sum to 1
sum(elephants$prior)

# Plot the prior probabilities against n
plot(elephants$prior ~ n)

# calculate the posterior probabilities of all the possible values of n
elephants$posterior <- elephants$prior*elephants$like_p / sum(elephants$prior*elephants$like_p)

# confirm that the posterior probabilities sum to 1
sum(elephants$posterior)

# Plot the posterior probabilities against n
plot(elephants$posterior ~ elephants$n)

# What is the most probable value of n + m
elephants$n[elephants$posterior == max(elephants$posterior)] + m

# Calculate the 95% credible interval for n + m
library(tidyverse)

elephants_mod <- elephants %>% 
  arrange(desc(posterior)) %>% 
  arrange(desc(n)) %>% 
  mutate(cum_sum = cumsum(posterior))

min(elephants_mod$n[elephants_mod$cum_sum <= .95]) + m
max(elephants_mod$n[elephants_mod$cum_sum <= .95]) + m

# First, order the posterior probabilities from highest to lowest
post.ordered <- elephants_mod$posterior[order(elephants_mod$posterior, decreasing = TRUE)]
# Remember to order the corresponding n values the same way
n.ordered <- elephants_mod$n[order(elephants_mod$posterior, decreasing=TRUE)]
# Obtain the cumulative sum of the posterior probabilities from lowest to highest
post.cumsum <- cumsum(post.ordered)
# Finally, find n corresponding to a cumulative posterior probability of 0.95. 
range(n.ordered[post.cumsum <= 0.95]) + m
