
# Warmup ------------------------------------------------------------------

# The probability of heads in a coin toss is 0.5. If you flip a coin 10 times, what is the probability of obtaining exactly 5 heads and 5 tails?

dbinom(5, size = 10, prob = 0.5)

# The fraction of human babies born who are boys is about 0.512. 
# If 20 newborn babies are randomly sampled, what is the probability that exactly 10 are boys?
dbinom(10, size = 20, prob = 0.512)


# Plot the entire probability distribution for the number of boys in families having six children. 
# Assume the probability that any one child is a boy is 0.512.

boys <- dbinom(1, size = 6, prob = 0.512)
boys[2] <- dbinom(2, size = 6, prob = 0.512)
boys[3] <- dbinom(3, size = 6, prob = 0.512)
boys[4] <- dbinom(4, size = 6, prob = 0.512)
boys[5] <- dbinom(5, size = 6, prob = 0.512)
boys[6] <- dbinom(6, size = 6, prob = 0.512)

plot(boys)

# If the probability of dying in any given year is 0.1, 
# what fraction of individuals are expected to survive 10 years and then die in their 11th year?

dgeom(10, prob = 0.1)

# If the probability of death in any give year is 0.1, 
# what fraction of individuals die before they reach their 6th birthday

dgeom(0, prob = 0.1) + dgeom(1, prob = 0.1) + dgeom(2, prob = 0.1) + dgeom(3, prob = 0.1) + dgeom(4, prob = 0.1) + dgeom(5, prob = 0.1)


# Imagine an environment in which the mean search time between prey items is 0.5 hours. 
# What is the probability density corresponding to a search time of 2 hours?

search_times <- c(0:5)
search_times_density <- dexp(search_times, rate = 2)
plot(search_times_density)

# Illegal tender ----------------------------------------------------------

# Generate a vector that includes a range of possible values for the population proportion p, 
# from 0.01 to 0.99 in increments of 0.01.
p <- seq(0.01, 0.99, by = 0.01)

# Given the dollar bill data above, calculate the log-likelihood of each value for p.
log_like_p <- dbinom(46, size = 50, prob = p, log = TRUE)

# Create a line plot of the log-likelihood against the range of values for p. 
# What is the resulting curve called? 
# Can you see approximately the value of p corresponding to the highest point of the curve? 
# What is this value called?

plot(log_like_p)

# To get closer to this value, repeat steps (1) to (3) 
# using a narrower range of values for p surrounding the highest point in the curve.
p <- seq(0.85, 0.95, by = 0.001)
log_like_p <- dbinom(46, size = 50, prob = p, log = TRUE)
plot(log_like_p)

# Use your results to determine the maximum likelihood estimate of the proportion 
# of US 1-dollar bills contaminated with cocaine.
p[log_like_p == max(log_like_p)]

# 95% confidence intervals 
max(log_like_p) - (1.92)
log_like_p < max(log_like_p) - (1.92)

# Left handed flowers -----------------------------------------------------



# counting elephants ------------------------------------------------------

x <- 15
k <- 74
m <- 27
n <- seq(75,500)
# calculate the maximum likelihood estimate for the total number of elephants in the park
log_like_p <- dhyper(x, m, n, k, log = TRUE)
plot(log_like_p)

# maximum likelihood estimate 
n[log_like_p == max(log_like_p)] + m

# 95% confidence interval
min(n[log_like_p >= max(log_like_p) - (1.92) & log_like_p <= max(log_like_p) + (1.92)] + m)
max(n[log_like_p >= max(log_like_p) - (1.92) & log_like_p <= max(log_like_p) + (1.92)] + m)
