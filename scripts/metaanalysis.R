library(tidyverse)
sparrow <- read_csv("./data/sparrow.csv")
str(sparrow)
View(sparrow)
ggplot(data = sparrow, aes(x = n, y = r)) + geom_point() + geom_hline(yintercept = 0, linetype = "dashed")


# Aggressive Bibs - fixed effects model -----------------------------------

# z = 0.5 ln((1 + r)/(1 – r))
# SEz = 1/√(n – 3)
# Each weight is the inverse of the squared standard error
sparrow <- sparrow %>% 
  mutate(z = 0.5 * log((1 + r)/(1-r)),
         SE = 1/sqrt(n - 3),
         weight = 1/(SE^2))

# calculate the weighted mean, z, of the z-transformed correlations
wt_mean <- weighted.mean(sparrow$z, sparrow$weight)

# does it differ from the unweighted mean of the z-transformed correlations?
mean(sparrow$z)

# Calculate the standard error of the weighted mean
# SEz = √(1/∑w)
SE_z <- sqrt(1/sum(sparrow$weight))

# Calculate an approximate 95% confidence interval 
# for the mean of the transformed effect sizes using the normal approximation 
tanh(wt_mean + SE_z * qnorm(1 - 0.05/2))
wt_mean - SE_z * qnorm(1 - 0.05/2)

# r = tanh(z)
r_mean <- tanh(wt_mean)

# Add a horizontal line indicating the mean effect size to your 
# funnel plot created in the previous section
ggplot(data = sparrow, aes(x = n, y = r)) + geom_point() + geom_hline(yintercept = r_mean, linetype = "dashed")

# back-transformation to the lower and upper limits of your confidence interval
# in (4) to yield the 95% confidence interval for the mean correlation coefficient
tanh(wt_mean + SE_z * qnorm(1 - 0.05/2))
tanh(wt_mean - SE_z * qnorm(1 - 0.05/2))

# Aggressive Bibs - random effects model -----------------------------------

#  estimate the variance among the system-specific effect sizes, Τ2 

