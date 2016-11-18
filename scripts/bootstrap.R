library(tidyverse)
antilles <- read_csv("./data/antilles.csv")
str(antilles)

# shape of the distribution
ggplot(data = antilles, aes(x = immigration.date)) + geom_histogram()
hist(antilles$immigration.date)

# mean and median dates of immigration, in millions of years
mean(antilles$immigration.date)
median(antilles$immigration.date)

# Obtain a single bootstrap replicate of the immigration dates
xboot <- sample(antilles$immigration.date, replace = TRUE)

# plot the results
hist(xboot)

# Write a short loop to generate 10000 bootstrap replicate estimates 
# for the sample median immigration date

z <- vector()        # initialize z (do only once)

for (i in 1:10000) {
  xboot <- sample(antilles$immigration.date, replace = TRUE)
  z[i] <- median(xboot)  
}

# Plot the frequency distribution of your results 
# shape of the sampling distribution
hist(z)

# Using your results to calculate a standard error for the sample median
sd(z)

# Calculate the mean of the bootstrap replicate estimates of the 
# median immigration date to estimate the bias
mean(z)

# Use the percentile method to generate an approximate 95% confidence for the median
quantile(z, probs = c(0.025,0.975))

# Apply the boot package to generate a more accurate boostrap confidence interval
# for the median using the BCa method
library(boot)

boot.mean <- function(z,i){boot.mean <- mean(z[i])}

x <- boot(z, boot.mean, R = 2000)

print(x)  
boot.ci(x, type = "bca")  # 95% confidence interval using BCa
boot.ci(x, type = "perc") # 95% confidence interval using percentile
  