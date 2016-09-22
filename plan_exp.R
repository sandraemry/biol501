
# Random Sampling ---------------------------------------------------------
library(dplyr)
library(binom)

# Sample 20 observations from a population having two groups of individuals
# “infected” and “uninfected”, in equal proportions
treatment <- rep(c("infected", "uninfected"), c(50,50))
mydata <- sample(treatment, size = 20, replace = FALSE, prob = NULL)

# Use a table to show the frequencies of the two categories obtained.
table(mydata)

# Sample 18 individuals from a population having two groups of individuals:
# “mated” and “unmated”, where the proportion mated in the population is 0.7
treatment <- rep(c("mated", "unmated"), c(70,30))
mydata <- sample(treatment, size = 18, replace = FALSE, prob = NULL)

# Summarise the frequencies in a table.
table(mydata)

# Sample 30 observations from a normally-distributed population 
# having mean 0 and standard deviation 2
myrand <- rnorm(30, mean = 0, sd = 2)

# Plot the results in a histogram.
hist(myrand)


# Detect a preference -----------------------------------------------------

# Randomly sample n = 10 females 
# from a population having equal numbers of “successes” 
# (females who choose males of her own species) and 
# “failures” (females who choose males of the other species). 
n <- 10
choice <- rep(c("success", "failure"), c(50,50))
pref <- as.data.frame(sample(choice, size = n, replace = FALSE, prob = NULL))

#What was the proportion of successes in your sample?
table(pref)

#counts number of successes
no_of_success <- pref %>% 
  filter(. == "success") %>% 
  count(. == "success")
  

no_of_success <- as.integer(no_of_success[1,2])
prop_of_success <- no_of_success/n

# calculate an approximate 95% confidence interval for the population
# proportion of successes
myCI <- binom.confint(as.integer(no_of_success), n, method = "ac")
print(myCI)
myCI$lower
myCI$upper
myCI$upper - myCI$lower

# loop to calculate confidence intervals 5 times
fiveCI <- vector("numeric", length = c(5))
for(i in 1:10) {
  pref <- as.data.frame(sample(choice, size = n, replace = FALSE, prob = NULL))
  
  no_of_success <- pref %>% 
    filter(. == "success") %>% 
    count(. == "success")
  
  no_of_success <- as.integer(no_of_success[1,2])
  
  myCI <- binom.confint(as.integer(no_of_success), n, method = "ac")
  
  fiveCI <- c(myCI$lower,  myCI$upper)

}

