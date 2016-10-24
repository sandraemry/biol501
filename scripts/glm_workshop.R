library(tidyverse)
library(visreg)
songsparrow <- read.csv(file = "./data/songsparrow.csv")
head(songsparrow)
str(songsparrow)
songsparrow$year <- as.factor(songsparrow$year)
songsparrow$survival <- as.factor(songsparrow$survival)
str(songsparrow)

# Plot survival against tarsus length of female sparrows
ggplot(songsparrow, aes(x = tarsus, y = survival)) + geom_jitter(size = 1, color = "red", height = 0.5) + geom_smooth(lwd = 3, se = FALSE)
plot(songsparrow$survival ~ songsparrow$tarsus)
abline(lm(songsparrow$survival ~ songsparrow$tarsus)) 


# fit a glm with link = logit, family = binomial
z <- glm(survival ~ tarsus, family = binomial(link = "logit"), data = songsparrow)

# visualize fit of model 
visreg(z)

# estimated regression coefficients of the model 
summary(z)

plot(jitter(survival, amount = 0.02) ~ tarsus, data = songsparrow)
yhat <- fitted(z)
lines(yhat[order(songsparrow$tarsus)] ~ songsparrow$tarsus[order(songsparrow$tarsus)])

# Use the coefficients to calculate the predicted survival probability of a song sparrow having tarsus length 20.5 mm
xnew <- 20.5
a <- predict(z, newdata = data.frame(tarsus=xnew), interval = "confidence", level = 0.95)
exp(a)/(1 + exp(a))

# calculate the LD50
library(MASS)
dose.p(z, p = 0.50)

# Calculate the likelihood-based 95% confidence interval for the logistic regression coefficients
confint(z, level = 0.95)

# test the null hypothesis of zero slope
anova(z, test = "Chisq")

# add year to logistic regression model 
z <- glm(survival ~ tarsus + year, family = binomial(link = "logit"), data = songsparrow)
visreg(z)
plot(songsparrow$survival ~ songsparrow$tarsus, col = songsparrow$year)

library(lsmeans) 
lsmeans(z, c("year"))


# Crab satellites ---------------------------------------------------------

sat <- read.csv(file = "./data/satellites.csv")
str(sat)
head(sat)
# Plot the number of satellites against the width of the carapace
plot(nsatellites ~ width.cm, data = sat)
# Fit a smooth curve to examine the trend
lines(lowess(sat$width.cm, sat$nsatellites))

# fit a glm, link = , family = poisson
z <- glm(nsatellites ~ width.cm, family = poisson(link = "log"), data = sat)

#visualize fit of model 
visreg(z)

# Plot the data on the original scale, and add the glm model fit to your plot
# Why is it curvilinear?
plot(nsatellites ~ width.cm, data = sat)
yhat <- fitted(z)
lines(yhat[order(sat$width.cm)] ~ sat$width.cm[order(sat$width.cm)])

# Extract the estimated regression coefficients from your model object
summary(z)

# Calculate the likelihood-based 95% confidence interval for the regression coefficients 
# Convert the lower and upper confidence limits for the slope to the original scale
exp(confint(z, level = 0.95))

# test the null hypothesis that slope is zero 
anova(z, test = "Chisq")

# calculate dispersion 
z_disp <- z$deviance / z$df.residual

# Refit a glm without making the assumption that the dispersion parameter is 1
z_new <- glm(nsatellites ~ width.cm, family = quasipoisson(link = "log"), data = sat)

# estimated regression coefficients
summary(z_new)

# Calculate the likelihood-based 95% confidence interval for the regression coefficients 
# Convert the lower and upper confidence limits for the slope to the original scale
exp(confint(z_new, level = 0.95))

#visualize fit of model 
visreg(z_new)

# test the null hypothesis that slope is zero 
anova(z_new, test = "F")


# Prion resistance not futile ---------------------------------------------

kuru <- read.csv(file = "./data/kurudata.csv")
str(kuru)

# Create a contingency table comparing the frequency of the three genotypes at codon 129 
# of the prion protein gene of young and elderly individuals

table(kuru$Genotype, kuru$Cohort)
kuru_prob <- kuru %>% 
  group_by(Cohort) %>% 
  mutate(sum_cohort = n()) %>% 
  group_by(Cohort, Genotype, sum_cohort) %>% 
  summarise (n=n()) %>% 
  mutate(prob = n/sum_cohort) 

  
