library(tidyverse)

bmr <- read.csv("./data/bmr.csv")
str(bmr)
ggplot(data = bmr, aes(x = mass.g, y = bmr.w)) + geom_point() + scale_x_log10() + scale_y_log10() + geom_smooth(method = "lm")

plot(log(bmr.w) ~ log(mass.g), data = bmr)
abline(lm(log(bmr$bmr.w) ~ log(bmr$mass.g)))

plot(log(bmr.w) ~ log(mass.g), data = bmr)
abline(a = z$coefficients[1], b = 0.66)
abline(a = z$coefficients[1], b = 0.75)

z <- lm(log(bmr.w) ~ log(mass.g), data = bmr) 
z1 <- lm(log(bmr.w) ~ 1 + offset(0.666666*log(mass.g)), data = bmr)
z2 <- lm(log(bmr.w) ~ 1 + offset(0.75*log(mass.g)), data = bmr)

confint(z)

summary(z)
summary(z1)
summary(z2)

sum(residuals(z1)^2)
sum(residuals(z2)^2)


# Calculate the log-likelihood of each model  
logLik(z1)
logLik(z2)

AIC(z1)
AIC(z2)

AIC_diff <- AIC(z1) - AIC(z2)

x <- c(AIC(z1), AIC(z2))          # stores AIC values in a vector
delta <- x - min(x)               # AIC differences
L <- exp(-0.5 * delta)            # likelihoods of models
w <- L/sum(L)                     # Akaike weights


# Bird abundance in forest fragments --------------------------------------

bird <- read.csv("./data/birdabund.csv")
str(bird)

hist(bird$abund)
hist(bird$area)
hist(bird$yr.isol)
hist(bird$dist)
hist(bird$ldist)
hist(bird$graze)
hist(bird$alt)

# log transform area, dist, ldist
bird <- bird %>% 
  mutate(log_area = log(area),
         log_dist = log(dist), 
         log_ldist = log(ldist)) %>% 
  select(abund, log_area, yr.isol, log_dist, log_ldist, graze, alt)

hist(bird$log_area)
hist(bird$log_dist)
hist(bird$log_ldist)

pairs(bird)

# Use the cor command to estimate the correlation between pairs of explanatory variables
cor(bird)

library(leaps)
x <- bird %>% 
  select(log_area, yr.isol, log_dist, log_ldist, graze, alt)

# How many predictors does the best model have*?
z <- leaps(x, bird$abund, names=names(x), nbest = 20) 
plot(z$size, z$Cp)  
lines(z$size, z$size)
i <- which(z$Cp==min(z$Cp))
vars <- which(z$which[i,])

# Are there other acceptable models (have Cp < p)?
# not working
j <- which(z$Cp < z$size)
vars <- which(z$which[j,])

# Use a linear model to fit the “best” model to the data. 
mod <- lm(abund ~ log_area + yr.isol + graze, data = bird)

# Produce a summary of the results. 
summary(mod)

# use visreg to visualize the relationship between bird abundance and each of the three variable
library(visreg)
par(mfrow = c(2, 2))
visreg(mod, xvar = "log_area", whitespace = 0.4, 
       points.par = list(cex = 1.1, col = "red"))
visreg(mod, xvar = "yr.isol", whitespace = 0.4, 
       points.par = list(cex = 1.1, col = "red"))
visreg(mod, xvar = "graze", whitespace = 0.4, 
       points.par = list(cex = 1.1, col = "red"))

# calculate AIC and other quantities of interest
z1 <- leaps2aic(x, bird$abund, z)
z1[,c("model","AICc", "AIC")] # prints model and AIC differences

# keep only those cases for which the difference in AICc < 10
# How many models were retained?

z_10$model <- (z1$model[z1$AICc < 10])
z_10$AICc <- (z1$AICc[z1$AICc < 10])
length(z_10)

# Using the AICc values, calculate the Akaike weights of all the models retained.
# How much weight is given to the best model? 

delta <- z_10$AICc - min(z_10$AICc)               # AIC differences
L <- exp(-0.5 * delta)            # likelihoods of models
w <- L/sum(L)                     # Akaike weights

# Are there common features shared among the models having the highest weights?
z_10$model[1:5]

## analyzing the data using stepAIC
library(MASS)
library(car)
z <- lm(bird$abund ~., data = x)     # additive model, all variables
z1 <- stepAIC(z, upper =~ ., lower =~ 1, direction = "both")
summary(z1)
plot(z1)
anova(z1)

mod <- lm(abund ~ log_area + yr.isol + graze, data = bird)
anova(mod)
Anova(mod, type = 3)
summary(mod)
AIC(mod)

# Run stepAIC again, including all two-way interaction terms.
z <- lm(bird$abund ~(.)^2, data = x)  # includes 2-way interactions
z1 <- stepAIC(z, upper =~ ., lower =~ 1, direction = "both")

mod <- lm(abund ~ (log_area + yr.isol + graze) ^ 2, data = bird)
Anova(mod, type = 3)
summary(mod)
AIC(mod)
