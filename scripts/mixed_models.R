library(nlme)
library(tidyverse)


# flycatcher data ---------------------------------------------------------

flycatcher <- read_csv("./data/flycatcher.csv")
str(flycatcher)
head(flycatcher)
tail(flycatcher)

str(flycatcher$year)
flycatcher$year <- as.character(flycatcher$year)
ggplot(data = flycatcher, aes(x = bird, y = patch)) + geom_point(aes(color = year))

#Fit a linear mixed-effects model to the data
#treat the individual birds as the random groups
z <- lme(patch ~ 1, random = ~1|bird, data = flycatcher)
summary(z)
intervals(z)
VarCorr(z)
repeat_z <- 1.24331/(1.24331 + 0.358)

plot(z)

stripchart(flycatcher$patch ~ flycatcher$bird, vertical = TRUE, pch = 1)
stripchart(fitted(z) ~ flycatcher$bird, vertical = TRUE, add = TRUE, pch = "---")


# goldfish data -----------------------------------------------------------

goldfish <- read_csv("./data/goldfish.csv")
str(goldfish)
head(goldfish)
interaction.plot(goldfish$wavelength, goldfish$fish, goldfish$sensitivity)

goldfish$fish <- as.factor(goldfish$fish)
goldfish$wavelength <- as.factor(goldfish$wavelength)
z <- lme(sensitivity ~ wavelength, random = ~1|fish, data = goldfish)
library(visreg)
visreg(z)
plot(z)
summary(z)
library(lsmeans)
lsmeans(z, "wavelength")
anova(z)


# kluane data -------------------------------------------------------------

kluane <- read.csv("./data/kluane.csv")
str(kluane)
head(kluane)

stripchart(log(phen.ach) ~ treatment, vertical=TRUE, 
           data = kluane, method="jitter", pch="")
points(log(phen.ach) ~ c(as.numeric(treatment) - 0.1), 
        data = subset(kluane, duration == "permanent"), pch=16)
points(log(phen.ach) ~ c(as.numeric(treatment) + 0.1), 
        data = subset(kluane, duration == "reverse"), pch=1)

z1 <- lme(log(phen.ach) ~ treatment + duration, random = ~ 1|plot, data = kluane)
z2 <- lme(log(phen.ach) ~ treatment * duration, random = ~ 1|plot, data = kluane)

par(mfrow = c(2,2))
visreg(z1)
visreg(z2)
plot(z)
summary(z)

anova(z)
anova(z, type = "marginal")

AIC(z1)
AIC(z2)
