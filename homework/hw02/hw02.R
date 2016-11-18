library(nlme)
library(tidyverse)
library(visreg)
library(lsmeans)

# data formatting ------------------------------------------------------------

aging <- read.csv("./data/Aging Data Combined.csv")

# structure of data frame
str(aging)

# first few rows of dataframe
head(aging)

# delete rows with NAs
aging <- aging %>% 
  na.omit()


# plot the data -----------------------------------------------------------

ggplot(data = aging, aes(x = standistance, y = stanstrain)) + geom_point(aes(color = Order)) + 
  xlab("distance") + ylab("strain")

# Fit a linear mixed-effects model to the data ----------------------------

# position along the blade = fixed factor
# species = random factor
# breaking strain = response variable

z <- lme(stanstrain ~ standistance, random = ~1|Species, data = aging)

# view fit of model to data
visreg(z, points.par = list(cex = 0.8, col = "red"), overlay = TRUE, band = TRUE)
visreg(z, whitespace = 0.4)
visreg(z, xvar = "standistance", by = "Species", scales=list(rot = 90))

stripchart(stanstrain ~ standistance, vertical = TRUE, method = "jitter", pch = 16,
           col = "red", data = aging, xlab)
yhat <- tapply(fitted(z), aging$standistance, mean)
for(i in 1:length(yhat)){
  lines(rep(yhat[i], 2) ~ c(i-.2, i+.2))
}

#residuals
plot(z)
hist(resid(z))

# parameter estimates
summary(z)

confint(z)

anova(z)

lsmeans(z, "standistance")

# by species
stripchart(aging$stanstrain ~ aging$Species, vertical = TRUE, pch = 1)
stripchart(fitted(z) ~ aging$Species, vertical = TRUE, add = TRUE, pch = "---", col = "red")

