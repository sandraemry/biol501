library(tidyverse)
arsenic <- read_csv("./data/arsenic.csv")
str(arsenic)

# Graph the data. Explain your graph. What is the pattern in the data?
ggplot(data = arsenic, aes(reorder(x = line, cube.root.height), y = cube.root.height)) + 
  stat_summary(fun.y = median, colour = "red", geom = "point", size = 5) + 
  geom_jitter(position = position_jitter(width = 0.1, height = 0), alpha = 1/2)

# The boxplot shows that the height of the plant tiller is heighest for the high tolerance 
# parent line, the f1 hybrid, and the back crosses with the high tolerance plant. The height
# is lower for the f2 hybrid and the back cross with the low tolerance plant. Finally, 
# the hieght is the lowest for the low tolerance parent line. This pattern indicates that tolerance
# the allele for soil arsenic exhibits dominance, so that if at least one copy of the allele from 
# the high tolerance parent is present, height remains equally as high as the parent genotype. At least some of the plants of 
# the f2 hybrid and the back cross of the f1 hybrid with the low tolerance parent will be 
# homozygous for the recessive allele. This is shown in the data, as these plants have a 
# lower tiller height. However some of these plants will be heterozygous, which pulls the average
# height up closer to that of the homozygous high tolerance parent. However all plants of 
# the low tolerance parent genotype will then be homozygous recessive, and exhibit the shortest
# plant tiller height. 

# Create a table of means and standard deviations of genotypes. 
# Make this a high-quality table rather than simply computer output.
geno_stats <- arsenic %>% 
  group_by(line) %>% 
  summarise(avg = mean(cube.root.height), stdev = sd(cube.root.height))

knitr::kable(geno_stats)

# Add a numeric variable in the data set to represent 
# the proportion of the genome inherited from the high-tolerance parent:
# 1 for the high-tolerance parent genotype
# 0 for the low-tolerance parent genotype
# 0.5 for the F1 and F2 hybrids
# 0.25 for the backcross to the low tolerance population
# 0.75 for the backcross to the high tolerance population

arsenic$prop <- ifelse(arsenic$line == "high", 1,
                       ifelse(arsenic$line == "low", 0,
                              ifelse(arsenic$line == "f1", 0.5,
                                     ifelse(arsenic$line == "f2", 0.5,
                                            ifelse(arsenic$line == "bl", 0.25,
                                                   ifelse(arsenic$line == "bh", 0.75,
                                                          NA))))))

# Make sure that the variable is numeric rather than a factor or character.
str(arsenic$prop)


# model 1: additive model  ------------------------------------------------


# Fit the numeric variable you created in (3) to the height data using a linear model. 

hist(arsenic$cube.root.height)
# model1: additive model - tolerance increases linearly with the 
# proportion of the genome inherited from the high tolerance parent
mod1 <- lm(cube.root.height ~ prop, data = arsenic)

# check assumptions
plot(mod1)
hist(resid(mod1))

summary(mod1)

# plot mod1
plot(cube.root.height ~ prop, data = arsenic)
xnew <- range(arsenic$prop)
# predicted values
yhat <- predict(mod1, newdata = data.frame(prop = xnew))
# Add the regression line to a scatter plot
lines(yhat ~ xnew)

# add confidence bands 
xnew <- seq(min(arsenic$prop), max(arsenic$prop), length.out = 100)
ynew <- data.frame(predict(mod1, newdata = data.frame(prop = xnew), 
                           interval = "confidence", level = 0.95))
plot(cube.root.height ~ prop, data = arsenic)
lines(ynew$fit ~ xnew)
lines(ynew$lwr ~ xnew, lty = 2)
lines(ynew$upr ~ xnew, lty = 2)

# or adding prediction intervals instead
xnew <- seq(min(arsenic$prop), max(arsenic$prop), length.out = 100)
ynew <- data.frame(predict(mod1, newdata = data.frame(prop = xnew), 
                           interval = "prediction", level = 0.95))
plot(cube.root.height ~ prop, data = arsenic)
lines(ynew$fit ~ xnew)
lines(ynew$lwr ~ xnew, lty = 2)
lines(ynew$upr ~ xnew, lty = 2)

# visualizing with visreg instead
library(visreg)
visreg(mod1, points.par = list(cex = 1.2, col = "red")) # with points options

# Evaluate the model fit (Remember: no P values!).



# Add another numeric variable to the data set to represent 
# dominance effects that might be present in the hybrids:
# 0 for both parent genotypes
# 1 for the F1 hybrid
# 0.5 for the remaining three hybrid genotypes
# Make sure that the variable is numeric rather than a factor or character.  

arsenic$dominance <- ifelse(arsenic$line == "high", 0,
                            ifelse(arsenic$line == "low", 0,
                                   ifelse(arsenic$line == "f1", 1,
                                          ifelse(arsenic$line == "f2", 0.5,
                                                 ifelse(arsenic$line == "bl", 0.5,
                                                        ifelse(arsenic$line == "bh", 0.5,
                                                               NA))))))



# model 2: additive plus dominance model ----------------------------------

# Fit a second model to the same data that includes both of the numeric variables created in 
# (3) and (5). 
 # Leave out any interaction terms. 
# This is the additive plus dominance model. 
# Any dominance effects present will displace the mean value of the hybrids toward 
# one or other of the parents relative to the values predicted by the additive model. 
# Evaluate model fit.

mod2 <- lm(cube.root.height ~ prop + dominance, data = arsenic)

# check assumptions
plot(mod2)
hist(resid(mod2))

summary(mod2)

# plot mod2
plot(cube.root.height ~ prop + dominance, data = arsenic)

visreg(mod2, xvar = "dominance", whitespace = 0.4, 
       points.par = list(cex = 1.1, col = "red"))
visreg(mod2, xvar = "prop", by = "dominance", whitespace = 0.5, overlay = TRUE, 
       band = FALSE, points.par = list(cex = 1.1))

# Use the lsmeans package to obtain fitted group means
library(lsmeans)
lsmeans(mod2, c("prop", "dominance"))   # model fitted means* for all combinations of A and B groups

# Evaluate the model fit (Remember: no P values!).


# model 3: epistasis model  ----------------------------------------------


# Finally, fit a third model that has the original genotype variable as the only explanatory variable.
# The fit of this model will deviate from the 2nd model if there is interaction (epistasis)
# between genes inherited from the two parents.

arsenic$line <- as.factor(arsenic$line)
mod3 <- lm(cube.root.height ~ line, data = arsenic)

# check assumptions
plot(mod3)
hist(resid(mod3))

summary(mod3)

# visualize model fit
visreg(mod3, points.par = list(cex = 1.2, col = "red"))

# Present your results, comparing model fits. 
# Which genetic model best fit the data? 
# Explain and summarize

AIC(mod1)
AIC(mod2)
AIC(mod3)

x <- c(AIC(mod1), AIC(mod2), AIC(mod3))  # stores AIC values in a vector
delta <- x - min(x)                # AIC differences

BIC(mod1)
BIC(mod2)
BIC(mod3)

x <- c(BIC(mod1), BIC(mod2), BIC(mod3))  # stores BIC values in a vector
delta <- x - min(x)                      # BIC differences
