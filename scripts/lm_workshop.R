
# Load data & packages ----------------------------------------------------

lions <- read.csv("./data/lions.csv")
library(tidyverse)
library(visreg)

# examine data ------------------------------------------------------------

head(lions)
str(lions)
View(lions)

mod_plot1 <- ggplot(lions, aes(x = age, y = black)) + geom_point() + geom_smooth(se = TRUE, method = "lm")

mod1 <- lm(black ~ age, data = lions)

#estimates of coefficient, slope, intercept and standard error
summary(mod1)

#95 confidence interval
confint(mod1, level = 0.95)

#check assumptions of model
plot(mod1)

# remove age outlier and run model again
lions_mod <- lions %>% 
  filter(lions$age != max(lions$age))

#replot new model 
mod2 <- lm(black ~ age, data = lions_mod)
mod_plot2 <- ggplot(lions_mod, aes(x = age, y = black)) + 
  geom_point() + 
  geom_smooth(method = 'lm', aes(fill = 'confidence'), alpha = 0.5) +
  geom_ribbon(aes(fill = 'prediction'),
              alpha = 0.2)
summary(mod2)

visreg(mod1)
visreg(mod2)

# Effects of light treatment on circadian rhythms -------------------------

knees <- read.csv(file = "./data/knees.csv")
str(knees)
head(knees)
levels(knees$treatment)

ggplot(data = knees, aes(x = treatment, y = shift)) + geom_point()

#fit a linear model 
mod3 <- lm(shift ~ treatment, data = knees)
plot(mod3)
model.matrix(mod3)
visreg(mod3)

stripchart(shift ~ treatment, vertical = TRUE, method = "jitter", pch = 16,
           col = "red", data = knees)
yhat <- tapply(fitted(mod3), mydata$treatment, mean)
for(i in 1:length(yhat)){
  lines(rep(yhat[i], 2) ~ c(i-.2, i+.2))
}

confint(mod3, level = 0.95)
summary(mod3)

anova(mod3)
table(mod3)
hist(resid(mod3))


# Fly sex and longevity revisited -----------------------------------------

fruitflies <- read.csv(file = "./data/fruitflies.csv")
str(fruitflies)
levels(fruitflies$treatment)

fruitflies$treatment <- factor(fruitflies$treatment, levels = c("no females added", "1 pregnant female", "1 virgin female", "8 pregnant females", "8 virgin females"))
str(fruitflies$treatment)

ggplot(data = fruitflies, aes(x = thorax.mm, y = longevity.days)) + 
  geom_point(aes(colour = treatment))

mod4 <- lm(longevity.days ~ thorax.mm + treatment, data = fruitflies)
plot(mod4)

# log transform longevity 
fruitflies <- fruitflies %>% 
  mutate(log_longev = log(longevity.days)) %>% View

mod5 <- lm(log_longev ~ thorax.mm + treatment, data = fruitflies)
plot(mod5)
