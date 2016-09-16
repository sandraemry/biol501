
# Read in data ------------------------------------------------------------
mammals <- read.csv(file = "./raw_data/mammals.csv", na.strings="", stringsAsFactors = FALSE, strip.white = TRUE)

# load packages -----------------------------------------------------------

library(dplyr)
library(tidyr)

# data manipulation -------------------------------------------------------

head(mammals)
table(mammals$status, mammals$continent)

#change type from Af to AF
mammals$continent[mammals$continent == "Af"] <- "AF"

mammals <- mammals %>% 
  mutate(log_body_mass = log(mass.grams))


# plots of data - one variable -----------------------------------------------------------

barplot_data <- mammals %>% 
  group_by(continent) %>% 
  summarise(num_sp = n()) %>% 
  select(continent, num_sp) 

barplot(sort(barplot_data$num_sp,decreasing = TRUE), xlab = "continent", ylab = "no. of species", names.arg = barplot_data$continent, cex.names=0.8, ylim = c(0,1700), col = "purple" )

hist(mammals$mass.grams)
hist((mammals$mass.grams)**(1/3))
hist(mammals$log_body_mass)
hist(mammals$log_body_mass, breaks = seq(0, 20, by=2))
hist(mammals$log_body_mass, breaks = seq(0, 20, by=1))
hist(mammals$log_body_mass, breaks = seq(0, 20, by=0.5))

hist(mammals$log_body_mass, breaks = seq(0, 20, by=1), prob = TRUE)
## not working
qnorm(mammals$log_body_mass)

hist(mammals$log_body_mass, breaks = seq(0, 20, by=1), prob = TRUE)
m <- mean(mammals$log_body_mass, na.rm = TRUE)
s <- sd(mammals$log_body_mass, na.rm = TRUE)
## not working
curve(qnorm(mammals$log_body_mass, mean=m, sd=s), col="red", lwd=2, add=TRUE, pch=".")


# plots of data - associations btwn variables -----------------------------

boxplot(mammals$log_body_mass ~ mammals$status, cex.axis = 0.8, varwidth = TRUE)

tapply(mammals$log_body_mass, INDEX = mammals$status, FUN = median, na.rm=TRUE)

#not working
mammals %>% 
  group_by(status) %>% 
  summarise(med = median(!is.na(log_body_mass))) %>% View

tapply(mammals$log_body_mass, INDEX = mammals$status, FUN = mean, na.rm = TRUE)

table(mammals$status, mammals$continent)

mammals_mod <- mammals %>% 
  group_by(status, continent) %>% 
  count(status, continent) %>% 
  filter(status %in% c("extinct", "extant")) %>% 
  group_by(continent) %>% 
  spread(status, n, fill = NA, convert = FALSE) %>% 
  mutate(ratio = extinct/extant)

max(mammals_mod$ratio, na.rm = TRUE)

mammals_mod2 <- mammals %>% 
  filter(status %in% c("extinct", "extant"))

mosaicplot(table(mammals_mod2$continent, mammals_mod2$status), col=TRUE, las=2, cex.axis=0.8)

# Fly sex and longevity ---------------------------------------------------

fruitflies <- read.csv(file = "./raw_data/fruitflies.csv")
head(fruitflies)
str(fruitflies)

par(mfrow = c(1,2))
boxplot(fruitflies$longevity.days ~ fruitflies$treatment, cex.axis = 0.4, varwidth = TRUE, ylab = "longevity (days)")
stripchart(fruitflies$longevity.days ~ fruitflies$treatment, vertical = TRUE, cex.axis = 0.6, method = "jitter", jitter = 0.1, ylab = "longevity (days)")
par(mfrow = c(1,1))
plot(fruitflies$longevity.days ~ fruitflies$thorax.mm, ylab = "longevity (days)")
x1 <- fruitflies$thorax.mm[order(fruitflies$thorax.mm)]
y1 <- fruitflies$longevity.days[order(fruitflies$longevity.days)]
lines(lowess(x1, y1, f = .67))

library(ggplot2)

ggplot(aes(x = thorax.mm, y = longevity.days), data = fruitflies, group = treatment, col = treatment) +geom_point(aes(colour=factor(fruitflies$treatment)))


# multipanel plot ---------------------------------------------------------

one_preg_fem <- fruitflies %>% 
  filter(treatment %in% "1 pregnant female")

one_virg_fem <- fruitflies %>% 
  filter(treatment %in% "1 virgin female")

eight_preg_fem <- fruitflies %>% 
  filter(treatment %in% "8 pregnant females")

eight_virg_fem <- fruitflies %>% 
  filter(treatment %in% "8 virgin females")

no_fem <- fruitflies %>% 
  filter(treatment %in% "no females added")

plot1 <- ggplot(aes(x = thorax.mm, y = longevity.days), data = one_preg_fem) + geom_point()
plot2 <- ggplot(aes(x = thorax.mm, y = longevity.days), data = one_virg_fem) + geom_point()
plot3 <- ggplot(aes(x = thorax.mm, y = longevity.days), data = eight_preg_fem) + geom_point()
plot4 <- ggplot(aes(x = thorax.mm, y = longevity.days), data = eight_virg_fem) + geom_point()
plot5 <- ggplot(aes(x = thorax.mm, y = longevity.days), data = no_fem) + geom_point()

multiplot(plot1, plot2, plot3, plot4, plot5, cols=3)
