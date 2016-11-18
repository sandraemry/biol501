library(tidyverse)
arsenic <- read_csv("./data/arsenic.csv")
str(arsenic)

# Graph the data. Explain your graph. What is the pattern in the data?
ggplot(data = arsenic, aes(x = line, y = cube.root.height)) + geom_boxplot()

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

# Fit the numeric variable you created in (3) to the height data using a linear model. 
# This is called the additive model, whereby tolerance increases linearly with the 
# proportion of the genome inherited from the high tolerance parent. 
# Evaluate the model fit (Remember: no P values!).

hist(arsenic$cube.root.height)
mod1 <- lm(cube.root.height ~ prop, data = arsenic)
plot(mod1)
summary(mod1)

# Add another numeric variable to the data set to represent 
# dominance effects that might be present in the hybrids:
# 0 for both parent genotypes
# 1 for the F1 hybrid
# 0.5 for the remaining three hybrid genotypes
# Make sure that the variable is numeric rather than a factor or character.  

arsenic$dominance <- ifelse(arsenic$line == "high", 0,
                            ifelse(arsenic$line == "low", 0,
                                   ifelse(arsenic$line == "f1", 1,
                                          ifelse(arsenic$line == "f2", 1,
                                                 ifelse(arsenic$line == "bl", 0.5,
                                                        ifelse(arsenic$line == "bh", 0.5,
                                                               NA))))))


# Fit a second model to the same data that includes both of the numeric variables created in 
# (3) and (5). 
# Leave out any interaction terms. 
# This is the additive plus dominance model. 
# Any dominance effects present will displace the mean value of the hybrids toward 
# one or other of the parents relative to the values predicted by the additive model. 
# Evaluate model fit.

mod2 <- lm(cube.root.height ~ prop + dominance, data = arsenic)
plot(mod2)
summary(mod2)

# Finally, fit a third model that has the original genotype variable as the only explanatory variable.
# The fit of this model will deviate from the model fitted in (6) if there is interaction (epistasis)
# between genes inherited from the two parents.

mod3 <- lm(cube.root.height ~ line, data = arsenic)
plot(mod3)
summary(mod3)

# Present your results, comparing model fits. 
# Which genetic model best fit the data? 
# Explain and summarize.
