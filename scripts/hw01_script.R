
# load in data and packages ------------------------------------------------------------

feeding <- read.csv(file = "./data/hw01_data_2.csv")
suppressMessages(library(tidyverse))

# Look at the data... ------------------------------------------------------------

# load packages
suppressMessages(library(tidyverse))

# examine data
str(feeding)

# rename variables
feeding <- feeding %>% 
  rename(prey_size = Prey_size, 
         prof = Profitability_g_per_min,
         feed_time = feeding_time_1_hr_d, 
         hand_time = Handling_time_min,
         seastar = Seastar) 


# New Plots ---------------------------------------------------------------

# Box plot of handling time vs mussel size 
ggplot(feeding, aes(x = as.factor(prey_size), y = hand_time)) + 
  geom_boxplot(outlier.colour = "green") +
  geom_jitter(position = position_jitter(width = 0, height = 0), alpha = 1/2) + 
  ylab("handling time (min/mussel)") +
  xlab("mussel size class") +
  scale_x_discrete(labels = c("10" = "10 mm", "15" = "15 mm",
                              "20" = "20 mm", "25" = "25 mm")) +
  theme_bw() +
  expand_limits(y = c(0, 500))

# Box plot of profitability vs mussel size
ggplot(feeding, aes(x = as.factor(prey_size), y = prof)) + 
  geom_boxplot(outlier.colour = "green") + 
  geom_jitter(position = position_jitter(width = 0, height = 0), alpha = 1/2) + 
  ylab("profitability (g tissue/hr)") + 
  xlab("mussel size class") + 
  scale_x_discrete(labels = c("10" = "10 mm", "15" = "15 mm",
                              "20" = "20 mm", "25" = "25 mm")) +
  theme_bw()
