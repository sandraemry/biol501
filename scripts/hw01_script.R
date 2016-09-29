
# load in data ------------------------------------------------------------

seastars <- read.csv(file = "./data/hw01_data.csv")
feeding <- read.csv(file = "./data/hw01_data_2.csv")
suppressMessages(library(tidyverse))

str(feeding)
as.factor(feeding$Prey_size)
levels(feeding$prey_size)

feeding <- feeding %>% 
  rename(prey_size = Prey_size, 
         prof = Profitability_g_per_min,
         feed_time = feeding_time_1_hr_d, 
         hand_time = Handling_time_min) %>% 
  as.factor(., prey_size)

feed_mod <- feeding %>% 
  select(prey_size, prof) %>% View

plot3 <- ggplot(feed_mod, aes(x = as.factor(prey_size), y = prof)) + 
  geom_point() + 
  ylab("profitability (g tissue/hr)") + 
  xlab("mussel size class") + 
  scale_x_discrete(labels=c("10" = "10 mm", "15" = "15 mm",
                              "20" = "20 mm", "25" = "25 mm")) +
  theme_bw()

ggplot(feed_mod, aes(x = as.factor(prey_size), y = prof)) + 
  geom_point() + 
  ylab("profitability (g tissue/hr)") + 
  xlab("mussel size class") + 
  scale_x_discrete(labels=c("10" = "10 mm", "15" = "15 mm",
                            "20" = "20 mm", "25" = "25 mm")) +
  theme_bw()





plot2 <- (feeding, aes(x = as.factor(prey_size), y = hand_time)) + 
  geom_point() + 
  ylab("handling time (min/mussel)") +
  xlab("mussel size class") + 
  scale_x_discrete(labels=c("10" = "10 mm", "15" = "15 mm",
                            "20" = "20 mm", "25" = "25 mm")) +
  theme_bw()

str(feeding)
feeding$hand_time




