
# Load data ---------------------------------------------------------------

mydata <- read.csv(file = "./raw_data/anolis.csv", header = TRUE)


# data manipulation -------------------------------------------------------

str(mydata)
class(mydata$Island)
class(mydata)
head(mydata)
levels(mydata$Ecomorph)
which(mydata$Ecomorph == "Trunk-Crown ")
mydata$Ecomorph[which(mydata$Ecomorph == "Trunk-Crown ")] <- "Trunk-Crown"
table(mydata$Ecomorph)
levels(mydata$Ecomorph)
mydata$Ecomorph <- droplevels(mydata$Ecomorph)
levels(mydata$Ecomorph)


# Load data in properly ---------------------------------------------------

mydata <- read.csv(file = "./raw_data/anolis.csv", stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c("NA", ""))


# data manipulation with base r -------------------------------------------

table(mydata$Ecomorph)
table(mydata$Ecomorph, useNA = "ifany")
head(mydata)
table(mydata$Island)
sapply(mydata, class)

length(grep("Jamaica", mydata$Island))
length(grep("Cuba", mydata$Island))


# using dplyr -------------------------------------------------------------

## tally of species belonging to each ecomorph on the four largest Caribbean island

library(dplyr)

unique(mydata$Island)


## make a new df that only contains the islands we want
mydata_top4 <- mydata %>% 
  filter(Island %in% c("Cuba", "Jamaica", "Hispaniola", "Puerto Rico"))

## checking for the unique levels of Island
unique(mydata_top4$Island)

results_table <- mydata_top4 %>% # take the reduced dataset
  filter(!is.na(Ecomorph)) %>% # use filter to retain all rows where Ecomorph is not NA
  group_by(Island, Ecomorph) %>% # group by Island and Ecomorph, aka "for each combination of island and ecomorph"
  summarise(number_species = n()) %>% # use the n function to count the number of distinct rows in each group (using whatever group you grouped by)
  summarise(number_of_species = mean(number_species)) #added line of code; use the summarise verb to create a summary statistic for each group in your df 

## What is the most frequent ecomorph for species that do not occur on the four largest islands?
mydata %>% 
  filter(!grepl('Cuba|Hispaniola|Puerto Rico|Jamaica', Island)) %>% 
  filter(!is.na(Ecomorph)) %>% 
  group_by(Ecomorph) %>% 
  summarise(num_eco = n()) %>% View

  

