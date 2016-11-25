library(tidyverse)
anolis <- read.csv("./data/anolis.convergence.csv", stringsAsFactors = TRUE )
str(anolis)

# Which variables are most strongly correlated with one another?
pairs(~ SVLength + FemurLength + TibiaLength + FootLength + ToeLength +  HumerusLength + 
        RadiusLength + FingerLength + FootLamellae + HandLamellae + TailLength, data = anolis)

anolis_num <- anolis %>% 
  select(SVLength, FemurLength, TibiaLength, FootLength, ToeLength, HumerusLength,
           RadiusLength, FingerLength, FootLamellae, HandLamellae, TailLength)
# Carry out a principal components analysis on the Anolis species mean measurements

# check that variances of each trait is on the same order of magnitude
v <- cov(anolis_num)
v <- var(anolis_num)

z <- prcomp(anolis[ ,4:ncol(anolis)])  

# Examine the proportion of variance explained by each principal component.
summary(z)                 # square root of eigenvalues, variance proportions

# What are eigenvalues? Create a scree plot to visualize the magnitudes of the eigenvalues
print(z)                   # square root of eigenvalues; eigenvectors too
plot(z, type= "lines")     # "scree" plot of eigenvalues
screeplot(z, type="lines") # same
z$sdev^2                   # eigenvalues (variances)

# Create a biplot to visualize the contribution of traits to the first two principal components. 
biplot(z, cex = 0.7)
# Which traits contribute most to PC1? Which vary most with PC2?
max(abs(z$rotation[ ,1])) == abs(z$rotation[ ,1])
max(abs(z$rotation[ ,2])) == abs(z$rotation[ ,2])

# Examine and interpret the eigenvectors for the first two principal components. 
z$rotation                 # eigenvectors (with trait loadings)
# Which variables contribute the most to the first two principal components? 
max(abs(z$rotation[ ,1]) + abs(z$rotation[ ,2])) == abs(z$rotation[ ,1]) + abs(z$rotation[ ,2])
# Can any of the principal components be interpreted as a general “size” variable? 
# Which one? How much of the variance does it account for?

# Compare the eigenvectors for the first two principal components to the biplot you generated for these same two components. Do you see a correspondence?
z$rotation[ ,1:2]       

# Save the scores for the first four principal components, the measurements of every individual along these principal axes, into the Anolis data frame.
pc_first_four <- predict(z)
anolis[ ,15:18] <- pc_first_four[ ,1:4]

colnames(anolis)[15:18] <- c("PC1", "PC2", "PC3", "PC4")
