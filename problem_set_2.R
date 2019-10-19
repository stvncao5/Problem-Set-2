library(tidyverse)
library(skimr)
library(cluster) # for daisy(), used to generate a dissimilarity matrix

# Part 1, Problem 1

# Generate vectors
p <- c(1,2)
q <- c(3,4)

# Calculate Manhattan distance between the p point-pair and the q point-pair
vector_absoluteDifferences = abs(p-q)
manhattan_distance = sum(vector_absoluteDifferences)

# Calculate Canberra distance between the p point-pair and the q point-pair
vector_absoluteDifferences = abs(p-q) # i.e. the numerator
vector_totalFeatureMagnitudes = abs(p) + abs(q) # i.e. the denominator
canberra_distance = sum(vector_absoluteDifferences) / sum(vector_totalFeatureMagnitudes)

# Calculate Euclidean distance between the p point-pair and the q point-pair
vector_differences = p-q # get "a" and "b", as per "a^2 + b^2 = c^2"
c_squared = vector_differences^2 # i.e. the vector is {a^2, b^2}
c_squared = sum(c_squared) # compress from vector to its equivalent scalar
euclidean_distance = sqrt(c_squared) # get c, which is the euclidean distance



# Part 1, Problem 2

# Combine the point-pair vectors into a matrix
matrix_points <- cbind(p,q)

# Check distances
check_manhattan_distance = dist(matrix_points, method = "manhattan")
check_canberra_distance = dist(matrix_points, method = "canberra")
check_euclidean_distance = dist(matrix_points, method = "euclidean")



# Part 1, Problem 3

# Do in Rmd



# Part 1, Problem 4

# Load the Old Faithful Geyser data
# For reference, it measures 2 variables (eruption times and idle times), both containing 272 observations
data_geyser_OldFaithful <- faithful %>%
  dplyr::select(eruptions, waiting) # technically select() not necessary; mutate() also not necessary

# Quick statistical summary
skim(data_geyser_OldFaithful)

# Some quick exploration of the data
ggplot(data_geyser_OldFaithful) + 
  geom_point( aes( x=waiting, y=eruptions) ) + 
  theme_bw()

ggplot(data_geyser_OldFaithful) + 
  geom_histogram( aes( x=waiting ) ) + 
  theme_bw()

ggplot(data_geyser_OldFaithful) + 
  geom_histogram( aes( x=eruptions ) ) +
  theme_bw()



# Part 1, Problem 5

# We are assuming that "dissimilarity matrix" and "distance matrix" means the same thing.
# A distance matrix takes a point in some n-dimensional feature space, and then finds the distance
# between that point and all other points. So naturally enough, the output is an n-by-n matrix.

# Standardise (i.e. scale) the data
dataStandardised_geyser_OldFaithful = scale(data_geyser_OldFaithful)

# Get all possible pairwise distances in the form of a distance matrix
distanceMatrix_geyser_OldFaithful = dist(dataStandardised_geyser_OldFaithful, method = "euclidean")

# Quicker way of doing it
# distanceMatrix_geyser_OldFaithful = daisy(data_geyser_OldFaithful, metric = "euclidean", stand = TRUE)



# Part 1, Problem 6

library(seriation) # Required for dissplot()

# Generate Ordered Dissimilarity Image (aka Visual Assessment of Tendency)
dissplot(distanceMatrix_geyser_OldFaithful)



# Part 1, Problem 7

# Load the Iris data, scrub the dataset of qualitative variables,
# then perform steps needed to yield a distance matrix.
# For reference, it measures 4+1 variables, all containing 150 observations
distanceMatrix_flowers_Iris <- data(iris) %>%
  dplyr::select(-Species) %>% # exclude species
  scale() %>% # standardise variables
  dist(method = "euclidean") # and finally calculate the distance matrix



# Part 1, Problem 8

# Apply agglomerative hierarchical clustering to the data, using complete (maximum distance) linkage
# Outputs a 'tree' object (plot() outputs a dendogram when it takes a 'tree' object)
hc_complete_flowers_Iris = hclust(distanceMatrix_flowers_Iris, method = "complete")
# Plot a dendogram of the clustering
plot(hc_complete_flowers_Iris, hang = -1)



# Part 1, Problem 9

library(dendextend) # Required for cutree()

# Retrieve the clustering results at k=2 and k=3
prunedTree_flowers_Iris = cutree(hc_complete_flowers_Iris, k=c(2,3))

# Plot them side-by-side
par(mfrow=c(2,2))
plot( prunedTree_flowers_Iris[,1] )
plot( prunedTree_flowers_Iris[,2] )
par(mfrow=c(1,1))



# Part 1, Problem 10

# Do what we did in Part 1 Problem 8, but using and plotting both single (minimum distance) and
# complete (maximum distance) linkage this time.
hc_complete_flowers_Iris = hclust(distanceMatrix_flowers_Iris, method = "complete")
hc_single_flowers_Iris = hclust(distanceMatrix_flowers_Iris, method = "single")
# Plot them side-by-side
par(mfrow=c(2,2))
plot( hc_complete_flowers_Iris, hang = -1 )
plot( hc_single_flowers_Iris, hang = -1 )
par(mfrow=c(1,1))



# Part 2, Problem 1

# Do in Rmd



# Part 2, Problem 2

# Do in Rmd
