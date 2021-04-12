library(cluster)


college_clean=read.csv("insurance cluter2.csv",header = T)
college_clean$bodily_injuries<-as.factor(college_clean$bodily_injuries)
college_clean$witnesses<-as.factor(college_clean$witnesses)

dist <- daisy(college_clean, metric = "gower")

cls <- hclust(dist)

plot(cls)

str(college_clean)


# Data
data("mtcars")
mtcars$cyl <- as.factor(mtcars$cyl)

# Hierarchical clustering with Euclidean distance - works 
clusters <- hclust(dist(mtcars[, 1:2]))
plot(clusters)

# Hierarchical clustering with Gower distance - doesn't work
library(philentropy)
clusters <- hclust(distance(mtcars[, 1:2], method = "gower"))
plot(clusters)


