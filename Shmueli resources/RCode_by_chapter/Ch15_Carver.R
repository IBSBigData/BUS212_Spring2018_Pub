#### Table 15.2

utilities.df <- read.csv("Data/Utilities.csv")

# set row names to the utilities column
row.names(utilities.df) <- utilities.df[,1]

# remove the utility column
utilities.df <- utilities.df[,-1]

# compute Euclidean distance
# (to compute other distance measures, change the value in method = )
d <- dist(utilities.df, method = "euclidean")



#### Table 15.4

# normalize input variables
utilities.df.norm <- sapply(utilities.df, scale)

# add row names: utilities
row.names(utilities.df.norm) <- row.names(utilities.df) 

# compute normalized distance based on Sales (column 6) and Fuel Cost (column 8)
d.norm <- dist(utilities.df.norm[,c(6,8)], method = "euclidean")

d.norm

#### Figure 15.3

# in hclust() set argument method =  
# to "ward.D", "single", "complete", "average", "median", or "centroid"
hc1 <- hclust(d.norm, method = "single")
plot(hc1, hang = -1, ann = FALSE)
hc2 <- hclust(d.norm, method = "average")
plot(hc2, hang = -1, ann = FALSE)

#### Table 15.6

memb <- cutree(hc1, k = 6)
memb
memb <- cutree(hc2, k = 6)
memb




#### Figure 15.4

# set labels as cluster membership and utility name
row.names(utilities.df.norm) <- paste(memb, ": ", row.names(utilities.df), sep = "")

# plot heatmap 
# rev() reverses the color mapping to large = dark
heatmap(as.matrix(utilities.df.norm), Colv = NA, hclustfun = hclust, 
        col=rev(paste("gray",1:99,sep="")))




#### Table 15.9  ## kmeans clustering

# load and preprocess data 
utilities.df <- read.csv("Data/Utilities.csv")
row.names(utilities.df) <- utilities.df[,1]
utilities.df <- utilities.df[,-1]

# normalized distance:
utilities.df.norm <- sapply(utilities.df, scale)
row.names(utilities.df.norm) <- row.names(utilities.df) 

# run kmeans algorithm 
km <- kmeans(utilities.df.norm, 6)

# show cluster membership
km$cluster



#### Table 15.10
# centroids
km$centers



#### Figure 15.5

# plot an empty scatter plot
plot(c(0), xaxt = 'n', ylab = "", type = "l", 
     ylim = c(min(km$centers), max(km$centers)), xlim = c(0, 8))

# label x-axes
axis(1, at = c(1:8), labels = names(utilities.df))

# plot centroids
for (i in c(1:6))
  lines(km$centers[i,], lty = i, lwd = 2, col = ifelse(i %in% c(1, 3, 5),
     "blue", "dark green"))

# name clusters
text(x = 0.5, y = km$centers[, 1], labels = paste("Cluster", c(1:6)))

#### Table 15.11

dist(km$centers)

######################
#  code (not in text) to help select k
# similar to Fig 15.6
# code adapted from https://uc-r.github.io/kmeans_clustering#elbow

library(tidyverse)
library(cluster)    # clustering algorithms
install.packages("factoextra")
library(factoextra) # clustering algorithms & visualization

set.seed(123)

df <- utilities.df.norm
# function to compute total within-cluster sum of square 
wss <- function(k) {
     kmeans(df, k, nstart = 10)$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

# package factoextra offers function fviz_nbclust to make the graph
set.seed(123)
fviz_nbclust(df, kmeans, method = "wss")
