# comp2.R  short script for part of the Intro to GitHub example
#  Created by Prof Carver
#  Modified by: 

# create some basic graphs using the diamonds dataframe that is included 
#     with package ggplot2
# you should explore this script, and then add a small amount of code as instructed as the end.
#
library(ggplot2)  # includes 'diamonds' dataframe. 
str(diamonds)  # examine the structure of the diamonds df
# our eventual goal is to model price as a function of other attributes.
# This script does some simple exploratory visualization, after dividing the 
# full data frame into a training and test partition.
#################
# Partition the data, as shown in Chapter 2
# use set.seed() to get the same partitions when re-running the R code.
set.seed(100)

## partitioning into training (60%) and validation (40%)
# randomly sample 60% of the row IDs for training; the remaining 40% serve as
# validation
train.rows <- as.numeric(sample(rownames(diamonds), dim(diamonds)[1]*0.6))
# collect all the columns with training row ID into training set:
train.data <- diamonds[train.rows, ]
# assign row IDs that are not already in the training set, into validation 
valid.rows <- as.numeric(setdiff(rownames(diamonds), train.rows))
valid.data <- diamonds[valid.rows, ]

## Now make plots based on the training and validation sets
## See if you can understand the entire plot command here; if not, do some
## research and/or ask me!

p1 <- ggplot(train.data, aes(carat, price, color =  color)) +
     geom_point(alpha = 0.10) +
     geom_smooth() +
     labs(title = "Price vs Carat, accounting for color",
          caption = "by Prof. Carver")
p1

p2 <- ggplot(valid.data, aes(carat, price, color =  color)) +
     geom_point(alpha = 0.10) +
     geom_smooth(se=FALSE) +
     labs(title = "Price vs Carat, accounting for color",
          caption = "by Prof. Carver") 
p2

# Here we might compare the results of the two subsets. For this assignment, though, the focus is on GitHub.

# After you run this script, go back and make the following changes, Then 
# re-save, commit the change, and then push the revised script back 
# to GitHub

#     1.  In line 3, add your name to "Modified by:  "
#     2.  In line 22, change the size of the training set to be 70%
#     3.  In both plots, change the color aesthetic to cut, rather than color
#     4.  In both plots, make an appropriate change to the title and caption
