#### Table 4.3

boston.housing.df <- read.csv("BostonHousing.csv", header = TRUE) 
head(boston.housing.df, 9)
summary(boston.housing.df) 

# compute mean, standard dev., min, max, median, length, and missing values of CRIM
mean(boston.housing.df$CRIM) 
sd(boston.housing.df$CRIM)
min(boston.housing.df$CRIM)
max(boston.housing.df$CRIM)
median(boston.housing.df$CRIM) 
length(boston.housing.df$CRIM) 

# find the number of missing values of variable CRIM
sum(is.na(boston.housing.df$CRIM)) 

# compute mean, standard dev., min, max, median, length, and missing values for all
# variables
data.frame(mean=sapply(boston.housing.df, mean), 
                         sd=sapply(boston.housing.df, sd), 
                         min=sapply(boston.housing.df, min), 
                         max=sapply(boston.housing.df, max), 
                         median=sapply(boston.housing.df, median), 
                         length=sapply(boston.housing.df, length),
                         miss.val=sapply(boston.housing.df, function(x) 
                         sum(length(which(is.na(x))))))



#### Table 4.4

round(cor(boston.housing.df),2)



#### Table 4.5

table(boston.housing.df$CHAS)



#### Table 4.6

# create bins of size 1
boston.housing.df$RM.bin <- .bincode(boston.housing.df$RM, c(1:9))

# compute the average of MEDV by (binned) RM and CHAS
# in aggregate() use the argument by= to define the list of aggregating variables, 
# and FUN= as an aggregating function.
aggregate(boston.housing.df$MEDV, by=list(RM=boston.housing.df$RM.bin, 
                                          CHAS=boston.housing.df$CHAS), FUN=mean) 



#### Table 4.7

# use install.packages("reshape") the first time the package is used
library(reshape) 
boston.housing.df <- read.csv("BostonHousing.csv")
# create bins of size 1
boston.housing.df$RM.bin <- .bincode(boston.housing.df$RM, c(1:9)) 

# use melt() to stack a set of columns into a single column of data.
# stack MEDV values for each combination of (binned) RM and CHAS
mlt <- melt(boston.housing.df, id=c("RM.bin", "CHAS"), measure=c("MEDV"))
head(mlt, 5)

# use cast() to reshape data and generate pivot table
cast(mlt, RM.bin ~ CHAS, subset=variable=="MEDV", 
     margins=c("grand_row", "grand_col"), mean)




#### Figure 4.1

library(ggmap)

tbl <- table(boston.housing.df$CAT..MEDV, boston.housing.df$ZN)
prop.tbl <- prop.table(tbl, margin=2)
barplot(prop.tbl, xlab="ZN", ylab="", yaxt="n",main="Distribution of CAT.MEDV by ZN")
axis(2, at=(seq(0,1, 0.2)), paste(seq(0,100,20), "%"))



#### Table 4.10

cereals.df <- read.csv("Cereals.csv") 
# compute PCs on two dimensions
pcs <- prcomp(data.frame(cereals.df$calories, cereals.df$rating)) 
summary(pcs) 
pcs$rot
scores <- pcs$x
head(scores, 5)



#### Table 4.11

pcs <- prcomp(na.omit(cereals.df[,-c(1:3)])) 
summary(pcs)




#### Table 4.12

pcs.cor <- prcomp(na.omit(cereals.df[,-c(1:3)]), scale. = T)
summary(pcs.cor)









