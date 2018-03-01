#### Table 8.4

library(e1071)  # one package for Naive Bayes
# install.packages("naivebayes")  # if not already there
library(naivebayes)  # used in DataCamp
library(dplyr)

setwd("C:/Users/Rob/Box Sync/My R Work/BUS212")

delays.df <- read.csv("Data/FlightDelays.csv")

# change numerical variables to categorical first
delays.df$DAY_WEEK <- factor(delays.df$DAY_WEEK)
delays.df$DEP_TIME <- factor(delays.df$DEP_TIME)
# create hourly bins departure time 
delays.df$CRS_DEP_TIME <- factor(round(delays.df$CRS_DEP_TIME/100))

# Create training and validation sets.
set.seed(721)
selected.var <- c(10, 1, 8, 4, 2, 13)
train.index <- sample(c(1:dim(delays.df)[1]), dim(delays.df)[1]*0.6)  
train.df <- delays.df[train.index, selected.var]
valid.df <- delays.df[-train.index, selected.var]

# run naive bayes
delays.nb <- naiveBayes(Flight.Status ~ ., data = train.df)
delays.nb

#######################
# do the same using DataCamp package 
#### Table 8.5
delays.nb2 <- naive_bayes(Flight.Status ~ ., data = train.df)
delays.nb2
#  results formatted differently, but same

## Now try with LaPlace correction
delays.nb3 <- naiveBayes(Flight.Status ~ ., data = train.df, laplace = 1)
delays.nb3

# use prop.table() with margin = 1 to convert a count table to a proportion table, 
# where each row sums up to 1 (use margin = 2 for column sums).
prop.table(table(train.df$Flight.Status, train.df$DEST), margin = 1)



#### Table 8.6

## predict probabilities
pred.probv <- predict(delays.nb, newdata = valid.df, type = "raw")
## predict class membership
pred.classv <- predict(delays.nb, newdata = valid.df)

### repeat using naivebayes with laplace, as in datacamp
pred.prob3 <- predict(delays.nb3, newdata = valid.df, type ="raw")
pred.class3 <- predict(delays.nb3, newdata = valid.df)

df <- data.frame(actual = valid.df$Flight.Status, predicted = pred.classv, pred.probv)

df[valid.df$CARRIER == "DL" & valid.df$DAY_WEEK == 7 & valid.df$CRS_DEP_TIME == 10 & 
     valid.df$DEST == "LGA" & valid.df$ORIGIN == "DCA",]




#### Table 8.7

library(caret)

# training
pred.classt <- predict(delays.nb, newdata = train.df)
confusionMatrix(pred.classt, train.df$Flight.Status)

# with laplace
pred.classtl <- predict(delays.nb3, newdata = train.df)
confusionMatrix(pred.classtl, train.df$Flight.Status)

# validation
pred.classv <- predict(delays.nb, newdata = valid.df)
confusionMatrix(pred.classv, valid.df$Flight.Status)


# new frame to compare predicted propensities and actual
compare <- as.data.frame(pred.probv)
compare <- cbind(compare, valid.df$Flight.Status, pred.classv)
compare <- rename(compare, "Actual" = "valid.df$Flight.Status","Predicted"="pred.classv" )
# resort by probability, descending
attach (compare)
hiprop <- compare[order(-delayed),]
lowprop <- compare[order(delayed),]
detach(compare)
head(compare,12)
head(hiprop,12)
head(lowprop,12)




#### Figure 8.1

library(gains)
gain <- gains(ifelse(valid.df$Flight.Status=="delayed",1,0), pred.prob[,1], groups=100)

plot(c(0,gain$cume.pct.of.total*sum(valid.df$Flight.Status=="delayed"))~c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(valid.df$Flight.Status=="delayed"))~c(0, dim(valid.df)[1]), lty=2)









