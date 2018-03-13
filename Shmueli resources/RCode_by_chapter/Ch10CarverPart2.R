# Ch10CarverPart2.R
# Logistic for profiling; extension to multinomial
# repeat some code from Part 1, then the newer material 
# begins with Table 10.9 chunk.

library(caret)
library(gains)

## read and partition the bank data
bank.df <- read.csv("Data/UniversalBank.csv")
bank.df <- bank.df[ , -c(1, 5)]  # Drop ID and zip code columns.
# treat Education as categorical (R will create dummy variables)
bank.df$Education <- factor(bank.df$Education, levels = c(1, 2, 3),
     labels = c("Undergrad", "Graduate", "Advanced/Professional"))

# partition data
set.seed(2)
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)  
train.df <- bank.df[train.index, ]
valid.df <- bank.df[-train.index, ]
logit.reg <- glm(Personal.Loan ~ ., data = train.df, family = "binomial") 
options(scipen=999)  #  this suppresses scientific notation
summary(logit.reg)

# use predict() with type = "response" to compute predicted probabilities. 
logit.reg.pred <- predict(logit.reg, valid.df[, -8], type = "response")

#### Figure 10.3

gain <- gains(valid.df$Personal.Loan, logit.reg.pred, groups=10)

#####################################################
# Part 2 code starts here
#### Table 10.9

reg <- lm(Personal.Loan ~ Income + Family + CD.Account, data = bank.df)
summary(reg)


#### Table 10.10 
summary(logit.reg)



#### Figure 10.9
## note: run after Table 10.3, before adding more varuables
# NOTICE:  ifelse clause sets the cutoff at 0.5 to classify 
# a case as "1".  
confusionMatrix(ifelse(logit.reg$fitted > 0.5, 1, 0), train.df[,8])

#  check the effect of cutoff = 0.4 (as one example)
confusionMatrix(ifelse(logit.reg$fitted > 0.4, 1, 0), train.df[,8])

# plot lift chart
plot(c(0,gain$cume.pct.of.total*sum(valid.df$Personal.Loan))~c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(valid.df$Personal.Loan))~c(0, dim(valid.df)[1]), lty=2)

#### Table 10.11

# simulate simple data
Y = rep(c("a", "b", "c"), 100)
x = rep(c(1, 2, 3), 100) +  rnorm(300, 0, 1)

# ordinal logistic regression
library(MASS)  # Support Functions and Datasets for Venables and Ripley's MASS. Functions and datasets to support Venables and Ripley, "Modern Applied Statistics with S" (4th edition, 2002).

Y = factor(Y, ordered = T)
polr(Y ~ x)

# nominal logistic regression
library(nnet)
Y = factor(Y, ordered = F)
multinom(Y ~ x)

