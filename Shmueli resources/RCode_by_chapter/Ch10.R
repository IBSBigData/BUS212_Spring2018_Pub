#### Table 10.2

bank.df <- read.csv("UniversalBank.csv")
bank.df <- bank.df[ , -c(1, 5)]  # Drop ID and zip code columns.
# treat Education as categorical (R will create dummy variables)
bank.df$Education <- factor(bank.df$Education, levels = c(1, 2, 3), 
                            labels = c("Undergrad", "Graduate", "Advanced/Professional"))

# partition data
set.seed(2)
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)  
train.df <- bank.df[train.index, ]
valid.df <- bank.df[-train.index, ]

# run logistic regression
# use glm() (general linear model) with family = "binomial" to fit a logistic 
# regression.
logit.reg <- glm(Personal.Loan ~ ., data = train.df, family = "binomial") 
options(scipen=999)
summary(logit.reg)


#### Table 10.3

# use predict() with type = "response" to compute predicted probabilities. 
logit.reg.pred <- predict(logit.reg, valid.df[, -8], type = "response")

# first 5 actual and predicted records
data.frame(actual = valid.df$Personal.Loan[1:5], predicted = logit.reg.pred[1:5])



#### Figure 10.3

library(gains)
gain <- gains(valid.df$Personal.Loan, logit.reg.pred, groups=10)

# plot lift chart
plot(c(0,gain$cume.pct.of.total*sum(valid.df$Personal.Loan))~c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(valid.df$Personal.Loan))~c(0, dim(valid.df)[1]), lty=2)

# compute deciles and plot decile-wise chart
heights <- gain$mean.resp/mean(valid.df$Personal.Loan)
midpoints <- barplot(heights, names.arg = gain$depth, ylim = c(0,9), 
                     xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")

# add labels to columns
text(midpoints, heights+0.5, labels=round(heights, 1), cex = 0.8)



#### Figure 10.4

# code for generating top-right bar chart
# for other plots, replace aggregating variable by setting argument by =  in 
# aggregate(). 
# in function barplot(), set the x-label (argument xlab =) and y-label 
# (argument names.arg =) 
# according to the variable of choice.  

barplot(aggregate(delays.df$Flight.Status == "delayed", by = list(delays.df$DAY_WEEK), 
                  mean, rm.na = T)[,2], xlab = "Day of Week", ylab = "Average Delay", 
        names.arg = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))



#### Figure 10.5

library(reshape)
library(ggplot2)
# create matrix for plot
delays.df$isDelay <- 1 * (delays.df$Flight.Status == "delayed")

agg <- aggregate(delays.df$isDelay, 
                 by = list(delays.df$DAY_WEEK, delays.df$CARRIER, delays.df$ORIGIN), 
                 FUN = mean, na.rm = TRUE)
m <- melt(agg)
names(m)[1:3] <- c("DAY_WEEK", "CARRIER", "ORIGIN")

# plot with ggplot
# use facet_grid() with arguments scales = "free" and space = "free" to skip 
# missing values.
ggplot(m, aes(y = CARRIER, x = DAY_WEEK, fill = value)) +  geom_tile() + 
  facet_grid(ORIGIN ~ ., scales = "free", space = "free") + 
  scale_fill_gradient(low="white", high="black")



#### Table 10.6

delays.df <- read.csv("FlightDelays.csv")

# transform variables and create bins
delays.df$DAY_WEEK <- factor(delays.df$DAY_WEEK, levels = c(1:7), 
                             labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
delays.df$CRS_DEP_TIME <- factor(round(delays.df$CRS_DEP_TIME/100))

# create reference categories
delays.df$ORIGIN <- relevel(delays.df$ORIGIN, ref = "IAD")
delays.df$DEST <- relevel(delays.df$DEST, ref = "LGA")
delays.df$CARRIER <- relevel(delays.df$CARRIER, ref = "US")
delays.df$DAY_WEEK <- relevel(delays.df$DAY_WEEK, ref = "Wed")
delays.df$isDelay <- 1 * (delays.df$Flight.Status == "delayed")

# create training and validation sets
selected.var <- c(10, 1, 8, 4, 2, 9, 14)
train.index <- sample(c(1:dim(delays.df)[1]), dim(delays.df)[1]*0.6)  
train.df <- delays.df[train.index, selected.var]
valid.df <- delays.df[-train.index, selected.var]

# run logistic model, and show coefficients and odds
lm.fit <- glm(isDelay ~ ., data = train.df, family = "binomial")
data.frame(summary(lm.fit)$coefficients, odds = exp(coef(lm.fit))) 

round(data.frame(summary(lm.fit)$coefficients, odds = exp(coef(lm.fit))), 5)



#### Figure 10.6

library(gains)
pred <- predict(lm.fit, valid.df)
gain <- gains(valid.df$isDelay, pred, groups=100)

plot(c(0,gain$cume.pct.of.total*sum(valid.df$isDelay))~
       c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(valid.df$isDelay))~c(0, dim(valid.df)[1]), lty=2)



#### Table 10.8

# fewer predictors
delays.df$Weekend <- delays.df$DAY_WEEK %in% c("Sun", "Sat")
delays.df$CARRIER_CO_MQ_DH_RU <- delays.df$CARRIER %in% c("CO", "MQ", "DH", "RU")
delays.df$MORNING <- delays.df$CRS_DEP_TIME %in% c(6, 7, 8, 9)
delays.df$NOON <- delays.df$CRS_DEP_TIME %in% c(10, 11, 12, 13)
delays.df$AFTER2P <- delays.df$CRS_DEP_TIME %in% c(14, 15, 16, 17, 18)
delays.df$EVENING <- delays.df$CRS_DEP_TIME %in% c(19, 20)


set.seed(1)  # Set the seed for the random number generator for reproducing the 
# partition.
train.index <- sample(c(1:dim(delays.df)[1]), dim(delays.df)[1]*0.6)  
valid.index <- setdiff(c(1:dim(delays.df)[1]), train.index)  
train.df <- delays.df[train.index, ]
valid.df <- delays.df[valid.index, ]

lm.fit <- glm(isDelay ~ Weekend + Weather + CARRIER_CO_MQ_DH_RU + MORNING  +  NOON + 
                AFTER2P + EVENING, data = train.df, family = "binomial")
summary(lm.fit)

# evaluate
pred <- predict(lm.fit, valid.df)
confusionMatrix(ifelse(pred > 0.5, 1, 0), valid.df$isDelay)



#### Table 10.9

reg <- lm(Personal.Loan ~ Income + Family + CD.Account, data = bank.df)
summary(reg)



#### Table 10.10 

summary(logit.reg)



#### Figure 10.9

## note: run after Table 10.3, before adding more varuables
confusionMatrix(ifelse(logit.reg$fitted > 0.5, 1, 0), train.df[,8])



#### Table 10.11

# simulate simple data
Y = rep(c("a", "b", "c"), 100)
x = rep(c(1, 2, 3), 100) +  rnorm(300, 0, 1)

# ordinal logistic regression
library(MASS)
Y = factor(Y, ordered = T)
polr(Y ~ x)

# nominal logistic regression
library(nnet)
Y = factor(Y, ordered = F)
multinom(Y ~ x)

