#### Table 11.2

library(neuralnet)
df <- read.csv("example1.csv")

df$Like <- df$Acceptance=="like"
df$Dislike <- df$Acceptance=="dislike"

set.seed(1)
nn <- neuralnet(Like + Dislike ~ Salt + Fat, data = df, linear.output = F, hidden = 3)

# display weights
nn$weights

# display predictions
prediction(nn)

# plot network
plot(nn, rep="best")



#### Table 11.3

library(caret)
predict <- compute(nn, data.frame(df$Salt, df$Fat))
predicted.class=apply(predict$net.result,1,which.max)-1
confusionMatrix(ifelse(predicted.class=="1", "dislike", "like"), df$Acceptance)



#### Table 11.6, 11.7
library(neuralnet,nnet,caret)

accidents.df <- read.csv("Accidents.csv")
# selected variables
vars <- c("ALCHL_I", "PROFIL_I_R", "VEH_INVL")

# partition the data
set.seed(2)
training=sample(row.names(accidents.df), dim(accidents.df)[1]*0.6)
validation=setdiff(row.names(accidents.df), training)

# when y has multiple classes - need to dummify
trainData <- cbind(accidents.df[training,c(vars)], 
                   class.ind(accidents.df[training,]$SUR_COND),
                   class.ind(accidents.df[training,]$MAX_SEV_IR))
names(trainData) <- c(vars, 
                   paste("SUR_COND_", c(1, 2, 3, 4, 9), sep=""), paste("MAX_SEV_IR_", c(0, 1, 2), sep=""))

validData <- cbind(accidents.df[validation,c(vars)], 
                   class.ind(accidents.df[validation,]$SUR_COND),
                   class.ind(accidents.df[validation,]$MAX_SEV_IR))
names(validData) <- c(vars, 
                   paste("SUR_COND_", c(1, 2, 3, 4, 9), sep=""), paste("MAX_SEV_IR_", c(0, 1, 2), sep=""))

# run nn with 2 hidden nodes 
# use hidden= with a vector of integers specifying number of hidden nodes in each layer
nn <- neuralnet(MAX_SEV_IR_0 + MAX_SEV_IR_1 + MAX_SEV_IR_2 ~ 
                  ALCHL_I + PROFIL_I_R + VEH_INVL + SUR_COND_1 + SUR_COND_2 
                + SUR_COND_3 + SUR_COND_4, data = trainData, hidden = 2)

training.prediction <- compute(nn, trainData[,-c(8:11)])
training.class <- apply(training.prediction$net.result,1,which.max)-1
confusionMatrix(training.class, accidents.df[training,]$MAX_SEV_IR)

validation.prediction <- compute(nn, validData[,-c(8:11)])
validation.class <-apply(validation.prediction$net.result,1,which.max)-1
confusionMatrix(validation.class, accidents.df[validation,]$MAX_SEV_IR)
