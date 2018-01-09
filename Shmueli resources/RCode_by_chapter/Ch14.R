#### Table 14.4
library(arules)
fp.df <- read.csv("Faceplate.csv")

# remove first column and convert to matrix
fp.mat <- as.matrix(fp.df[, -1])

# convert the binary incidence matrix into a transactions database
fp.trans <- as(fp.mat, "transactions")
inspect(fp.trans)

## get rules
# when running apriori(), include the minimum support, minimum confidence, and target
# as arguments. 
rules <- apriori(fp.trans, parameter = list(supp = 0.2, conf = 0.5, target = "rules"))

# inspect the first six rules, sorted by their lift
inspect(head(sort(rules, by = "lift"), n = 6))



#### Table 14.6

rules.tbl <- inspect(rules)
rules.tbl[rules.tbl$support >= 0.04 & rules.tbl$confidence >= 0.7,]



#### Table 14.8

all.books.df <- read.csv("CharlesBookClub.csv")

# create a binary incidence matrix
count.books.df <- all.books.df[, 8:18]
incid.books.df <- ifelse(count.books.df > 0, 1, 0)
incid.books.mat <- as.matrix(incid.books.df[, -1])

#  convert the binary incidence matrix into a transactions database
books.trans <- as(incid.books.mat, "transactions")
inspect(books.trans)

# plot data
itemFrequencyPlot(books.trans)

# run apriori function
rules <- apriori(books.trans, 
                 parameter = list(supp= 200/4000, conf = 0.5, target = "rules"))

# inspect rules
inspect(sort(rules, by = "lift"))



#### Table 14.11
library(recommenderlab)

# simulate matrix with 1000 users and 100 movies
m <- matrix(nrow = 1000, ncol = 100)
# simulated ratings (1% of the data)
m[sample.int(100*1000, 1000)] <- ceiling(runif(1000, 0, 5))
## convert into a realRatingMatrix
r <- as(m, "realRatingMatrix")

# user-based collaborative filtering
UB.Rec <- Recommender(r, "UBCF")
pred <- predict(UB.Rec, r, type="ratings")
as(pred, "matrix")


# item-based collaborative filtering
IB.Rec <- Recommender(r, "IBCF")
pred <- predict(IB.Rec, r, type="ratings")
as(pred, "matrix")
