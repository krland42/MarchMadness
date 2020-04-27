training <- sample(1:nrow(regular_season_stats), 0.8 * nrow(regular_season_stats), replace=FALSE)
testing <- setdiff(1:nrow(regular_season_stats), training)

train <- subset(regular_season_stats[training, ], select = c(Score, OR, DR, Ast, TO, Stl, Blk))
train$FGP3 <- subset(regular_season_stats[training, ], select = FGM3) / subset(regular_season_stats[training, ], select = FGA3)
train$FGP <- subset(regular_season_stats[training, ], select = FGM) / subset(regular_season_stats[training, ], select = FGA)
train$FTP <- subset(regular_season_stats[training, ], select = FTM) / subset(abc[training, ], select = FTA)

test <- subset(regular_season_stats[testing, ], select = c(Score, OR, DR, Ast, TO, Stl, Blk))
test$FGP3 <- subset(regular_season_stats[testing, ], select = FGM3) / subset(regular_season_stats[testing, ], select = FGA3)
test$FGP <- subset(regular_season_stats[testing, ], select = FGM) / subset(regular_season_stats[testing, ], select = FGA)
test$FTP <- subset(regular_season_stats[testing, ], select = FTM) / subset(abc[testing, ], select = FTA)

outcome <- regular_season_stats$Outcome[training]
true.labels <- regular_season_stats$Outcome[testing]
misclassification.rate <- array()

for (k in 1:50){
  prediction <- knn(train, test, outcome, k)
  num.incorrect.labels <- sum(prediction != true.labels)
  misclassification.rate[k] <- num.incorrect.labels / nrow(test)
}

min(misclassification.rate) # Output was 0.2223873
which(misclassification.rate == min(misclassification.rate)) # With a K of 5