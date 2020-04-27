library(dplyr)
library(class)
regular_season_deatiled <- read.csv("march-madness-analytics-2020/2020DataFiles/2020DataFiles/2020-Mens-Data/MDataFiles_Stage1/MRegularSeasonDetailedResults.csv")
regular_season_winning_stats <- subset(regular_season_deatiled, Season != 2019, select=c(4, 9:21))
regular_season_losing_stats <- subset(regular_season_deatiled, Season != 2019, select=c(6, 22:34))
regular_season_winning_stats <- rename(regular_season_winning_stats, c(Score=WScore, FGM=WFGM, FGA=WFGA, FGM3=WFGM3, FGA3=WFGA3, FTM=WFTM, FTA=WFTA, OR=WOR, DR=WDR, Ast=WAst, TO=WTO, Stl=WStl, Blk=WBlk, PF=WPF))
regular_season_losing_stats <- rename(regular_season_losing_stats, c(Score=LScore, FGM=LFGM, FGA=LFGA, FGM3=LFGM3, FGA3=LFGA3, FTM=LFTM, FTA=LFTA, OR=LOR, DR=LDR, Ast=LAst, TO=LTO, Stl=LStl, Blk=LBlk, PF=LPF))
regular_season_winning_stats$Outcome <- "W"
regular_season_losing_stats$Outcome <- "L"
regular_season_stats <- rbind(regular_season_winning_stats, regular_season_losing_stats)
write.csv(regular_season_stats, "march-madness-analytics-2020/CustomFiles/regular_season_stats.csv", row.names = TRUE)

tourney_deatiled <- read.csv("march-madness-analytics-2020/2020DataFiles/2020DataFiles/2020-Mens-Data/MDataFiles_Stage1/MNCAATourneyDetailedResults.csv")
tourney_winning_stats <- subset(tourney_deatiled, Season != 2019, select=c(4, 9:21))
tourney_losing_stats <- subset(tourney_deatiled, Season != 2019, select=c(6, 22:34))
tourney_winning_stats <- rename(tourney_winning_stats, c(Score=WScore, FGM=WFGM, FGA=WFGA, FGM3=WFGM3, FGA3=WFGA3, FTM=WFTM, FTA=WFTA, OR=WOR, DR=WDR, Ast=WAst, TO=WTO, Stl=WStl, Blk=WBlk, PF=WPF))
tourney_losing_stats <- rename(tourney_losing_stats, c(Score=LScore, FGM=LFGM, FGA=LFGA, FGM3=LFGM3, FGA3=LFGA3, FTM=LFTM, FTA=LFTA, OR=LOR, DR=LDR, Ast=LAst, TO=LTO, Stl=LStl, Blk=LBlk, PF=LPF))
tourney_winning_stats$Outcome <- "W"
tourney_losing_stats$Outcome <- "L"
tourney_stats <- rbind(tourney_winning_stats, tourney_losing_stats)
write.csv(tourney_stats, "march-madness-analytics-2020/CustomFiles/tourney_stats.csv", row.names = TRUE)

train <- select(regular_season_stats, Score, OR, DR, Ast, TO, Stl, Blk)
train$FGP3 <- regular_season_stats$FGM3 / regular_season_stats$FGA3
train$FGP <- regular_season_stats$FGM / regular_season_stats$FGA
train$FTP <- regular_season_stats$FTM / ifelse(regular_season_stats$FTA>0, regular_season_stats$FTA, -1)
test <- select(tourney_stats, Score, OR, DR, Ast, TO, Stl, Blk)
test$FGP3 <- tourney_stats$FGM3 / tourney_stats$FGA3
test$FGP <- tourney_stats$FGM / tourney_stats$FGA
test$FTP <- tourney_stats$FTM / tourney_stats$FTA
true.labels <- tourney_stats$Outcome
misclassification.rate <- array()
for (k in 1:50){
  print(k)
  prediction <- knn(train, test, regular_season_stats$Outcome, k)
  num.incorrect.labels <- sum(prediction != true.labels)
  misclassification.rate[k] <- num.incorrect.labels / nrow(test)
  print(misclassification.rate[k])
}