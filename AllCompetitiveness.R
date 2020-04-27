library(dplyr)
regular_season_detailed2 <- read.csv("march-madness-analytics-2020/2020DataFiles/2020DataFiles/2020-Mens-Data/MDataFiles_Stage1/MRegularSeasonDetailedResults.csv")
regular_season_winning_stats2 <- subset(regular_season_detailed2, Season != 2019, select = c(1, 3, 4, 9:21))
regular_season_losing_stats2 <- subset(regular_season_detailed2, Season != 2019, select = c(1, 5, 6, 22:34))
regular_season_winning_stats2 <- rename(regular_season_winning_stats2, c(TeamID=WTeamID, Score=WScore, FGM=WFGM, FGA=WFGA, FGM3=WFGM3, FGA3=WFGA3, FTM=WFTM, FTA=WFTA, OR=WOR, DR=WDR, Ast=WAst, TO=WTO, Stl=WStl, Blk=WBlk, PF=WPF))
regular_season_losing_stats2 <- rename(regular_season_losing_stats2, c(TeamID=LTeamID, Score=LScore, FGM=LFGM, FGA=LFGA, FGM3=LFGM3, FGA3=LFGA3, FTM=LFTM, FTA=LFTA, OR=LOR, DR=LDR, Ast=LAst, TO=LTO, Stl=LStl, Blk=LBlk, PF=LPF))
regular_season_winning_stats2$Outcome <- 1
regular_season_losing_stats2$Outcome <- 0

tourney_detailed2 <- read.csv("march-madness-analytics-2020/2020DataFiles/2020DataFiles/2020-Mens-Data/MDataFiles_Stage1/MNCAATourneyDetailedResults.csv")
tourney_winning_stats2 <- subset(tourney_detailed2, Season != 2019, select = c(1, 3, 4, 9:21))
tourney_losing_stats2 <- subset(tourney_detailed2, Season != 2019,select = c(1, 5, 6, 22:34))
tourney_winning_stats2 <- rename(tourney_winning_stats2, c(TeamID=WTeamID, Score=WScore, FGM=WFGM, FGA=WFGA, FGM3=WFGM3, FGA3=WFGA3, FTM=WFTM, FTA=WFTA, OR=WOR, DR=WDR, Ast=WAst, TO=WTO, Stl=WStl, Blk=WBlk, PF=WPF))
tourney_losing_stats2 <- rename(tourney_losing_stats2, c(TeamID=LTeamID, Score=LScore, FGM=LFGM, FGA=LFGA, FGM3=LFGM3, FGA3=LFGA3, FTM=LFTM, FTA=LFTA, OR=LOR, DR=LDR, Ast=LAst, TO=LTO, Stl=LStl, Blk=LBlk, PF=LPF))
tourney_winning_stats2$Outcome <- 1
tourney_losing_stats2$Outcome <- 0

all_stats2 <- rbind(regular_season_winning_stats2, regular_season_losing_stats2, tourney_winning_stats2, tourney_losing_stats2)

detach("package:dplyr", unload = TRUE)
library(plyr)
all_total_stats2 <- ddply(all_stats2, c("Season", "TeamID"), numcolwise(sum))
all_total_stats2$games <- count(all_stats2, vars=c("Season", "TeamID"))$freq

all_average_stats2 <- all_total_stats2 / all_total_stats2$games
all_average_stats2$TeamID <- all_average_stats2$TeamID * all_total_stats2$games
all_average_stats2$Season <- all_average_stats2$Season * all_total_stats2$games
all_average_stats2$TR <- all_average_stats2$OR + all_average_stats2$DR
all_average_stats2$FGP3 <- all_average_stats2$FGM3 / all_average_stats2$FGA3
all_average_stats2$FGP <- all_average_stats2$FGM / all_average_stats2$FGA
all_average_stats2$FTP <- all_average_stats2$FTM / ifelse(all_average_stats2$FTA>0, all_average_stats2$FTA, -1)

detach("package:plyr", unload = TRUE)
library(dplyr)
test_all2 <- select(all_average_stats2, Score, TR, FGP, FGP3, FTP, Ast, TO, Stl, Blk)

detach("package:dplyr", unload = TRUE)
library(neuralnet)
predict_all2=compute(nn,test_all2)
competitiveness_all2 <- data.frame(all_average_stats2$Season, all_average_stats2$TeamID, predict_all2$net.result)
detach("package:neuralnet", unload = TRUE)
library(dplyr)
competitiveness_all2 <- rename(competitiveness_all2, c(Season=all_average_stats2.Season, TeamID=all_average_stats2.TeamID, competitiveness=predict_all2.net.result))

write.csv(competitiveness_all2, "march-madness-analytics-2020/CustomFiles/all-competitiveness.csv", row.names = TRUE)