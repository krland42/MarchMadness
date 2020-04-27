detach("package:plyr", unload = TRUE)
library(dplyr)
regular_season_detailed <- read.csv("march-madness-analytics-2020/2020DataFiles/2020DataFiles/2020-Mens-Data/MDataFiles_Stage1/MRegularSeasonDetailedResults.csv")
regular_season_2019_with_comp <- subset(regular_season_detailed, Season == 2019)

regular_season_2019_with_comp <- merge(regular_season_2019_with_comp, competitiveness, by.x="LTeamID", by.y="TeamID")
regular_season_2019_with_comp <- rename(regular_season_2019_with_comp, c(LComp=competitiveness))
regular_season_2019_with_comp <- merge(regular_season_2019_with_comp, competitiveness, by.x="WTeamID", by.y="TeamID")
regular_season_2019_with_comp <- rename(regular_season_2019_with_comp, c(WComp=competitiveness))

regular_season_2019_winning_stats_with_comp <- subset(regular_season_2019_with_comp, Season == 2019, select = c(1, 5, 9:21, 35)) 
regular_season_2019_losing_stats_with_comp <- subset(regular_season_2019_with_comp, Season == 2019, select = c(2, 6, 22:34, 36)) 
regular_season_2019_winning_stats_with_comp <- rename(regular_season_2019_winning_stats_with_comp, c(TeamID=WTeamID, Score=WScore, FGM=WFGM, FGA=WFGA, FGM3=WFGM3, FGA3=WFGA3, FTM=WFTM, FTA=WFTA, OR=WOR, DR=WDR, Ast=WAst, TO=WTO, Stl=WStl, Blk=WBlk, PF=WPF, OComp=LComp))
regular_season_2019_losing_stats_with_comp <- rename(regular_season_2019_losing_stats_with_comp, c(TeamID=LTeamID, Score=LScore, FGM=LFGM, FGA=LFGA, FGM3=LFGM3, FGA3=LFGA3, FTM=LFTM, FTA=LFTA, OR=LOR, DR=LDR, Ast=LAst, TO=LTO, Stl=LStl, Blk=LBlk, PF=LPF, OComp=WComp))
regular_season_2019_winning_stats_with_comp$Outcome <- 1 
regular_season_2019_losing_stats_with_comp$Outcome <- 0
regular_season_2019_stats_with_comp <- rbind(regular_season_2019_winning_stats_with_comp, regular_season_2019_losing_stats_with_comp)
regular_season_2019_stats_with_comp  <- subset(regular_season_2019_stats_with_comp, OComp > 0.50)
# 23 without subset
detach("package:dplyr", unload = TRUE)
library(plyr)
regular_season_2019_total_stats_with_comp <- ddply(regular_season_2019_stats_with_comp, "TeamID", numcolwise(sum))
regular_season_2019_total_stats_with_comp$games <- count(regular_season_2019_stats_with_comp$TeamID)$freq

regular_season_2019_average_stats_with_comp <- regular_season_2019_total_stats_with_comp / regular_season_2019_total_stats_with_comp$games
regular_season_2019_average_stats_with_comp$TeamID <- regular_season_2019_average_stats_with_comp$TeamID * regular_season_2019_total_stats_with_comp$games
regular_season_2019_average_stats_with_comp$TR <- regular_season_2019_average_stats_with_comp$OR + regular_season_2019_average_stats_with_comp$DR
regular_season_2019_average_stats_with_comp$FGP3 <- regular_season_2019_average_stats_with_comp$FGM3 / regular_season_2019_average_stats_with_comp$FGA3
regular_season_2019_average_stats_with_comp$FGP <- regular_season_2019_average_stats_with_comp$FGM / regular_season_2019_average_stats_with_comp$FGA
regular_season_2019_average_stats_with_comp$FTP <- regular_season_2019_average_stats_with_comp$FTM / ifelse(regular_season_2019_average_stats_with_comp$FTA>0, regular_season_2019_average_stats_with_comp$FTA, -1)

detach("package:plyr", unload = TRUE)
library(dplyr)
test_with_comp <- select(regular_season_2019_average_stats_with_comp, Score, TR, FGP, FGP3, FTP, Ast, TO, Stl, Blk)

detach("package:dplyr", unload = TRUE)
library(neuralnet)
predict=compute(nn2,test_with_comp)
competitiveness_with_comp <- data.frame(regular_season_2019_average_stats_with_comp$TeamID, predict$net.result)
detach("package:neuralnet", unload = TRUE)
library(dplyr)
competitiveness_with_comp <- rename(competitiveness_with_comp, c(TeamID=regular_season_2019_average_stats_with_comp.TeamID, competitiveness=predict.net.result))

tourney_detailed <- read.csv("march-madness-analytics-2020/2020DataFiles/2020DataFiles/2020-Mens-Data/MDataFiles_Stage1/MNCAATourneyDetailedResults.csv")
team_names <- read.csv("march-madness-analytics-2020/2020DataFiles/2020DataFiles/2020-Mens-Data/MDataFiles_Stage1/Mteams.csv")
tourney_2019_outcomes_with_comp <- subset(tourney_detailed, Season == 2019, select = c(3, 5))
tourney_2019_outcomes_with_comp$Winning_Team <- tourney_2019_outcomes_with_comp$WTeamID
tourney_2019_outcomes_with_comp$gamenum <- 1:nrow(tourney_2019_outcomes_with_comp)
tourney_2019_outcomes_with_comp <- merge(tourney_2019_outcomes_with_comp, competitiveness_with_comp, by.x="LTeamID", by.y="TeamID")
tourney_2019_outcomes_with_comp <- merge(tourney_2019_outcomes_with_comp, team_names[,1:2], by.x="LTeamID", by.y="TeamID")
tourney_2019_outcomes_with_comp <- rename(tourney_2019_outcomes_with_comp, c(LComp=competitiveness))
tourney_2019_outcomes_with_comp <- rename(tourney_2019_outcomes_with_comp, c(LTeamName=TeamName))
tourney_2019_outcomes_with_comp <- merge(tourney_2019_outcomes_with_comp, competitiveness_with_comp, by.x="WTeamID", by.y="TeamID")
tourney_2019_outcomes_with_comp <- merge(tourney_2019_outcomes_with_comp, team_names[,1:2], by.x="WTeamID", by.y="TeamID")
tourney_2019_outcomes_with_comp <- rename(tourney_2019_outcomes_with_comp, c(WComp=competitiveness))
tourney_2019_outcomes_with_comp <- rename(tourney_2019_outcomes_with_comp, c(WTeamName=TeamName))
length(which(tourney_2019_outcomes_with_comp$WComp < tourney_2019_outcomes_with_comp$LComp))