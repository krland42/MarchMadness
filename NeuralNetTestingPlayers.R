detach("package:plyr", unload = TRUE)
library(dplyr)
regular_season_detailed <- read.csv("march-madness-analytics-2020/2020DataFiles/2020DataFiles/2020-Mens-Data/MDataFiles_Stage1/MRegularSeasonDetailedResults.csv")
regular_season_2019_winning_stats <- subset(regular_season_detailed, Season == 2019, select = c(1:3, 4, 9:21))
regular_season_2019_losing_stats <- subset(regular_season_detailed, Season == 2019, select = c(1:2, 5, 6, 22:34))
regular_season_2019_winning_stats <- rename(regular_season_2019_winning_stats, c(TeamID=WTeamID, Score=WScore, FGM=WFGM, FGA=WFGA, FGM3=WFGM3, FGA3=WFGA3, FTM=WFTM, FTA=WFTA, OR=WOR, DR=WDR, Ast=WAst, TO=WTO, Stl=WStl, Blk=WBlk, PF=WPF))
regular_season_2019_losing_stats <- rename(regular_season_2019_losing_stats, c(TeamID=LTeamID, Score=LScore, FGM=LFGM, FGA=LFGA, FGM3=LFGM3, FGA3=LFGA3, FTM=LFTM, FTA=LFTA, OR=LOR, DR=LDR, Ast=LAst, TO=LTO, Stl=LStl, Blk=LBlk, PF=LPF))
regular_season_2019_winning_stats$Outcome <- 1
regular_season_2019_losing_stats$Outcome <- 0
regular_season_2019_stats <- rbind(regular_season_2019_winning_stats, regular_season_2019_losing_stats)
regular_season_2019_stats <- merge(regular_season_2019_stats, players_played_per_game_testing, by.x=c("Season", "DayNum", "TeamID"), by.y=c("Season", "DayNum", "EventTeamID"))

detach("package:dplyr", unload = TRUE)
library(plyr)
regular_season_2019_total_stats <- ddply(regular_season_2019_stats, "TeamID", numcolwise(sum))
regular_season_2019_total_stats$games <- count(regular_season_2019_stats$TeamID)$freq

regular_season_2019_average_stats <- regular_season_2019_total_stats / regular_season_2019_total_stats$games
regular_season_2019_average_stats$TeamID <- regular_season_2019_average_stats$TeamID * regular_season_2019_total_stats$games
regular_season_2019_average_stats$TR <- regular_season_2019_average_stats$OR + regular_season_2019_average_stats$DR
regular_season_2019_average_stats$FGP3 <- regular_season_2019_average_stats$FGM3 / regular_season_2019_average_stats$FGA3
regular_season_2019_average_stats$FGP <- regular_season_2019_average_stats$FGM / regular_season_2019_average_stats$FGA
regular_season_2019_average_stats$FTP <- regular_season_2019_average_stats$FTM / ifelse(regular_season_2019_average_stats$FTA>0, regular_season_2019_average_stats$FTA, -1)

detach("package:plyr", unload = TRUE)
library(dplyr)
test <- select(regular_season_2019_average_stats, Score, TR, FGP, FGP3, FTP, Ast, TO, Stl, Blk, PlayersPlayed)

detach("package:dplyr", unload = TRUE)
library(neuralnet)
predict=compute(nnplayer,test)
competitiveness_player <- data.frame(regular_season_2019_average_stats$TeamID, predict$net.result)
detach("package:neuralnet", unload = TRUE)
library(dplyr)
competitiveness_player <- rename(competitiveness_player, c(TeamID=regular_season_2019_average_stats.TeamID, competitiveness=predict.net.result))

tourney_detailed <- read.csv("march-madness-analytics-2020/2020DataFiles/2020DataFiles/2020-Mens-Data/MDataFiles_Stage1/MNCAATourneyDetailedResults.csv")
team_names <- read.csv("march-madness-analytics-2020/2020DataFiles/2020DataFiles/2020-Mens-Data/MDataFiles_Stage1/Mteams.csv")
tourney_2019_outcomes <- subset(tourney_detailed, Season == 2019, select = c(3, 5))
tourney_2019_outcomes$Winning_Team <- tourney_2019_outcomes$WTeamID
tourney_2019_outcomes$gamenum <- 1:nrow(tourney_2019_outcomes)
tourney_2019_outcomes <- merge(tourney_2019_outcomes, competitiveness_player, by.x="LTeamID", by.y="TeamID")
tourney_2019_outcomes <- merge(tourney_2019_outcomes, team_names[,1:2], by.x="LTeamID", by.y="TeamID")
tourney_2019_outcomes <- rename(tourney_2019_outcomes, c(LComp=competitiveness))
tourney_2019_outcomes <- rename(tourney_2019_outcomes, c(LTeamName=TeamName))
tourney_2019_outcomes <- merge(tourney_2019_outcomes, competitiveness_player, by.x="WTeamID", by.y="TeamID")
tourney_2019_outcomes <- merge(tourney_2019_outcomes, team_names[,1:2], by.x="WTeamID", by.y="TeamID")
tourney_2019_outcomes <- rename(tourney_2019_outcomes, c(WComp=competitiveness))
tourney_2019_outcomes <- rename(tourney_2019_outcomes, c(WTeamName=TeamName))
length(which(tourney_2019_outcomes$WComp < tourney_2019_outcomes$LComp))