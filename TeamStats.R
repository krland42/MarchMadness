library(dplyr)
tourney_detailed <- read.csv("march-madness-analytics-2020/2020DataFiles/2020DataFiles/2020-Mens-Data/MDataFiles_Stage1/MNCAATourneyDetailedResults.csv")
placeholder_winning_stats <- subset(tourney_detailed, Season != 2019, select = c(3, 4, 9:21))
placeholder_losing_stats <- subset(tourney_detailed, Season != 2019, select = c(5, 6, 22:34))
placeholder_winning_stats <- rename(placeholder_winning_stats, c(TeamID=WTeamID, Score=WScore, FGM=WFGM, FGA=WFGA, FGM3=WFGM3, FGA3=WFGA3, FTM=WFTM, FTA=WFTA, OR=WOR, DR=WDR, Ast=WAst, TO=WTO, Stl=WStl, Blk=WBlk, PF=WPF))
placeholder_losing_stats <- rename(placeholder_losing_stats, c(TeamID=LTeamID, Score=LScore, FGM=LFGM, FGA=LFGA, FGM3=LFGM3, FGA3=LFGA3, FTM=LFTM, FTA=LFTA, OR=LOR, DR=LDR, Ast=LAst, TO=LTO, Stl=LStl, Blk=LBlk, PF=LPF))
placeholder_stats <- rbind(placeholder_winning_stats, placeholder_losing_stats)
placeholder_stats$TR <- placeholder_stats$OR + placeholder_stats$DR
placeholder_stats$FGP3 <- placeholder_stats$FGM3 / placeholder_stats$FGA3
placeholder_stats$FGP <- placeholder_stats$FGM / placeholder_stats$FGA
placeholder_stats$FTP <- placeholder_stats$FTM / ifelse(placeholder_stats$FTA>0, placeholder_stats$FTA, -1)

i = 1

for (year in 2003:2018) {
	regular_season_detailed_year <- read.csv("march-madness-analytics-2020/2020DataFiles/2020DataFiles/2020-Mens-Data/MDataFiles_Stage1/MRegularSeasonDetailedResults.csv")
	regular_season_winning_stats_year <- subset(regular_season_detailed_year, Season == year, select = c(3, 4, 9:21))
	regular_season_losing_stats_year <- subset(regular_season_detailed_year, Season == year, select = c(5, 6, 22:34))
	regular_season_winning_stats_year <- rename(regular_season_winning_stats_year, c(TeamID=WTeamID, Score=WScore, FGM=WFGM, FGA=WFGA, FGM3=WFGM3, FGA3=WFGA3, FTM=WFTM, FTA=WFTA, OR=WOR, DR=WDR, Ast=WAst, TO=WTO, Stl=WStl, Blk=WBlk, PF=WPF))
	regular_season_losing_stats_year <- rename(regular_season_losing_stats_year, c(TeamID=LTeamID, Score=LScore, FGM=LFGM, FGA=LFGA, FGM3=LFGM3, FGA3=LFGA3, FTM=LFTM, FTA=LFTA, OR=LOR, DR=LDR, Ast=LAst, TO=LTO, Stl=LStl, Blk=LBlk, PF=LPF))
	regular_season_stats_year <- rbind(regular_season_winning_stats_year, regular_season_losing_stats_year)
	
	detach("package:dplyr", unload = TRUE)
	library(plyr)
	regular_season_total_stats_year <- ddply(regular_season_stats_year, "TeamID", numcolwise(sum))
	regular_season_total_stats_year$games <- count(regular_season_stats_year$TeamID)$freq
	
	regular_season_average_stats_year <- regular_season_total_stats_year / regular_season_total_stats_year$games
	regular_season_average_stats_year$TeamID <- regular_season_average_stats_year$TeamID * regular_season_total_stats_year$games
	regular_season_average_stats_year$TR <- regular_season_average_stats_year$OR + regular_season_average_stats_year$DR
	regular_season_average_stats_year$FGP3 <- regular_season_average_stats_year$FGM3 / regular_season_average_stats_year$FGA3
	regular_season_average_stats_year$FGP <- regular_season_average_stats_year$FGM / regular_season_average_stats_year$FGA
	regular_season_average_stats_year$FTP <- regular_season_average_stats_year$FTM / ifelse(regular_season_average_stats_year$FTA>0, regular_season_average_stats_year$FTA, -1)
	regular_season_average_stats_year$games <- NULL
	
	detach("package:plyr", unload = TRUE)
	library(dplyr)
	tourney_detailed_year <- read.csv("march-madness-analytics-2020/2020DataFiles/2020DataFiles/2020-Mens-Data/MDataFiles_Stage1/MNCAATourneyDetailedResults.csv")
	tourney_winning_stats_year <- subset(tourney_detailed_year, Season == year, select = c(3, 4, 9:21))
	tourney_losing_stats_year <- subset(tourney_detailed_year, Season == year, select = c(5, 6, 22:34))
	tourney_winning_stats_year <- rename(tourney_winning_stats_year, c(TeamID=WTeamID, Score=WScore, FGM=WFGM, FGA=WFGA, FGM3=WFGM3, FGA3=WFGA3, FTM=WFTM, FTA=WFTA, OR=WOR, DR=WDR, Ast=WAst, TO=WTO, Stl=WStl, Blk=WBlk, PF=WPF))
	tourney_losing_stats_year <- rename(tourney_losing_stats_year, c(TeamID=LTeamID, Score=LScore, FGM=LFGM, FGA=LFGA, FGM3=LFGM3, FGA3=LFGA3, FTM=LFTM, FTA=LFTA, OR=LOR, DR=LDR, Ast=LAst, TO=LTO, Stl=LStl, Blk=LBlk, PF=LPF))
	tourney_stats_year <- rbind(tourney_winning_stats_year, tourney_losing_stats_year)
	tourney_stats_year$TR <- tourney_stats_year$OR + tourney_stats_year$DR
	tourney_stats_year$FGP3 <- tourney_stats_year$FGM3 / tourney_stats_year$FGA3
	tourney_stats_year$FGP <- tourney_stats_year$FGM / tourney_stats_year$FGA
	tourney_stats_year$FTP <- tourney_stats_year$FTM / ifelse(tourney_stats_year$FTA>0, tourney_stats_year$FTA, -1)
	
	for (j in 1:nrow(tourney_stats_year)) {
		temp <- subset(regular_season_average_stats_year, TeamID == tourney_stats_year[j,1])
		placeholder_stats[i,] <- tourney_stats_year[j,] - temp[1,]
		i = i + 1
	}
}

c <- colSums(placeholder_stats)
c <- c / nrow(placeholder_stats)