library(dplyr)

tourney_detailed <- read.csv("march-madness-analytics-2020/2020DataFiles/2020DataFiles/2020-Mens-Data/MDataFiles_Stage1/MNCAATourneyDetailedResults.csv")
seeds <- read.csv("march-madness-analytics-2020/2020DataFiles/2020DataFiles/2020-Mens-Data/MDataFiles_Stage1/MNCAATourneySeeds.csv")
ranks <- read.csv("march-madness-analytics-2020/2020DataFiles/2020DataFiles/2020-Mens-Data/MDataFiles_Stage1/MMasseyOrdinals.csv")

tourney_2019_outcomes <- subset(tourney_detailed, Season == 2019, select = c(3, 5))
seeds_2019 <- subset(seeds, Season == 2019, select = c(2, 3))
ranks_2019 <- subset(ranks, Season == 2019 & SystemName == "SEL" & RankingDayNum == 133, select = c(4, 5))
tourney_2019_outcomes$Winning_Team <- tourney_2019_outcomes$WTeamID
tourney_2019_outcomes$gamenum <- 1:nrow(tourney_2019_outcomes)

seeds_2019$Seed <- as.numeric(gsub("[^[:digit:]]", "", seeds_2019$Seed))

tourney_2019_outcomes <- merge(tourney_2019_outcomes, seeds_2019, by.x="LTeamID", by.y="TeamID")
tourney_2019_outcomes <- merge(tourney_2019_outcomes, ranks_2019, by.x="LTeamID", by.y="TeamID")

tourney_2019_outcomes <- rename(tourney_2019_outcomes, c(LSeed=Seed))
tourney_2019_outcomes <- rename(tourney_2019_outcomes, c(LRank=OrdinalRank))

tourney_2019_outcomes <- merge(tourney_2019_outcomes, seeds_2019, by.x="WTeamID", by.y="TeamID")
tourney_2019_outcomes <- merge(tourney_2019_outcomes, ranks_2019, by.x="WTeamID", by.y="TeamID")

tourney_2019_outcomes <- rename(tourney_2019_outcomes, c(WSeed=Seed))
tourney_2019_outcomes <- rename(tourney_2019_outcomes, c(WRank=OrdinalRank))

wrong = 0

for (i in 1:67) {
	
	if (tourney_2019_outcomes[i,"WSeed"] > tourney_2019_outcomes[i,"LSeed"]) {
		wrong = wrong + 1
	}
	else if (tourney_2019_outcomes[i,"WSeed"] == tourney_2019_outcomes[i,"LSeed"]) {
		if (tourney_2019_outcomes[i,"WRank"] > tourney_2019_outcomes[i,"LRank"]) {
			wrong = wrong + 1
		}
	}
}