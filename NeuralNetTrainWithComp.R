library(dplyr)
regular_season_detailed <- read.csv("march-madness-analytics-2020/2020DataFiles/2020DataFiles/2020-Mens-Data/MDataFiles_Stage1/MRegularSeasonDetailedResults.csv")
tourney_detailed <- read.csv("march-madness-analytics-2020/2020DataFiles/2020DataFiles/2020-Mens-Data/MDataFiles_Stage1/MNCAATourneyDetailedResults.csv")
all_detailed <- rbind(regular_season_detailed, tourney_detailed)

all_detailed_with_comp <- subset(all_detailed, Season != 2019)
all_detailed_with_comp <- merge(all_detailed_with_comp, competitiveness_all2, by.x=c("Season", "LTeamID"), by.y=c("Season", "TeamID"))
all_detailed_with_comp <- rename(all_detailed_with_comp, c(LComp=competitiveness))
all_detailed_with_comp <- merge(all_detailed_with_comp, competitiveness_all2, by.x=c("Season", "WTeamID"), by.y=c("Season", "TeamID"))
all_detailed_with_comp <- rename(all_detailed_with_comp, c(WComp=competitiveness))

all_winning_stats <- subset(all_detailed_with_comp, Season != 2019, select = c(5, 9:21, 35)) 
all_losing_stats <- subset(all_detailed_with_comp, Season != 2019, select = c(6, 22:34, 36)) 
all_winning_stats <- rename(all_winning_stats, c(Score=WScore, FGM=WFGM, FGA=WFGA, FGM3=WFGM3, FGA3=WFGA3, FTM=WFTM, FTA=WFTA, OR=WOR, DR=WDR, Ast=WAst, TO=WTO, Stl=WStl, Blk=WBlk, PF=WPF, OComp=LComp))
all_losing_stats <- rename(all_losing_stats, c(Score=LScore, FGM=LFGM, FGA=LFGA, FGM3=LFGM3, FGA3=LFGA3, FTM=LFTM, FTA=LFTA, OR=LOR, DR=LDR, Ast=LAst, TO=LTO, Stl=LStl, Blk=LBlk, PF=LPF, OComp=WComp))
all_winning_stats$Outcome <- 1 
all_losing_stats$Outcome <- 0

training_data_with_comp <- rbind(all_winning_stats, all_losing_stats)
training_data_with_comp$TR <- training_data_with_comp$OR + training_data_with_comp$DR
training_data_with_comp$FGP3 <- training_data_with_comp$FGM3 / training_data_with_comp$FGA3
training_data_with_comp$FGP <- training_data_with_comp$FGM / training_data_with_comp$FGA
training_data_with_comp$FTP <- training_data_with_comp$FTM / ifelse(training_data_with_comp$FTA>0, training_data_with_comp$FTA, -1)

library(neuralnet)
nn2=neuralnet(Outcome~Score+TR+FGP+FGP3+FTP+Ast+TO+Stl+Blk+OComp, data=training_data_with_comp, hidden=c(6,4), act.fct="logistic", linear.output=FALSE, lifesign="full", threshold=30.0)
plot(nn2, show.weights = FALSE, information = FALSE, col.entry = "#4F2984", col.hidden = "#4F2984", col.out = "#4F2984", col.hidden.synapse = "#FFDD00", intercept = FALSE)
