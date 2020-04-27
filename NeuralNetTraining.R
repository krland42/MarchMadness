library(dplyr)
regular_season_detailed <- read.csv("march-madness-analytics-2020/2020DataFiles/2020DataFiles/2020-Mens-Data/MDataFiles_Stage1/MRegularSeasonDetailedResults.csv")
regular_season_winning_stats <- subset(regular_season_detailed, Season != 2019, select = c(4, 9:21)) 
regular_season_losing_stats <- subset(regular_season_detailed, Season != 2019, select = c(6, 22:34)) 
regular_season_winning_stats <- rename(regular_season_winning_stats, c(Score=WScore, FGM=WFGM, FGA=WFGA, FGM3=WFGM3, FGA3=WFGA3, FTM=WFTM, FTA=WFTA, OR=WOR, DR=WDR, Ast=WAst, TO=WTO, Stl=WStl, Blk=WBlk, PF=WPF))
regular_season_losing_stats <- rename(regular_season_losing_stats, c(Score=LScore, FGM=LFGM, FGA=LFGA, FGM3=LFGM3, FGA3=LFGA3, FTM=LFTM, FTA=LFTA, OR=LOR, DR=LDR, Ast=LAst, TO=LTO, Stl=LStl, Blk=LBlk, PF=LPF))
regular_season_winning_stats$Outcome <- 1 
regular_season_losing_stats$Outcome <- 0

tourney_detailed <- read.csv("march-madness-analytics-2020/2020DataFiles/2020DataFiles/2020-Mens-Data/MDataFiles_Stage1/MNCAATourneyDetailedResults.csv")
tourney_winning_stats <- subset(tourney_detailed, Season != 2019, select = c(4, 9:21))
tourney_losing_stats <- subset(tourney_detailed, Season != 2019,select = c(6, 22:34))
tourney_winning_stats <- rename(tourney_winning_stats, c(Score=WScore, FGM=WFGM, FGA=WFGA, FGM3=WFGM3, FGA3=WFGA3, FTM=WFTM, FTA=WFTA, OR=WOR, DR=WDR, Ast=WAst, TO=WTO, Stl=WStl, Blk=WBlk, PF=WPF))
tourney_losing_stats <- rename(tourney_losing_stats, c(Score=LScore, FGM=LFGM, FGA=LFGA, FGM3=LFGM3, FGA3=LFGA3, FTM=LFTM, FTA=LFTA, OR=LOR, DR=LDR, Ast=LAst, TO=LTO, Stl=LStl, Blk=LBlk, PF=LPF))
tourney_winning_stats$Outcome <- 1
tourney_losing_stats$Outcome <- 0

training_data <- rbind(regular_season_winning_stats, regular_season_losing_stats, tourney_winning_stats, tourney_losing_stats) # Combine regular season and tourney data this time
training_data$TR <- training_data$OR + training_data$DR
training_data$FGP3 <- training_data$FGM3 / training_data$FGA3
training_data$FGP <- training_data$FGM / training_data$FGA
training_data$FTP <- training_data$FTM / ifelse(training_data$FTA>0, training_data$FTA, -1)

library(neuralnet)
nn=neuralnet(Outcome~Score+TR+FGP+FGP3+FTP+Ast+TO+Stl+Blk, data=training_data, hidden=c(6,4), act.fct="logistic", linear.output=FALSE, lifesign="full", threshold=30.0)
plot(nn, show.weights = FALSE, information = FALSE, col.entry = "#4F2984", col.hidden = "#4F2984", col.out = "#4F2984", col.hidden.synapse = "#FFDD00", intercept = FALSE, fontsize = 20)