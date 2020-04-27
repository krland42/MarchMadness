tourney_2019_outcomes_with_comp_sorted <- tourney_2019_outcomes_with_comp[order(tourney_2019_outcomes$gamenum),]
tourney_2019_first_round_with_comp <- tourney_2019_outcomes_with_comp_sorted[1:67,]
for (j in 37:67) {
  tourney_2019_first_round_with_comp[j,] <- data.frame(0, 0, 0, 0, 0, factor("empty"), 0, factor("empty"))
}

for (i in 1:4) {
	if (i == 1 && tourney_2019_first_round_with_comp[1,"WComp"] < tourney_2019_first_round_with_comp[1,"LComp"]) {
		tourney_2019_first_round_with_comp[13,c("LTeamID", "LComp", "LTeamName")] <- tourney_2019_first_round_with_comp[1,c("LTeamID", "LComp", "LTeamName")]
		tourney_2019_first_round_with_comp[1,"Winning_Team"] <- tourney_2019_first_round_with_comp[1,"LTeamID"]
	}
	else if (i == 2 && tourney_2019_first_round_with_comp[2,"WComp"] < tourney_2019_first_round_with_comp[2,"LComp"]) {
		tourney_2019_first_round_with_comp[9,c("LTeamID", "LComp", "LTeamName")] <- tourney_2019_first_round_with_comp[2,c("LTeamID", "LComp", "LTeamName")]
		tourney_2019_first_round_with_comp[2,"Winning_Team"] <- tourney_2019_first_round_with_comp[2,"LTeamID"]
	}
	else if (i == 3 && tourney_2019_first_round_with_comp[3,"WComp"] < tourney_2019_first_round_with_comp[3,"LComp"]) {
		tourney_2019_first_round_with_comp[21,c("LTeamID", "LComp", "LTeamName")] <- tourney_2019_first_round_with_comp[3,c("LTeamID", "LComp", "LTeamName")]
		tourney_2019_first_round_with_comp[3,"Winning_Team"] <- tourney_2019_first_round_with_comp[3,"LTeamID"]
	}
	else if (i == 4 && tourney_2019_first_round_with_comp[4,"WComp"] < tourney_2019_first_round_with_comp[4,"LComp"]) {
		tourney_2019_first_round_with_comp[22,c("LTeamID", "LComp", "LTeamName")] <- tourney_2019_first_round_with_comp[4,c("LTeamID", "LComp", "LTeamName")]
		tourney_2019_first_round_with_comp[4,"Winning_Team"] <- tourney_2019_first_round_with_comp[4,"LTeamID"]
	}
}

gamenum = 37
nxt = 1

tourney_2019_first_round_with_comp <- rename(tourney_2019_first_round_with_comp, c(TeamID1=WTeamID, TeamID2=LTeamID, Winning_Team=Winning_Team, gamenum=gamenum, Comp2=LComp, TeamName2=LTeamName, Comp1=WComp, TeamName1=WTeamName))

for (i in c(22, 33, 25, 35, 13, 12, 16, 15, 9, 6, 17, 8, 21, 31, 7, 14, 34, 28, 29, 32, 19, 18, 24, 30, 26, 36, 5, 10, 27, 23, 20, 11, 37:66)) {
	
	if (nxt == 1) {
		if (tourney_2019_first_round_with_comp[i,"Comp1"] < tourney_2019_first_round_with_comp[i,"Comp2"]) {
			tourney_2019_first_round_with_comp[gamenum,c("TeamID1", "Comp1", "TeamName1")] <- tourney_2019_first_round_with_comp[i,c("TeamID2", "Comp2", "TeamName2")]
			tourney_2019_first_round_with_comp[i,"Winning_Team"] <- tourney_2019_first_round_with_comp[i,"TeamID2"]
		}
		else {
			tourney_2019_first_round_with_comp[gamenum,c("TeamID1", "Comp1", "TeamName1")] <- tourney_2019_first_round_with_comp[i,c("TeamID1", "Comp1", "TeamName1")]
			tourney_2019_first_round_with_comp[i,"Winning_Team"] <- tourney_2019_first_round_with_comp[i,"TeamID1"]
		}
	}
	
	if (nxt == 2) {
		if (tourney_2019_first_round_with_comp[i,"Comp1"] < tourney_2019_first_round_with_comp[i,"Comp2"]) {
			tourney_2019_first_round_with_comp[gamenum,c("TeamID2", "Comp2", "TeamName2")] <- tourney_2019_first_round_with_comp[i,c("TeamID2", "Comp2", "TeamName2")]
			tourney_2019_first_round_with_comp[i,"Winning_Team"] <- tourney_2019_first_round_with_comp[i,"TeamID2"]
		}
		else {
			tourney_2019_first_round_with_comp[gamenum,c("TeamID2", "Comp2", "TeamName2")] <- tourney_2019_first_round_with_comp[i,c("TeamID1", "Comp1", "TeamName1")]
			tourney_2019_first_round_with_comp[i,"Winning_Team"] <- tourney_2019_first_round_with_comp[i,"TeamID1"]
		}
		
		gamenum = gamenum + 1
		nxt = 0
	}
	
	nxt = nxt + 1
}

if (tourney_2019_first_round_with_comp[67,"Comp1"] < tourney_2019_first_round_with_comp[67,"Comp2"]) {
	tourney_2019_first_round_with_comp[67,"Winning_Team"] <- tourney_2019_first_round_with_comp[67,"TeamID2"]
} else {
	tourney_2019_first_round_with_comp[67,"Winning_Team"] <- tourney_2019_first_round_with_comp[67,"TeamID1"]
}

length(which(tourney_2019_outcomes_with_comp_sorted$Winning_Team != tourney_2019_first_round_with_comp$Winning_Team))