tourney_2019_outcomes_sorted <- tourney_2019_outcomes[order(tourney_2019_outcomes$gamenum),]
tourney_2019_first_round <- tourney_2019_outcomes_sorted[1:67,]
for (j in 37:67) {
  tourney_2019_first_round[j,] <- data.frame(0, 0, 0, 0, 0, 0, 0, 0)
}

for (i in 1:4) {
	if (i == 1 && tourney_2019_first_round[1,"WRank"] > tourney_2019_first_round[1,"LRank"]) {
		tourney_2019_first_round[13,c("LTeamID", "LSeed", "LRank")] <- tourney_2019_first_round[1,c("LTeamID", "LSeed", "LRank")]
		tourney_2019_first_round[1,"Winning_Team"] <- tourney_2019_first_round[1,"LTeamID"]
	}
	else {
		tourney_2019_first_round[1,"Winning_Team"] <- tourney_2019_first_round[1,"WTeamID"]
	}
	if (i == 2 && tourney_2019_first_round[2,"WRank"] > tourney_2019_first_round[2,"LRank"]) {
		tourney_2019_first_round[9,c("LTeamID", "LSeed", "LRank")] <- tourney_2019_first_round[2,c("LTeamID", "LSeed", "LRank")]
		tourney_2019_first_round[2,"Winning_Team"] <- tourney_2019_first_round[2,"LTeamID"]
	}
	else {
		tourney_2019_first_round[2,"Winning_Team"] <- tourney_2019_first_round[2,"WTeamID"]
	}
	if (i == 3 && tourney_2019_first_round[3,"WRank"] > tourney_2019_first_round[3,"LRank"]) {
		tourney_2019_first_round[21,c("LTeamID", "LSeed", "LRank")] <- tourney_2019_first_round[3,c("LTeamID", "LSeed", "LRank")]
		tourney_2019_first_round[3,"Winning_Team"] <- tourney_2019_first_round[3,"LTeamID"]
	}
	else {
		tourney_2019_first_round[3,"Winning_Team"] <- tourney_2019_first_round[3,"WTeamID"]
	}
	if (i == 4 && tourney_2019_first_round[4,"WRank"] > tourney_2019_first_round[4,"LRank"]) {
		tourney_2019_first_round[22,c("LTeamID", "LSeed", "LRank")] <- tourney_2019_first_round[4,c("LTeamID", "LSeed", "LRank")]
		tourney_2019_first_round[4,"Winning_Team"] <- tourney_2019_first_round[4,"LTeamID"]
	}
	else {
		tourney_2019_first_round[4,"Winning_Team"] <- tourney_2019_first_round[4,"WTeamID"]
	}
}

gamenum = 37
nxt = 1

tourney_2019_first_round <- rename(tourney_2019_first_round, c(TeamID1=WTeamID, TeamID2=LTeamID, Winning_Team=Winning_Team, gamenum=gamenum, Seed2=LSeed, Rank2=LRank, Seed1=WSeed, Rank1=WRank))

for (i in c(22, 33, 25, 35, 13, 12, 16, 15, 9, 6, 17, 8, 21, 31, 7, 14, 34, 28, 29, 32, 19, 18, 24, 30, 26, 36, 5, 10, 27, 23, 20, 11, 37:66)) {
	
	if (nxt == 1) {
		if (tourney_2019_first_round[i,"Seed1"] > tourney_2019_first_round[i,"Seed2"]) {
			tourney_2019_first_round[gamenum,c("TeamID1", "Seed1", "Rank1")] <- tourney_2019_first_round[i,c("TeamID2", "Seed2", "Rank2")]
			tourney_2019_first_round[i,"Winning_Team"] <- tourney_2019_first_round[i,"TeamID2"]
		}
	  else if (tourney_2019_first_round[i,"Seed1"] == tourney_2019_first_round[i,"Seed2"]) {
	    if (tourney_2019_first_round[i,"Rank1"] > tourney_2019_first_round[i,"Rank2"]) {
	      tourney_2019_first_round[gamenum,c("TeamID1", "Seed1", "Rank1")] <- tourney_2019_first_round[i,c("TeamID2", "Seed2", "Rank2")]
	      tourney_2019_first_round[i,"Winning_Team"] <- tourney_2019_first_round[i,"TeamID2"]
	    }
	  }
		else {
			tourney_2019_first_round[gamenum,c("TeamID1", "Seed1", "Rank1")] <- tourney_2019_first_round[i,c("TeamID1", "Seed1", "Rank1")]
			tourney_2019_first_round[i,"Winning_Team"] <- tourney_2019_first_round[i,"TeamID1"]
		}
	}
	
	if (nxt == 2) {
		if (tourney_2019_first_round[i,"Seed1"] > tourney_2019_first_round[i,"Seed2"]) {
			tourney_2019_first_round[gamenum,c("TeamID2", "Seed2", "Rank2")] <- tourney_2019_first_round[i,c("TeamID2", "Seed2", "Rank2")]
			tourney_2019_first_round[i,"Winning_Team"] <- tourney_2019_first_round[i,"TeamID2"]
		}
	  else if (tourney_2019_first_round[i,"Seed1"] == tourney_2019_first_round[i,"Seed2"]) {
      if (tourney_2019_first_round[i,"Rank1"] > tourney_2019_first_round[i,"Rank2"]) {
        tourney_2019_first_round[gamenum,c("TeamID2", "Seed2", "Rank2")] <- tourney_2019_first_round[i,c("TeamID2", "Seed2", "Rank2")]
        tourney_2019_first_round[i,"Winning_Team"] <- tourney_2019_first_round[i,"TeamID2"]
      }
	  }
		else {
			tourney_2019_first_round[gamenum,c("TeamID2", "Seed2", "Rank2")] <- tourney_2019_first_round[i,c("TeamID1", "Seed1", "Rank1")]
			tourney_2019_first_round[i,"Winning_Team"] <- tourney_2019_first_round[i,"TeamID1"]
		}
		
		gamenum = gamenum + 1
		nxt = 0
	}
	
	nxt = nxt + 1
}

if (tourney_2019_first_round[67,"Seed1"] > tourney_2019_first_round[67,"Seed2"]) {
	tourney_2019_first_round[67,"Winning_Team"] <- tourney_2019_first_round[67,"TeamID2"]
} else if (tourney_2019_first_round[67,"Seed1"] == tourney_2019_first_round[67,"Seed2"]) {
    if (tourney_2019_first_round[67,"Rank1"] > tourney_2019_first_round[67,"Rank2"]) {
      tourney_2019_first_round[67,"Winning_Team"] <- tourney_2019_first_round[67,"TeamID2"]
    }
} else {
	tourney_2019_first_round[67,"Winning_Team"] <- tourney_2019_first_round[67,"TeamID1"]
}

length(which(tourney_2019_outcomes_with_comp_sorted$Winning_Team != tourney_2019_first_round$Winning_Team))