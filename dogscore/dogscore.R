library(dplyr)
match <- read.csv("match.csv", header=T)
match1 <- match%>%mutate(goals = home_team_goal-away_team_goal)%>% #### goals column is net goals
  mutate(outcome = ifelse(goals==0, 0, goals/abs(goals))) ##### outcome=1 if home win, =0 if draw, =-1 if away win
matchhome <- select(match1, seq(33,62,3))
matchdraw <- select(match1, seq(34,62,3))
matchaway <- select(match1, seq(35,62,3))
avghome <- transmute(matchhome, oddsh=rowMeans(matchhome[,-11,-12],na.rm=T)) ##### col oddsh is avg odds for home team
avgdraw <- transmute(matchdraw, oddsd=rowMeans(matchdraw[,-11,-12],na.rm=T))
avgaway <- transmute(matchaway, oddsa=rowMeans(matchaway[,-11,-12],na.rm=T))
match1 <- bind_cols(match1,avghome,avgdraw,avgaway)
match1 <- select(match1,-seq(33,62),-c(9,10))
####match <- match[complete.cases(match), ]
homedogs <- match1%>%filter(oddsh>=3.5)%>%group_by(home_team_id)%>%summarise(games=n())
awaydogs <- match1%>%filter(oddsa>=3.5)%>%group_by(away_team_id)%>%summarise(games=n())
homedogswin <- match1%>%filter(oddsh>=3.5,outcome==1)%>%group_by(home_team_id)%>%
  summarise(wins=n())%>%left_join(homedogs)%>%filter(!is.na(wins))%>%
  mutate(dogscore = wins/games)
awaydogswin <- match1%>%filter(oddsa>=3.5,outcome==-1)%>%group_by(away_team_id)%>%
  summarise(wins=n())%>%left_join(awaydogs)%>%filter(!is.na(wins))%>%
  mutate(dogscore = wins/games)
