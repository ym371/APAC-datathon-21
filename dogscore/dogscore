library(dplyr)
match <- read.csv("match.csv", header=T)
match <- match%>%mutate(goals = home_team_goal-away_team_goal)%>%
  mutate(outcome = ifelse(goals==0, 0, goals/abs(goals)))
matchhome <- select(match, seq(33,62,3))
matchdraw <- select(match, seq(34,62,3))
matchaway <- select(match, seq(35,62,3))
avghome <- transmute(matchhome, oddsh=rowMeans(matchhome[,-11,-12],na.rm=T))
avgdraw <- transmute(matchdraw, oddsd=rowMeans(matchdraw[,-11,-12],na.rm=T))
avgaway <- transmute(matchaway, oddsa=rowMeans(matchaway[,-11,-12],na.rm=T))
match <- bind_cols(match,avghome,avgdraw,avgaway)
match <- select(match,-seq(33,62),-c(9,10))
match <- match[complete.cases(match), ]
homedogs <- match%>%filter(oddsh>=3.5)%>%group_by(home_team_id)%>%summarise(games=n())
awaydogs <- match%>%filter(oddsa>=3.5)%>%group_by(away_team_id)%>%summarise(games=n())
homedogswin <- match%>%filter(oddsh>=3.5,outcome==1)%>%group_by(home_team_id)%>%
  summarise(wins=n())%>%left_join(homedogs)%>%filter(!is.na(wins))%>%
  mutate(dogscore = wins/games)
awaydogswin <- match%>%filter(oddsa>=3.5,outcome==-1)%>%group_by(away_team_id)%>%
  summarise(wins=n())%>%left_join(awaydogs)%>%filter(!is.na(wins))%>%
  mutate(dogscore = wins/games)
