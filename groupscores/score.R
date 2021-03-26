library(dplyr)
match <- read.csv("match.csv", header=T)
match1 <- match%>%mutate(goals = home_team_goal-away_team_goal)%>%  ##### goals column is net goals
  mutate(outcome = ifelse(goals==0, 0, goals/abs(goals)))           ##### outcome=1 if home win, =0 if draw, =-1 if away win
matchhome <- select(match1, seq(33,62,3))
matchdraw <- select(match1, seq(34,62,3))
matchaway <- select(match1, seq(35,62,3))
avghome <- transmute(matchhome, oddsh=rowMeans(matchhome[,-11,-12],na.rm=T))
avgdraw <- transmute(matchdraw, oddsd=rowMeans(matchdraw[,-11,-12],na.rm=T))
avgaway <- transmute(matchaway, oddsa=rowMeans(matchaway[,-11,-12],na.rm=T))
match1 <- bind_cols(match1,avghome,avgdraw,avgaway)
match1 <- select(match1,-seq(33,62),-c(9,10))
write.csv(match1,'match1.csv',row.names=F)

#####get dogscore
homedogs <- match1%>%filter(oddsh>=3.5)%>%group_by(home_team_id)%>%summarise(games=n())
awaydogs <- match1%>%filter(oddsa>=3.5)%>%group_by(away_team_id)%>%summarise(games=n())
homedogswin <- match1%>%filter(oddsh>=3.5,outcome==1)%>%group_by(home_team_id)%>%
  summarise(wins=n())%>%left_join(homedogs)%>%filter(!is.na(wins))%>%
  mutate(dogscore = wins/games)
awaydogswin <- match1%>%filter(oddsa>=3.5,outcome==-1)%>%group_by(away_team_id)%>%
  summarise(wins=n())%>%left_join(awaydogs)%>%filter(!is.na(wins))%>%
  mutate(dogscore = wins/games)
write.csv(homedogswin,'homedogswin.csv',row.names=F)
write.csv(awaydogswin,'awaydogswin.csv',row.names=F)

#####get leaguescore
match1 <- read.csv("match1.csv", header=T)
byleagueplayed <- match1%>%group_by(league_id)%>%summarise(games=n())
byleaguewin <- match1%>%filter(outcome!=0, ifelse(outcome==1,oddsh>3.5,oddsa>3.5))%>%
  group_by(league_id)%>%summarise(dogwins=n())%>%left_join(byleagueplayed)%>%
  mutate(leaguescore = dogwins/games)
write.csv(byleaguewin,'byleaguewin.csv',row.names=F)

#####get stagescore
match1 <- read.csv("match1.csv", header=T)
bystage <- match1%>%group_by(stage)%>%summarise(games=n())
bystagewin <- match1%>%filter(outcome!=0, ifelse(outcome==1,oddsh>3.5,oddsa>3.5))%>%
  group_by(stage)%>%summarise(dogwins=n())%>%left_join(bystage)%>%
  mutate(stagescore = dogwins/games)
write.csv(bystagewin,'bystagewin.csv',row.names=F)

######get seasonscore
match1 <- read.csv("match1.csv", header=T)
byseason <- match1%>%group_by(season)%>%summarise(games=n())
byseasonwin <- match1%>%filter(outcome!=0, ifelse(outcome==1,oddsh>3.5,oddsa>3.5))%>%
  group_by(season)%>%summarise(dogwins=n())%>%left_join(byseason)%>%
  mutate(seasonscore = dogwins/games)
write.csv(byseasonwin,'byseasonwin.csv',row.names=F)
