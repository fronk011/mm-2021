library(tidyverse)

# For yearly LEague rankings: https://extra.ncaa.org/solutions/rpi/SitePages/Home.aspx

# Read in Regular Season and Tourney data
regular.simple=read_csv("/Users/stuartfronk/Desktop/MUDAC2021/data/MRegularSeasonCompactResults.csv")
regular.detailed=read_csv("/Users/stuartfronk/Desktop/MUDAC2021/data/MRegularSeasonDetailedResults.csv")
tourney.simple=read_csv("/Users/stuartfronk/Desktop/MUDAC2021/data/MNCAATourneyCompactResults.csv")
tourney.detailed=read_csv("/Users/stuartfronk/Desktop/MUDAC2021/data/MNCAATourneyDetailedResults.csv")


# Find and store all the team IDs for the NCAA tourney for each year
ids.allyear = vector("list",17)
for(i in 2003:2019)
{
  # create var that will contain all the teamIDs for one year
  ids.year=c()
  tourney.teams=tourney.simple %>% filter(Season==i) %>% select(WTeamID,LTeamID)
  ids.year=tail(tourney.teams$WTeamID,1)
  ids.year=c(ids.year,unique(tourney.teams$LTeamID))
  ids.allyear[[i-2002]]=ids.year
}

## Create yearly aggregate regular season stats based on all the teamIDs for that season's tourney:
## Create new variable from regular season data for opponents win percentage, 
## total win percentage, home win percentage, away win percentage, ppg, and papg
## Variables being dropped: day number of he game, number of OTs in the game

# find the wins and losses for every team in regular season
team.wins = regular.simple %>% group_by(Season, WTeamID) %>%
  summarize(wins=n())
team.losses = regular.simple %>% group_by(Season, LTeamID) %>%
  summarize(losses=n()) %>%
  rename(WTeamID=LTeamID)

team.stat = full_join(team.wins,team.losses, by=c("Season","WTeamID"))
# find the home and away and neutral wins
team.home.wins = regular.simple %>%
  filter(WLoc=='H') %>% 
  group_by(Season, WLoc, WTeamID) %>%
  summarize(homewins=n())

team.away.wins = regular.simple %>%
  filter(WLoc=='A') %>% 
  group_by(Season, WLoc, WTeamID) %>%
  summarize(awaywins=n())

team.neutral.wins = regular.simple %>%
  filter(WLoc=='N') %>% 
  group_by(Season, WLoc, WTeamID) %>%
  summarize(neutralwins=n())

team.wins.combined = full_join(team.home.wins,team.away.wins, by=c("Season","WTeamID"))
team.wins.combined = full_join(team.wins.combined,team.neutral.wins, by=c("Season","WTeamID"))

# filter out unnecessary columns
team.wins.combined= team.wins.combined %>%
  select(Season, WTeamID,homewins,awaywins,neutralwins)

# combine our win data with our aggregate win and loss data and assign all NAs to 0
team.stat=full_join(team.stat,team.wins.combined,by=c("Season","WTeamID"))
team.stat[is.na(team.stat)] = 0

# do the same process above but for losses
team.home.losses = regular.simple %>%
  filter(WLoc=='A') %>% 
  group_by(Season, WLoc, LTeamID) %>%
  summarize(homelosses=n())

team.away.losses = regular.simple %>%
  filter(WLoc=='H') %>% 
  group_by(Season, WLoc, LTeamID) %>%
  summarize(awaylosses=n())

team.neutral.losses = regular.simple %>%
  filter(WLoc=='N') %>% 
  group_by(Season, WLoc, LTeamID) %>%
  summarize(neutrallosses=n())

team.losses.combined = full_join(team.home.losses,team.away.losses, by=c("Season","LTeamID"))
team.losses.combined = full_join(team.losses.combined,team.neutral.losses, by=c("Season","LTeamID"))

# filter out unnecessary columns
team.losses.combined= team.losses.combined %>%
  select(Season, LTeamID,homelosses,awaylosses,neutrallosses) %>%
  rename(WTeamID=LTeamID)

# combine our win data with our aggregate win and loss data and assign all NAs to 0
team.stat=full_join(team.stat,team.wins.combined,by=c("Season","WTeamID"))
team.stat=full_join(team.stat,team.losses.combined,by=c("Season","WTeamID"))
team.stat[is.na(team.stat)] = 0

# create new variables for total win %, home win %, and away win %
team.stat = team.stat %>% mutate(
  homewinperc=homewins/(homewins+homelosses),
  awaywinperc=awaywins/(awaywins+awaylosses),
  neutralwinperc=neutralwins/(neutralwins+neutrallosses)
  )
team.stat = team.stat %>% 
  select(Season,WTeamID,wins,losses,homewinperc,awaywinperc,neutralwinperc)

# Now we find the ppg and papg
team.points.wins = regular.simple %>%
  group_by(Season,WTeamID) %>%
  summarize(winpoints=sum(WScore),winpointsallowed=sum(LScore))

team.points.losses = regular.simple %>%
  group_by(Season,LTeamID) %>%
  summarize(losspoints=sum(LScore),losspointsallowed=sum(WScore)) %>%
  rename(WTeamID=LTeamID)

team.points.combined = full_join(team.points.wins,team.points.losses,by=c("Season","WTeamID"))
team.points.combined = team.points.combined %>%
  mutate(points=winpoints+losspoints,
         allowedpoints=winpointsallowed+losspointsallowed)
team.points.combined = team.points.combined %>% select(Season,WTeamID,points,allowedpoints)

#Combine our points and team.stat data
team.stat=full_join(team.stat,team.points.combined,by=c("Season","WTeamID"))

# add total win percentage to data
team.stat = team.stat %>%
  mutate(totalwinperc=wins/(wins+losses))

# Add total win percentage for each team to regular.simple
win.percentage=team.stat %>%
  select(Season,WTeamID,totalwinperc)
regular.simple = left_join(regular.simple,win.percentage,by=c("Season","WTeamID")) %>%
  rename(Wtotalwinperc=totalwinperc)
win.percentage=team.stat %>%
  select(Season,WTeamID,totalwinperc) %>%
  rename(LTeamID=WTeamID)
regular.simple = left_join(regular.simple,win.percentage,by=c("Season","LTeamID")) %>%
  rename(Ltotalwinperc=totalwinperc)

# Find average total win percentage of opposing teams for each team
Wopp.win.perc = regular.simple %>%
  group_by(Season,WTeamID) %>%
  summarize(Woppwinperc = sum(Ltotalwinperc))
Lopp.win.perc = regular.simple %>%
  group_by(Season,LTeamID) %>%
  summarize(Loppwinperc = sum(Wtotalwinperc)) %>%
  rename(WTeamID=LTeamID)

opp.win.perc = full_join(Wopp.win.perc,Lopp.win.perc,by=c("Season","WTeamID")) %>%
  mutate(oppwinsum=Woppwinperc+Loppwinperc)
opp.win.perc = opp.win.perc %>%
  select(Season,WTeamID,oppwinsum)
# combine our new dataframe with team.stat and divide our oppwinperc by the total games
team.stat = full_join(team.stat,opp.win.perc,by=c("Season","WTeamID"))
team.stat = team.stat %>%
  mutate(oppwinperc = oppwinsum/(wins+losses)) %>%
  select(-oppwinsum)


## Now we want to combine our detailed and simple data.  First we will filter our 
#team.stat data to 2003 and above.  This can be done earlier I just forgot.
team.stat=team.stat %>%
  filter(Season > 2002)

# detailed averages for each team
Wdetailed.team.stat = regular.detailed %>%
  mutate(WFGperc=WFGM/WFGA,
         WFG3perc=WFGM3/WFGA3,
         WFTperc=WFTM/WFTA)
Wdetailed.team.stat=Wdetailed.team.stat %>%
  group_by(Season,WTeamID) %>%
  summarize(
    WFGperc=sum(WFGperc),
    WFG3perc=sum(WFG3perc),
    WFTperc=sum(WFTperc),
    WOR=sum(WOR),
    WDR=sum(WDR),
    WAst=sum(WAst),
    WStl=sum(WStl),
    WBlk=sum(WBlk),
    WPF=sum(WPF))

Ldetailed.team.stat = regular.detailed %>%
  mutate(LFGperc=LFGM/LFGA,
         LFG3perc=LFGM3/LFGA3,
         LFTperc=LFTM/LFTA)
Ldetailed.team.stat=Ldetailed.team.stat %>%
  group_by(Season,LTeamID) %>%
  summarize(
    LFGperc=sum(LFGperc),
    LFG3perc=sum(LFG3perc),
    LFTperc=sum(LFTperc),
    LOR=sum(LOR),
    LDR=sum(LDR),
    LAst=sum(LAst),
    LStl=sum(LStl),
    LBlk=sum(LBlk),
    LPF=sum(LPF))
# join the two
detailed.team.stat = full_join(Wdetailed.team.stat,Ldetailed.team.stat,by=c("Season","WTeamID"))
detailed.team.stat = detailed.team.stat %>% 
  mutate(FGperc=LFGperc+WFGperc,
         FG3perc=LFG3perc+WFG3perc,
         FTperc=LFTperc+WFTperc,
         OR=LOR+WOR,
         DR=LDR+WDR,
         Ast=LAst+WAst,
         Stl=LStl+WStl,
         Blk=LBlk+WBlk,
         PF=LPF+WPF) %>%
  select(Season,WTeamID,FGperc,FG3perc,FTperc,OR,DR,Ast,Stl,Blk,PF)
  
# combine our detailed data with team.stat.  We will use full_join
team.stat = full_join(team.stat,detailed.team.stat,by=c("Season","WTeamID"))

# adjust everything to per game
team.stat = team.stat %>%
  mutate(FGperc=FGperc/(wins+losses),
         FG3perc=FG3perc/(wins+losses),
         FTperc=FTperc/(wins+losses),
         OR=OR/(wins+losses),
         DR=DR/(wins+losses),
         Ast=Ast/(wins+losses),
         Stl=Stl/(wins+losses),
         Blk=Blk/(wins+losses),
         PF=PF/(wins+losses)
  )
team.stat = team.stat %>%
  rename(FGpercpg = FGperc,
         PG3percpg = FG3perc,
         FTpercpg = FTperc,
         ORpg = OR,
         DRpg = DR,
         ASTpg = Ast,
         Stlpg = Stl,
         Blkpg =Blk,
         PFpg = PF)
# Align our team data with our tourney data so we only have the data for each tourney team
team.stat.tourney = team.stat %>%
  filter(Season == 2003,
         WTeamID %in% ids.allyear[[1]])

for(i in 2004:2019)
{
  team.stat.tourney.year = team.stat %>%
    filter(Season == i,
      WTeamID %in% ids.allyear[[i-2002]])
  team.stat.tourney=rbind(team.stat.tourney,team.stat.tourney.year)
}

# now we combine our tourney team data with each game including that team


# Turn our final data into csv
write.csv(team.stat,"/Users/stuartfronk/Desktop/MUDAC2021/data/team.stat.csv")
