tourney.simple=read_csv("/Users/stuartfronk/Desktop/MUDAC2021/data/MNCAATourneyCompactResults.csv")
# lets remove the unwanted columns from tourney.simple
tourney.simple=tourney.simple %>%
  filter(Season > 2002) %>%
  select(Season,WTeamID,LTeamID)

# lets remove wins and losses from team_stat_tourney
team_stat_tourney = team_stat_tourney %>%
  select(-c(wins,losses))
# lets replace all NA with the mean value
team_stat_tourney$neutralwinperc[is.na(team_stat_tourney$neutralwinperc)] = mean(team_stat_tourney$neutralwinperc,na.rm=T)
team_stat_tourney$FTpercpg[is.na(team_stat_tourney$FTpercpg)] = mean(team_stat_tourney$FTpercpg,na.rm=T)

# lets drop the two rows with missing data for every advanced stat. no NAs left
team_stat_tourney = team_stat_tourney %>% drop_na()

# Now we need to convert tourney.simple into a mergeable format with a binary variable for win or loss
# I will flip half of WTeamID to the LTeamID place and vice versa
# we will do this using a random sample
set.seed(1111)
rows = sample(nrow(tourney.simple))
tourney.simple = tourney.simple[rows,]

# now divide the sample in half
Wtourney.simple = tourney.simple[1:557,] %>% mutate(Team1win = 1) %>%
  rename(Team1 = WTeamID,Team2 = LTeamID)
Ltourney.simple = tourney.simple[558:1115,] %>% mutate(Team1win = 0) %>%
  select(Season,LTeamID,WTeamID,Team1win) %>%
  rename(Team1 = LTeamID,Team2 = WTeamID)

# now merge them together again into tourney.simple
tourney.simple = rbind(Wtourney.simple,Ltourney.simple)

# now we add our team_stat_tourney stats: first is Team 1 stats
team_stat_tourney = team_stat_tourney %>% rename(Team1=TeamID)
tourney.simple = left_join(tourney.simple, team_stat_tourney,by=c("Season","Team1"))

# next is Team2 stats
team_stat_tourney = team_stat_tourney %>% rename(Team2=Team1)
tourney.simple = left_join(tourney.simple, team_stat_tourney,by=c("Season","Team2"))
tourney.simple = tourney.simple %>% drop_na()

# now find the difference between the two similar stats
tourney.simple = tourney.simple %>%
  mutate(
    homewinpercdiff = homewinperc.x-homewinperc.y,
    awaywinpercdiff = awaywinperc.x-awaywinperc.y,
    neutralwinpercdiff = neutralwinperc.x-neutralwinperc.y,
    totalwinpercdiff = totalwinperc.x-totalwinperc.y,
    oppwinpercdiff = oppwinperc.x-oppwinperc.y,
    FGpercdiff = FGpercpg.x-FGpercpg.y,
    FG3percdiff = FG3percpg.x-FG3percpg.y,
    ORpgdiff = ORpg.x-ORpg.y,
    DRpgdiff = DRpg.x-DRpg.y,
    ASTpgdiff = ASTpg.x-ASTpg.y,
    STLpgdiff = Stlpg.x-Stlpg.y,
    BLKpgdiff = Blkpg.x-Blkpg.y,
    PFpgdiff = PFpg.x-PFpg.y,
    hotdiff = hot.x-hot.y,
    ppgdiff = ppg.x-ppg.y,
    papgdiff = papg.x-papg.y,
    Seeddiff = Seed.x-Seed.y,) %>%
  select(homewinpercdiff,awaywinpercdiff,neutralwinpercdiff,
         totalwinpercdiff,oppwinpercdiff,FGpercdiff,FG3percdiff,
         ORpgdiff,DRpgdiff,ASTpgdiff,STLpgdiff,BLKpgdiff,
         PFpgdiff,hotdiff,ppgdiff,papgdiff,Seeddiff,Team1win)







# now we will create a val and train set for this method
set.seed(1111)
train = sample(nrow(tourney.simple),round(0.8*nrow(tourney.simple),1))
tourney.train = tourney.simple[train,]
tourney.val = tourney.simple[-train,]

# This is our first model using all of our variables
tourney.model = glm(Team1win~., family = "binomial", data = tourney.train)
summary(tourney.model)

tourney.probs = predict(tourney.model,tourney.val,type="response")
tourney.pred = ifelse(tourney.probs>0.5,1,0)
#mean((tourney.val$Team1win - tourney.pred)^2)

attach(tourney.val)
table(tourney.pred,Team1win)
mean(tourney.pred==Team1win)

# now we will tune some of the hyperparameters to prevent overfitting
# aic measures
step(tourney.model,direction = "backward")

step(tourney.model,scope=list(lower=~1,upper=~.),direction="forward")

step(tourney.model,scope=list(lower=~1,upper=~.),direction="both")

# bic measures
step(tourney.model,direction = "backward",k=log(nrow(tourney.train)))

step(tourney.model,scope=list(lower=~1,upper=~.),direction="forward",k=log(nrow(tourney.train)))

step(tourney.model,scope=list(lower=~1,upper=~.),direction="both",k=log(nrow(tourney.train)))

# model 2 using aic selected model
tourney.model2 = glm(formula = Team1win ~ oppwinpercdiff + DRpgdiff + PFpgdiff + 
      hotdiff + ppgdiff + papgdiff + Seeddiff, family = "binomial", 
    data = tourney.train)

tourney.probs2 = predict(tourney.model2,tourney.val,type="response")
tourney.pred2 = ifelse(tourney.probs2>0.5,1,0)
#mean((tourney.val$Team1win - tourney.pred)^2)

attach(tourney.val)
table(tourney.pred2,Team1win)
mean(tourney.pred2==Team1win)

# model 3 using aic selected stuff
tourney.model3 = glm(formula = Team1win ~ oppwinpercdiff + DRpgdiff + 
                      ppgdiff + papgdiff, family = "binomial", 
                     data = tourney.train)

tourney.probs3 = predict(tourney.model3,tourney.val,type="response")
tourney.pred3 = ifelse(tourney.probs3>0.5,1,0)
#mean((tourney.val$Team1win - tourney.pred)^2)

attach(tourney.val)
table(tourney.pred3,Team1win)
mean(tourney.pred3==Team1win)

# overdispersion test
phihat = sum(residuals(tourney.model3,type="pearson")^2)/df.residual(tourney.model3)
phihat









#Now we build and test a model without our difference variables
tourney.model4 = glm(formula = Team1win ~ ., family = "binomial", 
                     data = tourney.train)
summary(tourney.model4)

tourney.probs4 = predict(tourney.model4,tourney.val,type="response")
tourney.pred4 = ifelse(tourney.probs4>0.5,1,0)
#mean((tourney.val$Team1win - tourney.pred)^2)

attach(tourney.val)
table(tourney.pred4,Team1win)
mean(tourney.pred4==Team1win)


# aic measures
step(tourney.model4,direction = "backward")

step(tourney.model4,scope=list(lower=~1,upper=~.),direction="forward")

step(tourney.model4,scope=list(lower=~1,upper=~.),direction="both")

# bic measures
step(tourney.model4,direction = "backward",k=log(nrow(tourney.train)))

step(tourney.model4,scope=list(lower=~1,upper=~.),direction="forward",k=log(nrow(tourney.train)))

step(tourney.model4,scope=list(lower=~1,upper=~.),direction="both",k=log(nrow(tourney.train)))


# none of these models are better than our previous
write.csv(tourney.simple,"/Users/stuartfronk/Desktop/MUDAC2021/data/model_data.csv")
