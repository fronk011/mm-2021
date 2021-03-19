# here we will simulate our actual model on the NCAA tourney
region_sim = function(tourney.val,model.fit)
{
  #We will import the 16 teams from the desired region and year.  
  #We will then simulate the bracket for this region.
  seeds = list(c(1,16),c(8,9),c(5,12),c(4,13),c(6,11),c(3,14),c(7,10),c(2,15))
  winners = c()
  teams = c()
  #points = 0
  for(i in 1:length(seeds))
  {
    winners[i] = format_data_and_sim(tourney.val,seeds,i,model.fit)
  }
  teams=winners
  seeds = list(c(winners[1],winners[2]),c(winners[3],winners[4]),
               c(winners[5],winners[6]),c(winners[7],winners[8]))
  for(i in 1:length(seeds))
  {
    winners[i] = format_data_and_sim(tourney.val,seeds,i,model.fit)
  }
  teams=cbind(teams,winners)
  seeds = list(c(winners[1],winners[2]),c(winners[3],winners[4]))
  for(i in 1:length(seeds))
  {
    winners[i] = format_data_and_sim(tourney.val,seeds,i,model.fit)
  }
  teams=cbind(teams,winners)
  seeds = list(c(winners[1],winners[2]))
  winners = format_data_and_sim(tourney.val,seeds,1,model.fit)
  teams=cbind(teams,winners)
  return(teams)
}

final_four = function(tourney.val,model.fit,seeds)
{
  # This function simulates the final four
  # it takes our data to be predicted, our model, and our seeds
  winners=c()
  teams=c()
  j=1
  for(i in 1:length(seeds))
  {
    winners[i] = format_data_and_sim_final_four(tourney.val,seeds,i,j,model.fit)
    j=j+2
  }
  teams=cbind(teams,winners)
  seeds = list(c(winners[1],winners[2]))
  tourney.val = filter(tourney.val,Region %in% seeds[[1]])
  print(tourney.val)
  winners = format_data_and_sim_final_four(tourney.val,seeds,1,1,model.fit)
  teams=cbind(teams,winners)
  return(teams)
}

format_data_and_sim = function(team_stat_tourney,seeds,i,model.fit)
{
  # runs a simulated game between two teams
  # takes data as an input and manipulates it so our model can process it.
  winners=c()
  # get data for both teams
  team1 = filter(team_stat_tourney,Seed==seeds[[i]][1])
  team2 = filter(team_stat_tourney,Seed==seeds[[i]][2])
  print(team1$Team2)
  print(team2$Team2)
  # combine the teams data
  tourney.game = left_join(team1,team2,by=c("Season"))
  # make our statistics model ready
  tourney.game = tourney.game %>%
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
      Seeddiff = Seed.x-Seed.y) %>%
    select(homewinpercdiff,awaywinpercdiff,neutralwinpercdiff,
           totalwinpercdiff,oppwinpercdiff,FGpercdiff,FG3percdiff,
           ORpgdiff,DRpgdiff,ASTpgdiff,STLpgdiff,BLKpgdiff,
           PFpgdiff,hotdiff,ppgdiff,papgdiff,Seeddiff,Team1win)
  # predict the game
  tourney.probs = predict(model.fit,tourney.game,type="response")
  print(tourney.probs)
  # put the winner in the winners bracket
  if(tourney.probs>=0.5)
  {
    winners = seeds[[i]][1]
  }
  else{winners = seeds[[i]][2]}
  return(winners)
}


format_data_and_sim_final_four = function(team_stat_tourney,seeds,i,j,model.fit)
{
  # predicts a final four game
  winners=c()
  # get data for both teams
  team1 = team_stat_tourney[j,]
  team2 = team_stat_tourney[j+1,]
  # combine the teams data
  tourney.game = left_join(team1,team2,by=c("Season"))
  # make our statistics model ready
  tourney.game = tourney.game %>%
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
      Seeddiff = Seed.x-Seed.y) %>%
    select(homewinpercdiff,awaywinpercdiff,neutralwinpercdiff,
           totalwinpercdiff,oppwinpercdiff,FGpercdiff,FG3percdiff,
           ORpgdiff,DRpgdiff,ASTpgdiff,STLpgdiff,BLKpgdiff,
           PFpgdiff,hotdiff,ppgdiff,papgdiff,Seeddiff,Team1win)
  # predict the game
  tourney.probs = predict(model.fit,tourney.game,type="response")
  
  # put the winner in the winners bracket
  if(tourney.probs>=0.5)
  {
    winners = seeds[[i]][1]
  }
  else{winners = seeds[[i]][2]
  }
  return(winners)
}

# now we have our actual code that simulates the tournament.  
#We will write down the picks manually from the output
# This code requires some changing and slight manipulation for each different region
bracket.region = team_stat_tourney %>%
  filter(Season==2021,Region=='Y') 
# remove first four teams from region
bracket.region = bracket.region[-c(1,18),]
# run function
region_sim(bracket.region,tourney.model)

#final four
team.w = team_stat_tourney %>%
  filter(Season==2021,Region=="W",Seed==2)
team.x = team_stat_tourney %>%
  filter(Season==2021,Region=="X",Seed==3)
team.y = team_stat_tourney %>%
  filter(Season==2021,Region=="Y",Seed==2)
team.z = team_stat_tourney %>%
  filter(Season==2021,Region=="Z",Seed==1)

final.bracket = rbind(team.w,team.x,team.y,team.z)

final_four(final.bracket,tourney.model,seeds = list(c("W","X"),
                                                    c("Y","Z")))