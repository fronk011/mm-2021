library(tidyverse)

# now we will create a val and train set for this method.
# This we did initially before using our final training set.
set.seed(1111)
train = sample(nrow(tourney.simple),round(0.8*nrow(tourney.simple),1))
tourney.train = tourney.simple[train,]
tourney.val = tourney.simple[-train,]

# This is our first model using all of our variables
tourney.model = glm(Team1win~., family = "binomial", data = tourney.train)
summary(tourney.model)

tourney.probs = predict(tourney.model,tourney.val,type="response")
tourney.pred = ifelse(tourney.probs>0.5,1,0)
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

attach(tourney.val)
table(tourney.pred3,Team1win)
mean(tourney.pred3==Team1win)

# overdispersion test
phihat = sum(residuals(tourney.model,type="pearson")^2)/df.residual(tourney.model)
phihat
# overdispersion parameter is around 1, which means we dont have to change it

# As all of our models lose around at least 1.5% in accuracy, which we think is 
# significant, we will stick with our initial model
