rm(list = ls())
load('shots.RData')



# EDA -------------------------------------------------------------------------------------------------------------------------------------------


# Dataset Description : 
# The dataset is composed by 12843 observations (that reppresent an time momenti in a football match where the player has shoot).
# The dataset stored shots of 12843 of La Liga
barplot(prop.table(table(shots$goal)), col = c('red','green'))
length(unique(shots$player))
length(shots$player)


barca_shot <- shots[shots$team == "Barcelona",]
n <- nrow(barca_shot)
groups <- length(unique(barca_shot$player)) # 194 groups
K <- ncol(barca_shot)
colnames(barca_shot)





# Model -----------------------------------------------------------------------------------------------------------------------------------------
library(lme4)
library(R2jags)


attach(barca_shot)
player = as.numeric(as.factor(barca_shot$player))
X = model.matrix(~ 1 + shot_distance  + shot_angle + bodypart + technique + first_touch + prefered_type + inside_18 + time + player)
detach(barca_shot)
X = as.data.frame(X)
colnames(X)


# Fit the generalized linear mixed effects model (GLMM)
model <- glmer(as.factor(barca_shot$goal) ~ (1|player) + scale(shot_distance) + scale(shot_angle) + bodypart +
                 technique + first_touch + prefered_type + inside_18 + scale(time), 
               data = X, family = binomial)


# Obtain the number of observations, groups, and parameters
n <- nrow(barca_shot)
G <- length(unique(barca_shot$player))
K <- length(fixef(model))

# Write JAGS model specification to a file
model_code <- "
model {
  for(k in 1:K){ 
	beta[k] ~ dnorm(0,0.001)  
}	

  tau.e ~ dgamma(9,.5)
  sigma.e <- 1/sqrt(tau.e)

  tau.b0 ~ dgamma(9,0.001)
  sigma.b0 <- 1/sqrt(tau.b0)

  for (j in 1:G) {
   b0[j] ~ dnorm(0, tau.b0)  
  }


  for (i in 1:n) {
    mu[i] <- beta[1] + b0[player[i]] + beta[2] * shot_distance[i] + 
    beta[3] * shot_angle[i] + beta[4] * bodypartLeft_Foot[i] + beta[5] *  bodypartOther_Bodypart[i] + 
    beta[6] * bodypartRight_Foot[i] + beta[7] * techniqueVolley[i] + beta[8] * first_touchTrue[i] +
    beta[9] * prefered_typeLeft_Foot[i] + beta[10] * prefered_typeRight_Foot[i] +
    beta[11] * inside_18True[i] + beta[12] * time[i]
    p[i] <- 1 / (1 + exp(-mu[i]))  
    y[i] ~ dbin(p[i], 1)           
  }
}"

# Save JAGS model specification to a file
writeLines(model_code, "lmm.model.txt")


inits = list( list(b0 = t(ranef(model)$player)[1:G], 
                   beta = as.numeric(fixef(model)),
                   tau.e = 1,
                   tau.b0 = 1)
)


params = c("beta","sigma.e","sigma.b0", "b0")

# Prepare data for JAGS
data.input <-  list(n = n,
                    K = K, 
                    G = G, 
                    y = barca_shot$goal,
                    shot_distance = X$shot_distance,
                    shot_angle = X$shot_angle,
                    bodypartLeft_Foot = X$`bodypartLeft Foot`,
                    bodypartOther_Bodypart = X$`bodypartOther Bodypart`,
                    bodypartRight_Foot = X$`bodypartRight Foot`,
                    techniqueVolley = X$techniqueVolley,
                    first_touchTrue = X$first_touchTrue,
                    prefered_typeLeft_Foot = X$`prefered_typeLeft Foot`,
                    prefered_typeRight_Foot = X$`prefered_typeRight Foot`,
                    inside_18True = X$inside_18True,
                    time = X$time,
                    player = X$player
                  )
    
                  
rjags::load.module('dic')


# Run JAGS model
true.model.jags <- jags(
  data = data.input,
  inits = inits,
  parameters.to.save = params,
  model.file = "lmm.model.txt",
  DIC = TRUE,
  n.chains = 1,
  n.iter = 20000,
  n.burnin = 1100,
  n.thin = 2
)


