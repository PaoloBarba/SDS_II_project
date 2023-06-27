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

# Fit the generalized linear mixed effects model (GLMM)
model <- glmer(as.factor(goal) ~ (1|player) + scale(shot_distance) + scale(shot_angle) + bodypart +
                 technique + first_touch + prefered_type + inside_18 + scale(time), 
               data = barca_shot, family = binomial)



# Obtain the number of observations, groups, and parameters
n <- nrow(barca_shot)
G <- length(unique(barca_shot$player))
K <- 12  # Number of fixed effects parameters

# Write JAGS model specification to a file
model_code <- "
model {
  for(k in 1:K){ 
	beta[k] ~ dnorm(0,0.001)   # prior for beta
}	

# Statistical (conditional) model
for (j in 1:G) {

  b0[j] ~ dnorm(0, tau.b0) # random intercept for each ranked player
}


  for (i in 1:n) {
    mu[i] <- beta[1] + b0[player[i]] + beta[2] * shot_distance[i] + beta[3] * shot_angle[i] + beta[4] * bodypart[i] + 
              beta[5] * technique[i] + beta[6] * first_touch[i] + beta[7] * prefered_type[i] + 
              beta[8] * inside_18[i] + beta[9] * time[i]
    y[i] ~ dbin(p[i], 1)  # Likelihood function with binomial distribution
    p[i] <- 1 / (1 + exp(-mu[i]))  # Link function (logit)
  }
}"

# Save JAGS model specification to a file
writeLines(model_code, "lmm.model.txt")


# Initialize parameters for JAGS
initials = list( list(b0 = t(ranef(model)$player)[1:G], 
                      beta = as.numeric(fixef(model)))
)

params = c("beta","b0")

# Prepare data for JAGS
data.input <- list(
  n = n,
  K = K,
  G = G,
  y = as.numeric(barca_shot$goal) - 1,  # Convert "goal" to numeric 0/1 values
  shot_distance = barca_shot$shot_distance,
  shot_angle = barca_shot$shot_angle,
  bodypart = as.numeric(as.factor(barca_shot$bodypart)),
  technique = as.numeric(as.factor(barca_shot$technique)),
  first_touch = as.numeric(as.factor(barca_shot$first_touch)),
  prefered_type = as.numeric(as.factor(barca_shot$prefered_type)),
  inside_18 = as.numeric(as.factor(barca_shot$inside_18)),
  time = barca_shot$time,
  player = as.numeric(as.factor(barca_shot$player))
)

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



