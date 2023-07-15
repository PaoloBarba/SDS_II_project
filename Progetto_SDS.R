rm(list = ls())
par(mfrow = c(1,1))
load('Rdata/shots.RData')
library(lme4)
library(R2jags)
# EDA -------------------------------------------------------------------------------------------------------------------------------------------
# Dataset Description : 
# The dataset is composed by 12843 observations (that reppresent an time momenti in a football match where the player has shoot).
# The dataset stored shots of 12843 of La Liga
barca_shot <- shots[shots$team == "Barcelona",]
barca_shot$goal <- as.integer(factor(barca_shot$goal))
distance_class <- factor(cut(barca_shot$shot_distance , breaks = c(0,10,20,30,40)))
plot(distance_class[ barca_shot$goal == 1])
# Model -----------------------------------------------------------------------------------------------------------------------------------------
# Convert player variable to numeric factor
idx <- sample(dim(barca_shot)[1], 1000, replace = TRUE)
barca_shot <- barca_shot[idx,]
player <- as.numeric(as.factor(barca_shot$player))

factor_playe <- as.factor(barca_shot$player)

levels(factor_playe <- as.factor(barca_shot$player))
# Create design matrix X
attach(barca_shot)
X <- model.matrix(~ 1 + shot_distance  + shot_angle + bodypart + technique + first_touch +
                    prefered_type + inside_18 + time + player)
detach(barca_shot)
X <- data.frame(X)
colnames(X)
# Fit the generalized linear mixed effects model (GLMM)
model <- glmer(as.factor(goal) ~ (1|player) + scale(shot_distance) + scale(shot_angle) + bodypart +
                 technique + first_touch + prefered_type + inside_18 + scale(time),
               data = barca_shot, family = binomial)


preds <- ifelse(predict(model , barca_shot , type = 'response') <= .5 , 1,2)
sum(preds == barca_shot$goal ) / length(barca_shot$goal)




linear_model <- glm(as.factor(goal) ~ scale(shot_distance) + scale(shot_angle) + bodypart + technique + first_touch + prefered_type + inside_18 + scale(time), data = barca_shot, family = binomial)




# Obtain the number of observations, groups, and parameters
n <- nrow(barca_shot)
G <- length(unique(barca_shot$player))
K <- length(fixef(model))
# Set initial values for JAgs
inits <- list("beta" = list(beta =  as.numeric(linear_model$coefficients)))

# Define parameters to save
params <- c("beta")

length(player)
# Prepare data for JAGS
data.input <-  list(n = n,
                    K = K,
                    #G = G,
                    y = barca_shot$goal - 1,
                    #intercept = X$X.Intercept,
                    shot_distance = X$shot_distance,
                    shot_angle = X$shot_angle,
                    bodypartLeft_Foot = X$bodypartLeft.Foot,
                    bodypartOther_Bodypart = X$bodypartOther.Bodypart,
                    bodypartRight_Foot = X$bodypartRight.Foot,
                    techniqueVolley = X$techniqueVolley,
                    first_touchTrue = X$first_touchTrue,
                    prefered_typeLeft_Foot = X$prefered_typeLeft.Foot,
                    prefered_typeRight_Foot = X$prefered_typeRight.Foot,
                    inside_18True = X$inside_18True,
                    time = X$time
                   # player = player
)



#load("Rdata/model_jags.RData")
#load("Rdata/jags_linear_model.RData")

true.model.jags <- jags(
  data = data.input,
  inits = inits,
  parameters.to.save = params,
  model.file = "txt_data/GLM.txt",
  DIC = TRUE,
  n.chains = length(inits),
  n.iter = 10000,
  n.burnin = 1100,
  n.thin = 2
)

save(true.model.jags , file = "Rdata/glm.jags_model.RData" )
random_beta <- matrix(NA , nrow = 4450 , ncol = G)
fixed_beta <- matrix(NA, nrow = 4450, ncol = K)
for ( i in 1:G){
  random_beta[,i] <- true.model.jags$BUGSoutput$sims.array[, 1, paste0("b0[",i,']')]
}
for (i in 1:K){
  fixed_beta[,i] <- true.model.jags$BUGSoutput$sims.array[, 1, paste0("beta[",i,']')]
}
fixed_beta <- data.frame(fixed_beta)
colnames(fixed_beta) <- c("Beta 1","Beta 2","Beta 3","Beta 4",
                          "Beta 5","Beta 6","Beta 7","Beta 8",
                          "Beta 9","Beta 10","Beta 11","Beta 12")
random_beta <- data.frame(random_beta)



#####
rm(list = ls())
load("Rdata/glmm.jags_model.RData")
library(ggmcmc)

ggmcmc(ggs(as.mcmc(true.model.jags)))
