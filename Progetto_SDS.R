rm(list = ls())
par(mfrow = c(1,1))
load('shots.RData')



# EDA -------------------------------------------------------------------------------------------------------------------------------------------


# Dataset Description : 
# The dataset is composed by 12843 observations (that reppresent an time momenti in a football match where the player has shoot).
# The dataset stored shots of 12843 of La Liga
barplot(prop.table(table(shots$goal)), col = c('red','green'))
length(unique(shots$player))
length(shots$player)
barca_shot <- shots[shots$team == "Barcelona",]
barca_shot$goal <- as.integer(factor(barca_shot$goal))
distance_class <- factor(cut(barca_shot$shot_distance , breaks = c(0,10,20,30,40)))
plot(distance_class[ barca_shot$goal == 1])






# Model -----------------------------------------------------------------------------------------------------------------------------------------
library(lme4)
library(R2jags)



# Convert player variable to numeric factor
player <- as.numeric(as.factor(barca_shot$player))

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

# Obtain the number of observations, groups, and parameters
n <- nrow(barca_shot)
G <- length(unique(barca_shot$player))
K <- length(fixef(model))

# Write JAGS model specification to a file
cat("model { 
  for(k in 1:K){ # prior for beta
	beta[k] ~ dnorm(0,0.001)
}	

  tau.b0 ~ dgamma(0.1,0.1) # prior variance for the between group variance
  sigma.b0 <- 1/sqrt(tau.b0)

   
  # Statistical (conditional) model
  for (j in 1:G) {

     b0[j] ~ dnorm(0, 0.01) # random intercept for each ranked player
}

  for (i in 1:n) {
    mu[i] <- beta[1] + b0[player[i]] + beta[2] * shot_distance[i] +
             beta[3] * shot_angle[i] + beta[4] * bodypartLeft_Foot[i] +
             beta[5] * bodypartOther_Bodypart[i] + beta[6] * bodypartRight_Foot[i] +
             beta[7] * techniqueVolley[i] + beta[8] * first_touchTrue[i] +
             beta[9] * prefered_typeLeft_Foot[i] + beta[10] * prefered_typeRight_Foot[i] +
             beta[11] * inside_18True[i] + beta[12] * time[i]
    p[i] <- 1 / (1 + exp(-mu[i]))
    y[i] ~ dbern(p[i])
   
  }
}" , file = "paolo.txt" , fill = TRUE ) 



# Set initial values for JAgs
inits <- function(){list("b0" = t(ranef(model)$player)[1:G],
                         "beta" = as.numeric(fixef(model)),
                         "tau.b0" = 3)}

# Define parameters to save
params <- c("beta", "sigma.b0", "b0")

length(player)
# Prepare data for JAGS
data.input <-  list(n = n,
                    K = K,
                    G = G,
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
                    time = X$time,
                    player = player
)

load("model_jags.RData")






random_beta <- matrix(NA , nrow = 9450 , ncol = G)
fixed_beta <- matrix(NA, nrow = 9450, ncol = K)
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
library(ggplot2)
library(tidyverse)


# Assuming df is your dataframe with multiple distributions

# Reshape the dataframe into a long format
# Create the density plot with facet_wrap


df_long <- reshape2::melt(fixed_beta)

ggplot(df_long, aes(x = value, fill = Var2)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ Var2, ncol = 1, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())


plot(cumsum(random_beta$X14) / (1:9450) )