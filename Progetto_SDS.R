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
model_code <- "
model {
  for (k in 1:K) { 
    beta[k] ~ dnorm(0, 1000)
  }

  tau.e ~ dgamma(.5, .5)
  sigma.e <- 1 / sqrt(tau.e)

  tau.b0 ~ dgamma(.5, .5)
  sigma.b0 <- 1 / sqrt(tau.b0)

  for (j in 1:G) {
    b0[j] ~ dnorm(0, tau.b0)
  }

  for (i in 1:n) {
    mu[i] <- beta[1] + b0[player[i]] + beta[2] * shot_distance[i] +
             beta[3] * shot_angle[i] + beta[4] * bodypartLeft_Foot[i] +
             beta[5] * bodypartOther_Bodypart[i] + beta[6] * bodypartRight_Foot[i] +
             beta[7] * techniqueVolley[i] + beta[8] * first_touchTrue[i] +
             beta[9] * prefered_typeLeft_Foot[i] + beta[10] * prefered_typeRight_Foot[i] +
             beta[11] * inside_18True[i] + beta[12] * time[i]
    p[i] <- 1 / (1 + exp(-mu[i]))
  }
}"

# Save JAGS model specification to a file
writeLines(model_code, "lmm.model.txt")

# Set initial values for JAGS
inits <- list(list(b0 = t(ranef(model)$player)[1:G],
                   beta = as.numeric(fixef(model)),
                   tau.e = 1,
                   tau.b0 = 1)
)

# Define parameters to save
params <- c("beta", "sigma.e", "sigma.b0", "b0")

# Prepare data for JAGS
data.input <-  list(n = n,
                    K = K,
                    G = G,
                    y = as.numeric(as.factor(barca_shot$goal)),
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


# Run JAGS model
true.model.jags <- jags(
  data = data.input,
  inits = inits,
  parameters.to.save = params,
  model.file = "lmm.model.txt",
  DIC = FALSE,
  n.chains = length(inits),
  n.iter = 20000,
  n.burnin = 1100,
  n.thin = 2
)


random_beta <- matrix(NA , nrow = 9450 , ncol = G)
fixed_beta <- matrix(NA, nrow = 9450, ncol = K)
for ( i in 1:groups){
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

ggplot(df_long, aes(x = value, fill = variable)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ variable, ncol = 1, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())







