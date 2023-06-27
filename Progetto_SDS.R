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
    beta[k] ~ dnorm(0, 0.001)
  }

  tau.e ~ dgamma(9, .5)
  sigma.e <- 1 / sqrt(tau.e)

  tau.b0 ~ dgamma(9, .5)
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


b1 = true.model.jags$BUGSoutput$sims.array[, 1, "b0[1]"]
b2 = true.model.jags$BUGSoutput$sims.array[, 1, "b0[2]"]
b3 = true.model.jags$BUGSoutput$sims.array[, 1, "b0[3]"]
B1 = true.model.jags$BUGSoutput$sims.array[, 1, "beta[1]"]
B2 = true.model.jags$BUGSoutput$sims.array[, 1, "beta[2]"]
B3 = true.model.jags$BUGSoutput$sims.array[, 1, "beta[3]"]
B4 = true.model.jags$BUGSoutput$sims.array[, 1, "beta[4]"]
B5 = true.model.jags$BUGSoutput$sims.array[, 1, "beta[5]"]
B6 = true.model.jags$BUGSoutput$sims.array[, 1, "beta[6]"]
B7 = true.model.jags$BUGSoutput$sims.array[, 1, "beta[7]"]
B8 = true.model.jags$BUGSoutput$sims.array[, 1, "beta[8]"]
B9 = true.model.jags$BUGSoutput$sims.array[, 1, "beta[9]"]
B10 = true.model.jags$BUGSoutput$sims.array[, 1, "beta[10]"]
B11 = true.model.jags$BUGSoutput$sims.array[, 1, "beta[11]"]
B12 = true.model.jags$BUGSoutput$sims.array[, 1, "beta[12]"]

sigma.b0 = true.model.jags$BUGSoutput$sims.array[, 1, "sigma.b0"]
sigma.e = true.model.jags$BUGSoutput$sims.array[, 1, "sigma.e"]
P = data.frame( rep(c(paste("beta",sep = "", 1:12 ), bquote("sigma[b0]"), bquote("sigma[e]")), each = 9450), rep(0, 132300))
colnames(P) = c("x", "y")
P[P$x == "beta1", 2 ] = B1
P[P$x == "beta2", 2 ] = B2
P[P$x == "beta3", 2 ] = B3
P[P$x == "beta4", 2 ] = B4
P[P$x == "beta5", 2 ] = B5
P[P$x == "beta6", 2 ] = B6
P[P$x == "beta7", 2 ] = B7
P[P$x == "beta8", 2 ] = B8
P[P$x == "beta9", 2 ] = B9
P[P$x == "beta10", 2 ] = B10
P[P$x == "beta11", 2 ] = B11
P[P$x == "beta12", 2 ] = B12

P[P$x == "sigma[b0]", 2 ] = sigma.b0
P[P$x == "sigma[e]", 2 ] = sigma.e
mean(b1)
mean(b2)
mean(b3)
mean(B1)
mean(B2)
mean(B3)
mean(B4)
mean(B5)
mean(B6)
mean(B7)
mean(B8)
mean(B9)
mean(B10)
mean(B11)
mean(B12)

str(true.model.jags)
sqrt(var(b1)/effectiveSize(b1))
sqrt(var(b2)/effectiveSize(b2))
sqrt(var(b3)/effectiveSize(b3))
sqrt(var(B1)/effectiveSize(B1))
sqrt(var(B2)/effectiveSize(B2))
sqrt(var(B3)/effectiveSize(B3))
sqrt(var(B4)/effectiveSize(B4))
sqrt(var(B5)/effectiveSize(B5))
sqrt(var(B6)/effectiveSize(B6))
sqrt(var(B7)/effectiveSize(B7))
sqrt(var(B8)/effectiveSize(B8))
sqrt(var(B9)/effectiveSize(B9))
sqrt(var(B10)/effectiveSize(B10))
sqrt(var(B11)/effectiveSize(B11))
sqrt(var(B12)/effectiveSize(B12))
mean(sigma.b0)
mean(sigma.e)
sqrt(var(sigma.b0)/effectiveSize(sigma.b0))
sqrt(var(sigma.e)/effectiveSize(sigma.e))


par(mfrow = c(2, 2))
plot(B1, type = "b", col = 'red', ylab = bquote(beta[1]), xlab = "iterations");plot(1:length(B1), cumsum(B1)/(1:length(B1)), col = "red", type = "b", xlab = "iterations", ylab = "mean");plot(B2, type = "b", col = 'blue', ylab = bquote(beta[2]), xlab = "iterations");plot(1:length(B2), cumsum(B2)/(1:length(B2)), col = "blue", type = "b", xlab = "iterations", ylab = "mean")
