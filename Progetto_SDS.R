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


library("lme4")
model = glmer(as.factor(goal) ~ (1|player) + shot_distance + shot_angle + bodypart +
        technique + first_touch + prefered_type + inside_18 + time , data = barca_shot, 
        family = 'binomial')




round(fixef(model), 4)
ranef(model)
data.frame(VarCorr(model))



cat("model {
for(k in 1:K){ # prior for beta
	beta[k] ~ dnorm(0,0.001)
}	

tau.e ~ dgamma(0.001,0.001) # prior for the within group variance
sigma.e <- 1/sqrt(tau.e)

tau.b0 ~ dgamma(0.001,0.001) # prior variance for the between group variance
sigma.b0 <- 1/sqrt(tau.b0)

# Statistical (conditional) model
for (j in 1:G) {

  b0[j] ~ dnorm(0, tau.b0) # random intercept for each ranked player
}

for(i in 1:n){ # likelihood function !!
	mu[i] <- beta[1] + b0[barca_shot$player[i]] + beta[2] * distance[i] + beta[3] * angle[i] + beta[4] * body_part[i] + 
	         beta[5] * tecnique[i] + beta[6] * first_touch[i] + beta[7] * preferred_type[i] + beta[8] * inside_18[i] +  beta[9] time[i]
	y[i] ~ dnorm(mu[i], tau.e)
}

}", file = "lmm.model.txt", fill = TRUE)

inits = list( list(b0 = t(ranef(model)$rank)[1:3], 
                   beta = as.numeric(fixef(model)),
                   tau.e = 1/data.frame(VarCorr(model))[2, "sdcor"]^2,
                   tau.b0 = 1/data.frame(VarCorr(model))[1, "sdcor"]^2)
)

params = c("beta","sigma.e","sigma.b0", "b0")



data.input = list(n = n, K = K, G = groups , 
                  y = barca_shot$goal , distance = barca_shot$shot_distance,
                  angle = barca_shot$shot_angle, body_part = barca_shot$bodypart,
                  tecnique = barca_shot$technique, first_touch = barca_shot$first_touch,
                  preferred_type = barca_shot$prefered_type, inside_18 = barca_shot$inside_18,
                  time = barca_shot$time)


