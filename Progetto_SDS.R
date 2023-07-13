rm(list = ls())
par(mfrow = c(1,1))
load('shots.RData')


1 - pnorm(1.66)
# EDA -------------------------------------------------------------------------------------------------------------------------------------------


# Dataset Description : 
# The dataset is composed by 12843 observations (that reppresent an time momenti in a football match where the player has shoot).
# The dataset stored shots of 12843 of La Liga
barca_shot <- shots[shots$team == "Barcelona",]
barca_shot$goal <- as.integer(factor(barca_shot$goal))
distance_class <- factor(cut(barca_shot$shot_distance , breaks = c(0,10,20,30,40)))
plot(distance_class[ barca_shot$goal == 1])

sd(y)
library(igraph)
sample_dirichlet(2,2)


qnorm(.98, mean = 30 / 100 , sd = sqrt((30/100 * 70 / 100 ) / 300)) 
?qnorm

# Model -----------------------------------------------------------------------------------------------------------------------------------------
library(lme4)
library(R2jags)

??ddirichlet
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


preds <- ifelse(predict(model , barca_shot , type = 'response') <= .5 , 1,2)
sum(preds == barca_shot$goal ) / length(barca_shot$goal)


ranef(model)




linear_model <- glm(as.factor(goal) ~ scale(shot_distance) + scale(shot_angle) + bodypart + technique + first_touch + prefered_type + inside_18 + scale(time), data = barca_shot, family = binomial)




# Obtain the number of observations, groups, and parameters
n <- nrow(barca_shot)
G <- length(unique(barca_shot$player))
K <- length(fixef(model))

# Write JAGS model specification to a file
cat("model { 
  for(k in 1:K){ # prior for beta
	beta[k] ~ dnorm(0,0.001)
}	


  for (i in 1:n) {
    mu[i] <- beta[1] + beta[2] * shot_distance[i] +
             beta[3] * shot_angle[i] + beta[4] * bodypartLeft_Foot[i] +
             beta[5] * bodypartOther_Bodypart[i] + beta[6] * bodypartRight_Foot[i] +
             beta[7] * techniqueVolley[i] + beta[8] * first_touchTrue[i] +
             beta[9] * prefered_typeLeft_Foot[i] + beta[10] * prefered_typeRight_Foot[i] +
             beta[11] * inside_18True[i] + beta[12] * time[i] 
    p[i] <- 1 / (1 + exp(-mu[i]))
    y[i] ~ dbern(p[i])
   
  }
}" , file = "paolo_2.txt" , fill = TRUE ) 






# Set initial values for JAgs
inits <- function(){list("b0" = t(ranef(model)$player)[1:G],
                         "beta" = as.numeric(fixef(model)),
                         "tau.b0" = 3)}

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
                    #player = player
)



load("model_jags.RData")

linear_model <- jags(data = data.input, inits = inits,
                     parameters.to.save = params , model.file = "paolo_2.txt" )

save(linear_model , file = "jags_linear_model.RData")


pnorm(0.09)


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

df_long$variable
df_long <- reshape2::melt(fixed_beta)

ggplot(df_long, aes(x = value, fill = variable)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ variable, ncol = 1, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())


plot(1:length(fixed_beta$Beta.2),cumsum(fixed_beta$Beta.2 ) / (1:length(fixed_beta$Beta.2)) )

install.packages("LaplacesDemon")
LaplacesDemon::rdirichlet(1,rep(1,6))	


a <- (85 / 150) + 1.96 * sqrt(((85 / 150)*(65 / 150))/ 150 )


((85 / 150) - 0.6) / sqrt(((0.6 * 0.4) / 150))

(170 * (1/2) + 220 *(2.5 / 2) + 290 * (3.5/2) + 120 * (5 / 2) )/ (170 + 220 + 290 + 120)

?rep
x <- c(rep((18 + 24 )/2,200) , rep((34 + 24 )/2,200) , rep((34 + 50 )/2,200 ), rep((70 + 50 )/2,200))
mean(x)       
sd(x)
y <- c(rep((0 + 1 )/2,170) , rep((1 + 1.5 )/2,220) , rep((1.5 + 2 )/2,290 ), rep((2 + 3 )/2,120))
mean(y) - 0.038520*mean(x)

(cov(x,y) / sd(x) * sd(y))^2
sd(x) * sd(y)
0.0043894 / 0.0273196
mod <- lm(y~x)
mod
length(x)
summary(mod)
0.0006702/ 0.2796 
qt(0.05, 798)


m <- c(rep(5/2 , 32), rep(7.5,16) , rep(25/2 , 6) , 25) 
var(m)
