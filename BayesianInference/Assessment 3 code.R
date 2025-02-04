

wines <- read.csv("D:\\General\\Bayesian Inference\\winequality-red.csv")

wines <- na.omit(wines)

wines$Good <- ifelse(wines$quality >= 6.5 , 1 , 0)

obj_log <- glm(wines$Good ~ . - quality, family = binomial(link="logit"), data=wines)

summary(obj_log)

#~ significant coefficients

lr_coef <- as.numeric(coefficients(obj_log))

range.tsd <- seq(from=min(wines$total.sulfur.dioxide), to=max(wines$total.sulfur.dioxide), by=1)

b0 <- lr_coef[1]
bx1 <- lr_coef[2]*colMeans(wines)[1]
bx2 <- lr_coef[3]*colMeans(wines)[2]
bx3 <- lr_coef[4]*colMeans(wines)[3]
bx4 <- lr_coef[5]*colMeans(wines)[4]
bx5 <- lr_coef[6]*colMeans(wines)[5]
bx6 <- lr_coef[7]*colMeans(wines)[6]
bx7 <- lr_coef[8]*range.tsd
bx8 <- lr_coef[9]*colMeans(wines)[8]
bx9 <- lr_coef[10]*colMeans(wines)[9]
bx10 <- lr_coef[11]*colMeans(wines)[10]
bx11 <- lr_coef[12]*colMeans(wines)[11]

t.s.d.Good <- b0+bx1+bx2+bx3+bx4+bx5+bx6+bx7+bx8+bx9+bx10+bx11

Prob.Good.wine <- exp(t.s.d.Good)/(1+exp(t.s.d.Good))

plot(range.tsd,Prob.Good.wine,ylim=c(0,1), type="l", lwd=2, col="blue",
     xlab="Total Sulfur-dioxide", ylab="P(Good Wine)", main="Probability of Good wine")

abline(h=.5, lty=2)


# Log-posterior distribution
lpost.LR <- function(beta,x,y){
  eta <- as.numeric(x %*% beta)
  logp <- eta - log(1+exp(eta))
  logq <- log(1-exp(logp))
  logl <- sum(logp[y==1]) + sum(logq[y==0])
  lprior <- sum(dnorm(beta,0,10,log=T))
  return(logl + lprior)
}

S <- 10^4

X = as.matrix(cbind(rep(1,nrow(wines)), wines[,1:11]))
colnames(X)[1] <- "Intercept"
y = wines[,"Good"]


k <- ncol(X)
acc <-0
Omega_prop <- solve(t(X) %*% X)

library(mvtnorm)

beta_mat1 <- beta_mat2 <- beta_mat3 <- beta_mat4 <- matrix(NA, nrow=S, ncol= ncol(X))
colnames(beta_mat1) <- colnames(beta_mat2) <- colnames(beta_mat3) <- colnames(beta_mat4) <- colnames(X)

beta_mat1[1,] <- lr_coef
beta_mat2[1,] <- c(10,0.5,-3, 1.2,0.3,-10,0.05,-0.018,-10,0.3,4,0.9)
beta_mat3[1,] <- c(-10,-0.1,-5,-1,-0.35,-12,0.05,-0.01,10,0,0,0)
beta_mat4[1,] <- c(rep(0,12))

tuning = 0.85
acc=0

for(i in 2:S){
  
  # 1. Propose a new set of values
  beta_star1 <- rmvnorm(1,beta_mat1[i-1,],tuning*Omega_prop)
  beta_star2 <- rmvnorm(1,beta_mat2[i-1,],tuning*Omega_prop)
  beta_star3 <- rmvnorm(1,beta_mat3[i-1,],tuning*Omega_prop)
  beta_star4 <- rmvnorm(1,beta_mat4[i-1,],tuning*Omega_prop)
  
  # 2. Compute the posterior density on the proposed value and on the old value  
  newpost1=lpost.LR(t(beta_star1),X,y)
  newpost2=lpost.LR(t(beta_star2),X,y)
  newpost3=lpost.LR(t(beta_star3),X,y)
  newpost4=lpost.LR(t(beta_star4),X,y)
  
  oldpost1=lpost.LR(matrix(beta_mat1[i-1,],ncol=1),X,y)
  oldpost2=lpost.LR(matrix(beta_mat2[i-1,],ncol=1),X,y)
  oldpost3=lpost.LR(matrix(beta_mat3[i-1,],ncol=1),X,y)
  oldpost4=lpost.LR(matrix(beta_mat4[i-1,],ncol=1),X,y)
  
  # 3. Acceptance step
  if(runif(1,0,1)>exp(newpost1-oldpost1)){
    beta_mat1[i,]=beta_mat1[i-1,]
  } else {
    beta_mat1[i,]=beta_star1
    acc=acc+1
  }
  if(runif(1,0,1)>exp(newpost2-oldpost2)){
    beta_mat2[i,]=beta_mat2[i-1,]
  } else {
    beta_mat2[i,]=beta_star2
    acc=acc+1
  }
  if(runif(1,0,1)>exp(newpost3-oldpost3)){
    beta_mat3[i,]=beta_mat3[i-1,]
  } else {
    beta_mat3[i,]=beta_star3
    acc=acc+1
  }
  if(runif(1,0,1)>exp(newpost4-oldpost4)){
    beta_mat4[i,]=beta_mat4[i-1,]
  } else {
    beta_mat4[i,]=beta_star4
    acc=acc+1
  }
  
  # 4. Print the stage of the chain
  if(i%%1000==0){print(c(i,acc/(i*4)))}
  
}

burn.in= 2000

par(mfrow=c(4,3))

for(i in 1:12){
  plot(beta_mat1[,i],type="l", col=1, ylab=colnames(beta_mat1)[i], xlab = "iteration")
  lines(beta_mat2[,i],type="l", col=2, lty=3)
  lines(beta_mat3[,i],type="l", col=3, lty=3)
  lines(beta_mat4[,i],type="l", col=4, lty=3)
}

par(mfrow=c(4,3))

for(i in 1:12){
  plot(beta_mat1[burn.in:S,i],type="l", col=1, ylab=colnames(beta_mat1)[i], xlab = "iteration")
  lines(beta_mat2[burn.in:S,i],type="l", col=2, lty=3)
  lines(beta_mat3[burn.in:S,i],type="l", col=3, lty=3)
  lines(beta_mat4[burn.in:S,i],type="l", col=4, lty=3)
}

y_new <- c(1)
x_new <- c(1,7.5,0.6,0,1.7,0.085,5,45,0.9965,3.4,0.63,12)
par(mfrow=c(1,1))

for(i in burn.in:S){
  
  # 5. Prediction 
  p_new <- exp(sum(beta_mat1[i,] * x_new) ) / (1 + exp(sum(beta_mat1[i] * x_new) ))
  y_new[i] <- rbinom(1,1,prob=p_new)
  
}

hist(y_new, main="Post-predictive distribution",
     breaks = 2, col="green")

table(y_new)
831/(831+7171)

library(mcmc)

# Log-posterior distribution
lpost.metr <- function(x,y)function(beta){
  eta <- as.numeric(x %*% beta)
  logp <- eta - log(1+exp(eta))
  logq <- log(1-exp(logp))
  logl <- sum(logp[y==1]) + sum(logq[y==0])
  lprior <- sum(dnorm(beta,0,10,log=T))
  return(logl + lprior)
}

lpost <- lpost.metr(X, y)

burn.in= 2000
tuning = 0.25

out1 <- metrop(lpost, beta_mat1[1,], S, scale=tuning*Omega_prop)
out2 <- metrop(lpost, beta_mat2[1,], S, scale=tuning*Omega_prop)
out3 <- metrop(lpost, beta_mat3[1,], S, scale=tuning*Omega_prop)
out4 <- metrop(lpost, beta_mat4[1,], S, scale=tuning*Omega_prop)

out1$accept
out2$accept
out3$accept
out4$accept

par(mfrow=c(4,3))

axis <- rbind(c(-25,25),c(-0.5,0.8),c(-6,1),c(-1.8,2),c(-0.5,0.5),c(-15,1),
              c(-0.02,0.07),c(-0.025,0.005),c(-25,25),c(-5,5),c(-1,5),c(-0.2,1.2))


for(i in 1:12){
  plot(out1$batch[burn.in:S,i],type="l", col=1, ylab=colnames(beta_mat1)[i], xlab = "iteration", ylim=axis[i,])
  lines(out2$batch[burn.in:S,i],type="l", col=2, lty=3)
  lines(out3$batch[burn.in:S,i],type="l", col=3, lty=3)
  lines(out4$batch[burn.in:S,i],type="l", col=4, lty=3)
}


