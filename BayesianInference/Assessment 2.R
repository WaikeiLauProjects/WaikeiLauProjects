

# Question 1 [2 points] Plot the density f(x)

  th=seq(0,1,length=1000)
  par(mfrow=c(1,1))

  fx= (1/3)*(dbeta(th,1,5)+dbeta(th,3,5)+dbeta(th,10,5))

  plot(th,fx,col="blue",
     lwd=2, type="l", xlab=expression(theta), ylab="Density",main="Monte Carlo Mixture of Beta",xlim=c(0,1))

# Question 2 [1 point] Set the seed in R at 1234

  set.seed(1234)

# Question 2 [3 points] Define the value K, i.e. the maximum of the density and associate that with the corresponding x value

  k=max(fx)
  Mx = th[which.max(fx)]

# Question 2 [2 points] Simulate 10,000 values for a uniform distribution

  numb_rejected =accepted_x =u_all =c()
  rejected=0
  
  for(i in 1:10000){
    
    x_prop<- runif(1,0,1)

    # Question 2 [3 points] Define the acceptance probability
    
    acceptance_prob<- (dbeta(x_prop,1,5)+dbeta(x_prop,3,5)+dbeta(x_prop,10,5))/
      (dbeta(Mx,1,5)+dbeta(Mx,3,5)+dbeta(Mx,10,5))
    
    # Question 2 [3 points] Accept or reject the simulated value according to the acceptance probability
    
    u<- runif(1,0,1)
    u_all <- c(u_all,x_prop)
    
    if(u<acceptance_prob){
      accepted_x <- c(accepted_x,x_prop)
      numb_rejected <- c(numb_rejected,rejected)
      rejected = 0
    } else {
      rejected = rejected +1
    }
    
  }

# Question 3 [3 points] Compute the observed acceptance rate and compare it with the theoretical one. 
  
  acceptance_rate = length(accepted_x)/100000
  
  theoretical_acceptance = 1/k
  
# Question 4 [1 point] Use the same 10,000 values generated from the Unif(0,1) for the implementation of the accept/reject algorithm
# Question 4 [3 points] Compute the importance weights
  
  w=(1/3)*(dbeta(u_all,1,5)+dbeta(u_all,3,5)+dbeta(u_all,10,5))/dunif(u_all)
  W=w/sum(w)

# Question 5 [4 points] Compare on the same plot the target density, the density of the accepted values for the
# accept/reject algorithm, and the density of the values weighted by importance sampling. Notice that R has an
# option in the density() function where you can add a vector of weights
  
  c=density(accepted_x, from = 0, to = 1)
  lines(c, col="green", lwd = 2, lty=2)
  
  d=density(u_all, weights = W, from = 0, to = 1)
  lines(d, col="red", lwd = 2, lty=2)
  
  legend(x="bottomleft", inset=0.005, bty = "n", cex=0.7, legend=c("Target","Accept/Reject","Importance"), col=c("blue","green","red"), lty = c(1,2,2))
  