thetas = seq(0.1,0.9,0.2)

IGs <- c()
EIGs <- c()

for (i in seq(1,15,1)) {
  
  a <- 1
  b <- i
  
  previous_dist <- dbeta(thetas, a, b) / length(thetas)
  current_dist <- dbeta(thetas, a, b+1) / length(thetas)
  
  previous_entropy <- -sum((previous_dist) * log(previous_dist))
  current_entropy <- -sum((current_dist) * log(current_dist))
  
  IG <- previous_entropy - current_entropy
  
  IGs <- c(IGs, IG)
  
  # calculate IG if had observed a success (i.e. a positive count)
  counterfactual_dist <- dbeta(thetas, a+1, b) /length(thetas)
  counterfactual_entropy <- -sum((counterfactual_dist) * log(counterfactual_dist))
  counterfactual_IG <- previous_entropy - counterfactual_entropy
  
  # calculate posterior predictive distribution (first value is probability of failure, second is of success)
  posterior_predictive <- unlist(lapply(0:1, beta_bernoulli))
  
  # expected information gain (weighted average of real IG and hypothetical IG)
  EIG <- posterior_predictive[1]*IG + posterior_predictive[2]*counterfactual_IG
  
  EIGs <- c(EIGs, EIG)
  
}

plot(seq(1,15,1), IGs)
plot(seq(1,15,1), EIGs)


beta_bernoulli <- function(success){
  log_prob <- lbeta(a+success,b+1-success) - lbeta(a,b)
  return(exp(log_prob))
}