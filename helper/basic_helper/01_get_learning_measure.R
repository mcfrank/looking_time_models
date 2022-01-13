get_beta_count <- function(block, feature_prior = c(3,1)){
  prior <- replicate(length(block[[1]]), feature_prior, simplify = FALSE)
  
  beta_count <- list()
  beta_count[[1]] <- prior 
  for (trial in 1:length(block)){
    beta_count[[trial+1]] <- mapply(function(x, y) {
      x[y + 1] <- x[y + 1] + 1
      return(list(x))
    },
    beta_count[[trial]], 
    block[[trial]])
  }
  
  return(beta_count)
  
}

get_probability <- function(block_beta){
  
  lapply(block_beta, function(x) lapply(x, function(x) x/sum(x)))
  
}

get_surprise <- function(block_probability, block_sequence){
  
  mapply(function (probs, seq) {
    sum(-log2(mapply(function (x, y) x[y + 1], probs, seq))) 
  }, block_probability[1:length(block_probability)-1], block_sequence)
  
}

#KL divergence betwen two bernoulli distribution? https://math.stackexchange.com/questions/2604566/kl-divergence-between-two-multivariate-bernoulli-distribution#:~:text=The%20KL%20divergence%20between%20two%20such%20distributions%20is%20DK,z)q(z).

get_learning_progress <- function(block_probability){
  learning_progress <- list()

  for (trial in 2:length(block_probability)){
    
    
    prev_prob = block_probability[[trial-1]]
    curr_prob = block_probability[[trial]]
    trial_lp = mapply(function(curr, prev){
      #lp <- curr[[1]] * log2(curr[[1]]/prev[[1]]) #the probability of seeing the feature 
      lp <- curr[[1]]*log2(curr[[1]]/prev[[1]]) + curr[[2]]*log2(curr[[2]]/prev[[2]])
      return(lp) 
    }, curr_prob, prev_prob)
    
    learning_progress[trial] <- sum(trial_lp) 
  }
  return(learning_progress[2:length(learning_progress)])
}