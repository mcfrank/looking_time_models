get_scaling_param <- function(all_sim_d){
  
  all_sim_d$intercept <- unlist(map(all_sim_d$data, function(x){
    fit_res <- lm(mean_sample~ mean_lt, d = x) %>% broom::tidy()
    intercept = filter(fit_res, term == "(Intercept)")$estimate
    return(intercept)
  })) 
  
  all_sim_d$coef <- unlist(map(all_sim_d$data, function(x){
    fit_res <- lm(mean_sample~ mean_lt, d = x) %>% broom::tidy()
    coef = filter(fit_res, term == "mean_lt")$estimate
    return(coef)
  })) 
  
  return(all_sim_d)
  
}


get_scaling_param_constraint <- function(all_sim_d){
  
  all_sim_d$intercept <- unlist(map(all_sim_d$data, function(x){
    intercept = colf::colf_nlxb(mean_sample ~ mean_lt, d = x, lower = c(-Inf, 0.0000471))$coefficients[[1]]
    return(intercept)
  })) 
  
  all_sim_d$coef <- unlist(map(all_sim_d$data, function(x){
    coef = colf::colf_nlxb(mean_sample ~ mean_lt, d = x, lower = c(-Inf, 0.0000471))$coefficients[[2]]
    return(coef)
  })) 
  
  return(all_sim_d)
  
}


get_scaling_param_flipped <- function(all_sim_d){
  
  all_sim_d$intercept <- unlist(map(all_sim_d$data, function(x){
    intercept = colf::colf_nlxb(mean_lt ~ mean_sample, d = x, lower = c(-Inf, 0))$coefficients[[1]]
    return(intercept)
  })) 
  
  all_sim_d$coef <- unlist(map(all_sim_d$data, function(x){
    coef = colf::colf_nlxb(mean_lt ~ mean_sample, d = x, lower = c(-Inf, 0))$coefficients[[2]]
    return(coef)
  })) 
  
  return(all_sim_d)
  
}