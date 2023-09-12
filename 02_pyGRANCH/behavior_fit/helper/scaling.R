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