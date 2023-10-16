
get_fitting_stats <- function(all_sim_d){
  
  # get pearson's r
  all_sim_d$r <- unlist(map(all_sim_d$data, function(x){
    r <- cor(x$mean_lt, x$mean_sample, method = "pearson")
  }))
  
  all_sim_d$r_ci <- unlist(map(all_sim_d$data, function(x){
    r_conf <- cor.test(x$mean_lt, x$mean_sample, method = "pearson", type = "bootstrap")$conf.int
    #r_conf <- confintr::ci_cor(x$mean_lt, x$mean_sample, method = "pearson", type = "bootstrap")["interval"][[1]]
    r_conf_print <- paste0("[", round(r_conf[1],2), ", ", round(r_conf[2],2), "]")
    return(r_conf_print)
  }))
  
  # get rmse's r 
  all_sim_d$rmse <- unlist(map(all_sim_d$data, function(x){
    rmse_log <- Metrics::rmse(x$mean_lt, x$mean_sample)
  }))

  
  all_sim_d$rmse_ci <- unlist(map(all_sim_d$data, function(x){
    rmse <- Metrics::rmse(x$mean_lt, x$mean_sample)
    rmse_interval <- rmse_interval(rmse, nrow(x))
    rmse_conf_print <- paste0("[", round(rmse_interval$.pred_lower,2), ", ", 
                              round(rmse_interval$.pred_upper,2), "]")
  }))

  
  return (all_sim_d)
}




rmse_interval <- function(rmse, deg_free, p_lower = 0.025, p_upper = 0.975){
  tibble(.pred_lower = sqrt(deg_free / qchisq(p_upper, df = deg_free)) * rmse,
         .pred_upper = sqrt(deg_free / qchisq(p_lower, df = deg_free)) * rmse)
}
