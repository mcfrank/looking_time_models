
# this function returns the corresponding vector of p(y|theta) given the concept value and the y_value  
select_y_val_theta <- function(lp_y_given_theta_one, 
                               lp_y_given_theta_two, 
                               concept, y_val){

  if(y_val == 0 & concept == 2){
    selected_y_theta <- lp_y_given_theta_two$lp_y_ZERO_given_theta_TWO
  }else if(y_val == 1 & concept == 2){
    selected_y_theta <- lp_y_given_theta_two$lp_y_ONE_given_theta_TWO
  }else if(y_val == 1 & concept == 1){
    selected_y_theta <- lp_y_given_theta_one$lp_y_ONE_given_theta_ONE
  }else if(y_val == 0 & concept == 1){
    selected_y_theta <-  lp_y_given_theta_one$lp_y_ZERO_given_theta_ONE
  }
  return(selected_y_theta)
}


# this function returns the value of /SUM/PRODp(y|theta) for a set of particular value of y 
get_y_theta_combination <- function(current_y_value_combo, 
                                    all_theta_value_combo, 
                                    lp_gamma_1, 
                                    lp_gamma_2){
  
  l_concept_combo_list <- vector(mode = "list", 
                                 length = nrow(all_theta_value_combo))
  # iterating through all the possible value combination 
  for (i in 1:nrow(all_theta_value_combo)){
    n_concept_one_occurence <- length(which(all_theta_value_combo[i, ] == 1))
    n_concept_two_occurence <- length(which(all_theta_value_combo[i, ] == 2))
    lp_y_theta_acc <- rep(0, length(grid_theta)) 
    for (j in 1:length(current_y_value_combo)){
      lp_y_theta <- select_y_val_theta(lp_y_given_theta_one, 
                                       lp_y_given_theta_two,
                                       concept = all_theta_value_combo[i, j], 
                                       y_val = current_y_value_combo[j])
      lp_y_theta_acc <- lp_y_theta_acc + lp_y_theta
      
    }
    
    l_concept_combo_list[[i]] <- tibble(
      theta = grid_theta, 
      lp_y_theta_gamma = lp_y_theta_acc + (lp_gamma_1 * n_concept_one_occurence) + (lp_gamma_2 * n_concept_two_occurence) 
    ) 
  }
  #res <- logSumExp(l_concept_combo_list)
  return (res)
}
