get_experiment_parameter <- function(exp_d){
  
  d_block_length <- exp_d %>% 
    mutate(temp_id = paste(subject, block_number)) %>% 
    group_by(temp_id) %>% 
    count() %>% 
    rename(block_length = n)
  
  d_experiment_parameter <- exp_d %>% 
    rowwise() %>% 
    mutate(
      temp_id = paste(subject, block_number), 
      dev_positions = case_when(
        !is.na(first_dev_position) && !is.na(second_dev_position) ~ map2(first_dev_position, second_dev_position, c), 
        is.na(first_dev_position) && !is.na(second_dev_position) ~ list(second_dev_position), 
        !is.na(first_dev_position) && is.na(second_dev_position) ~ list(first_dev_position),
        TRUE ~ list(NA)
      )) %>% 
    left_join(d_block_length, by = "temp_id") %>% 
    select(-temp_id) %>% 
    separate(block_type, into = c("complexity", "similarity"), sep = "_") %>% 
    select(subject, block_number, complexity, similarity, block_length, dev_positions) %>% 
    distinct(subject, block_number, .keep_all = TRUE)
  
  
   return (d_experiment_parameter)
  
}