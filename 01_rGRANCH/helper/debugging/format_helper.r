
format_hypothetical_kl_pp <- function(all_sims_res, sequence_scheme_id){
  
  total_t <- all_sims_res$results[[sequence_scheme_id * 2]] %>% 
    filter(!is.na(EIG)) %>% 
    nrow()
  
  all_kl_pp <- lapply(seq(1, total_t, 1), 
                      function(x){
                        kls <- all_sims_res$results[[sequence_scheme_id * 2 -1]][[x]][[2]] %>% 
                          t() %>% as.data.frame() %>% 
                          mutate(timestep = x, 
                                 type = "kl")
                        
                        pps <- 
                          all_sims_res$results[[sequence_scheme_id * 2 - 1]][[x]][[3]] %>% 
                          t() %>% as.data.frame() %>% 
                          mutate(timestep = x, 
                                 type = "pp")
                        
                        obs <- all_sims_res$results[[sequence_scheme_id * 2 - 1]][[x]][[4]] %>%                                   t() %>% 
                          as.data.frame() %>% mutate(timestep = x, type = "obs")
                        
                        df <- bind_rows(kls, pps, obs)
                        
                      }) %>% 
    bind_rows()
  
  return (all_kl_pp)
  
}


# function to get posterior 
# sequence_scheme_id to get the right habituation pattern 
get_posterior <- function(all_sims_res, sequence_scheme_id){
  # sim results stored at the second element 
  total_t <- all_sims_res$results[[sequence_scheme_id * 2]] %>% 
    filter(!is.na(EIG)) %>% 
    nrow()
  
  all_posteriors <- lapply(seq(1, total_t, 1), 
                           function(x){
                             all_sims_res$results[[sequence_scheme_id * 2 - 1]][[x]][[1]] %>% 
                               mutate(timestep = x)
                           }) %>% 
    bind_rows()
  
  return(all_posteriors)
}

get_hypo_posterior <- function(all_sims_res, sequence_scheme_id){
  # sim results stored at the second element 
  total_t <- all_sims_res$results[[sequence_scheme_id * 2]] %>% 
    filter(!is.na(EIG)) %>% 
    nrow()
  
  all_posteriors <- lapply(seq(1, total_t, 1), 
                           function(x){
                             lapply(seq(1, 3, 1), 
                                    function(id){
                                      all_sims_res$results[[sequence_scheme_id]][[x]][[5]][[id]] %>% 
                                        mutate(timestep = x, 
                                               hypo_obs_id = id)
                                    }) %>% 
                               bind_rows()
                             
                           }) %>% 
    bind_rows()
  
  return(all_posteriors)
}


get_stimulus_idx_t <- function(all_sims_res, sequence_scheme_id, 
                               target_stimulus_idx){
  
  all_sims_res$results[[sequence_scheme_id * 2]] %>% 
    filter(stimulus_idx == target_stimulus_idx) %>% 
    select(t)
  
}
