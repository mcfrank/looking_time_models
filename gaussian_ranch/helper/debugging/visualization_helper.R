plot_sim_results <- function(all_sims_res){
  # the simulation results are all stored in the even number rows 
  lapply(seq(2, nrow(all_sims_res), 2), 
         function(x){
           all_sims_res$results[[x]] %>% 
             mutate(sequence_scheme = all_sims_res$sequence_scheme[[x]], 
                    sim_id = x)
         }
  ) %>% 
    bind_rows() %>% 
    group_by(stimulus_idx, sim_id, sequence_scheme) %>%
    filter(!is.na(stimulus_idx)) %>%
    summarise(sample_n = n()) %>%
    ggplot(aes(x = stimulus_idx, y = sample_n))+
    geom_point() + 
    facet_wrap(~sequence_scheme)
}

plot_sim_eig <- function(all_sims_res){
  # the simulation results are all stored in the even number rows 
  lapply(seq(2, nrow(all_sims_res), 2), 
         function(x){
           all_sims_res$results[[x]] %>% 
             mutate(sequence_scheme = all_sims_res$sequence_scheme[[x]], 
                    sim_id = x)
         }
  ) %>% 
    bind_rows() %>% 
    ggplot(aes(x = t, y = EIG, 
               color= as.factor(stimulus_idx)))+
    geom_point() + 
    facet_wrap(~sequence_scheme)
}

plot_sim_eig_on_stimulus <- function(all_sims_res, start_stimulus_id, end_stimulus_id){
  # the simulation results are all stored in the even number rows 
  lapply(seq(2, nrow(all_sims_res), 2), 
         function(x){
           all_sims_res$results[[x]] %>% 
             mutate(sequence_scheme = all_sims_res$sequence_scheme[[x]], 
                    sim_id = x)
         }
  ) %>% 
    bind_rows() %>% 
    filter(stimulus_idx >= start_stimulus_id, stimulus_idx <= end_stimulus_id) %>% 
    ggplot(aes(x = t, y = EIG, 
               color= as.factor(stimulus_idx)))+
    geom_point() + 
    facet_wrap(~sequence_scheme)
}

visualize_posteriors <- function(all_posteriors, t_start, t_end){
  all_posteriors %>% 
    filter(timestep >= t_start & timestep <= t_end) %>% 
    ggplot(aes(x = grid_mu_theta, 
               y = posterior, 
               color = grid_sig_sq)) + 
    geom_point() + 
    facet_wrap(~timestep)
  
}


visualize_hypo_posteriors <- function(all_hypo_posteriors, t_start, t_end){
  all_hypo_posteriors %>% 
    filter(timestep >= t_start & timestep <= t_end) %>% 
    ggplot(aes(x = grid_mu_theta, 
               y = posterior, 
               color = grid_sig_sq)) + 
    geom_point() + 
    facet_grid(hypo_obs_id~timestep)
  
}