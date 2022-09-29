import torch
import compute_prob
import helper
import numpy as np
import pandas as pd 


class granch_params: 
    def __init__(self, 
                 grid_mu_theta, 
                 grid_sig_sq, 
                 grid_y, 
                 grid_epsilon, 
                 hypothetical_obs_grid_n, 
                 mu_prior,
                 V_prior, 
                 alpha_prior, 
                 beta_prior, 
                 epsilon, 
                 mu_epsilon, 
                 sd_epsilon, 
                 world_EIGs,
                 max_observation):
        
        # grid parameters 
        self.grid_mu_theta = grid_mu_theta
        self.grid_sig_sq = grid_sig_sq
        self.grid_y = grid_y
        self.grid_epsilon = grid_epsilon
        self.hypothetical_obs_grid_n = hypothetical_obs_grid_n

        # model parameters 
        self.mu_prior = mu_prior 
        self.V_prior = V_prior 
        self.alpha_prior = alpha_prior
        self.beta_prior = beta_prior
        self.epsilon = epsilon
        self.mu_epsilon = mu_epsilon
        self.sd_epsilon = sd_epsilon
        
        # simulation parameter
        self.world_EIGs = world_EIGs
        self.max_observation = max_observation

    def print_params_info(self): 
        # this could be used to gather human readable form on parameters
        pass 


    
    def add_meshed_grid(self): 
        # this function helps create all permuatation of those grid values 
        # so that it's easier to retrieve later     
        (meshed_grid_mu_theta, 
        meshed_grid_sig_sq, 
        meshed_grid_y, 
        meshed_grid_epsilon) = torch.meshgrid(
            self.grid_mu_theta,
            self.grid_sig_sq, 
            self.grid_y,
            self.grid_epsilon
        )

        full_meshed_grid = torch.stack([torch.reshape(meshed_grid_mu_theta, [-1]), 
                                        torch.reshape(meshed_grid_sig_sq, [-1]),
                                        torch.reshape(meshed_grid_y, [-1]),
                                        torch.reshape(meshed_grid_epsilon, [-1])], axis=1)

        full_meshed_grid_length= full_meshed_grid.size(dim = 0)

        self.meshed_grid_mu_theta = full_meshed_grid.gather(1, torch.zeros(full_meshed_grid_length, dtype = torch.int64).unsqueeze(1)).squeeze()
        self.meshed_grid_sig_sq = full_meshed_grid.gather(1, torch.ones(full_meshed_grid_length, dtype = torch.int64).unsqueeze(1)).squeeze()
        self.meshed_grid_y = full_meshed_grid.gather(1, torch.full([full_meshed_grid_length], fill_value = 2, dtype = torch.int64).unsqueeze(1)).squeeze()
        self.meshed_epsilon = full_meshed_grid.gather(1, torch.full([full_meshed_grid_length], fill_value = 3, dtype = torch.int64).unsqueeze(1)).squeeze()

        self.grouping_base = torch.cat(
            (
             self.meshed_epsilon.unsqueeze(1),
             self.meshed_grid_mu_theta.unsqueeze(1),
             self.meshed_grid_sig_sq.unsqueeze(1)
            ),
             dim = 1)



    def add_lp_mu_sig_sq(self): 
        
        # ad lp_mu_sig_sq
        self.lp_mu_sig_sq = torch.from_numpy(compute_prob.score_mu_sig_sq(
            input_x = self.meshed_grid_mu_theta.numpy(),                                          
            input_sig_sq = self.meshed_grid_sig_sq.numpy(), 
            mu  = self.mu_prior, 
            nu = self.V_prior, 
            alpha = self.alpha_prior,
            beta = self.beta_prior, 
            log = True
        ))


    def add_y_given_mu_sig_sq(self): 
        self.lp_y_given_mu_sig_sq = compute_prob.score_y_given_mu_sigma_sq(
            y_val = self.meshed_grid_y, 
            mu = self.meshed_grid_mu_theta, 
            sigma = self.meshed_grid_sig_sq
        )

    def add_lp_epsilon(self): 
        self.lp_epsilon = compute_prob.score_epsilon(
            epsilon = self.meshed_epsilon,
            mu_epsilon = self.mu_epsilon, 
            sd_epsilon = self.sd_epsilon
        )

    def add_priors(self):
        # not the most efficient 
        # but currently do so to avoid misalignment between prior and likelihood
        prior_df_t = torch.cat(
            (
             self.meshed_grid_mu_theta.unsqueeze(1),
             self.meshed_grid_sig_sq.unsqueeze(1), 
             self.meshed_epsilon.unsqueeze(1),
             self.lp_mu_sig_sq.unsqueeze(1),
             self.lp_epsilon.unsqueeze(1)
            ),
             dim = 1)

        unique_prior_df_t = torch.unique(prior_df_t, 
                                        dim = 0, sorted = False)
        
        
        self.prior_lp_mu_sig_sq = helper.get_ith_column(unique_prior_df_t, 3)
        self.prior_lp_epsilon = helper.get_ith_column(unique_prior_df_t, 4)
    


            





    
