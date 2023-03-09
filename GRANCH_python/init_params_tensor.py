import torch
import compute_prob_tensor
import helper
import numpy as np
import pandas as pd 


class granch_params: 
    def __init__(self, 
                 grid_mu, 
                 grid_sigma, 
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
        

        self.device = 'cuda' if torch.cuda.is_available() else 'cpu'
        # grid parameters 
        self.grid_mu = grid_mu
        self.grid_sigma = grid_sigma
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
        (self.meshed_grid_mu, 
        self.meshed_grid_sigma, 
        self.meshed_grid_y, 
        self.meshed_grid_epsilon) = torch.meshgrid(
            self.grid_mu,
            self.grid_sigma, 
            self.grid_y,
            self.grid_epsilon
        )


    def add_lp_mu_sigma(self): 
        
        # ad lp_mu_sigma
        self.lp_mu_sigma = compute_prob_tensor.score_mu_sigma(
            input_x = self.meshed_grid_mu,                                          
            input_sigma = self.meshed_grid_sigma, 
            mu  = self.mu_prior, 
            nu = self.V_prior, 
            alpha = self.alpha_prior,
            beta = self.beta_prior, 
            device = self.device
        )


    def add_y_given_mu_sigma(self): 
        self.lp_y_given_mu_sigma = compute_prob_tensor.score_y_given_mu_sigma(
            y_val = self.meshed_grid_y, 
            mu = self.meshed_grid_mu, 
            sigma = self.meshed_grid_sigma
        )

    def add_lp_epsilon(self): 
        self.lp_epsilon = compute_prob_tensor.score_epsilon(
            epsilon = self.meshed_grid_epsilon,
            mu_epsilon = self.mu_epsilon, 
            sd_epsilon = self.sd_epsilon
        )

    def add_priors(self):
        # not the most efficient 
        # but currently do so to avoid misalignment between prior and likelihood
       
       self.prior = self.lp_mu_sigma + self.lp_epsilon
    