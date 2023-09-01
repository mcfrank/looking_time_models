import numpy as np
from cmdstanpy.model import CmdStanModel
from cmdstanpy.utils import cmdstan_path

# create data parameters
num_coin_flips = 100;
samples_per_flip = 10;
p_heads = 0.3 # true weight of the coin 
p_flip = 0.1 # probability that sample is misperceived

# generate coin flip outcomes
np.random.seed(1)

# (tails = 0, heads = 1)
flip_outcomes = np.random.choice([0,1], size = (num_coin_flips,), p = [1-p_heads, p_heads])

# generate noisy samples
samples = np.array([])
for flip in flip_outcomes:
     # we take inaccurate samples with probability p_flip:
    # first determine whether sample is inaccurate 
    flip_samples = np.random.choice([0,1], size = (samples_per_flip,), p = [1-p_flip, p_flip])

    # flip samples that are misperceived (will not change if flip_samples = 0, otherwise will flip)
    cur_samples = abs(flip - flip_samples)

    # concatenate samples
    samples = np.append(samples, cur_samples)

# index which determines which samples goes with which flip
exemplar_idx = np.repeat(range(0,num_coin_flips), samples_per_flip)

# model priors
# priors on coin weight
alpha = 1 
beta = 1
# prior on probability of misperceiving
noise_theta = 0.1

# make data
data = {'M': len(samples), 'K':num_coin_flips, 'z': samples, 'exemplar_idx': exemplar_idx, # data
        'alpha': alpha, 'beta':beta, 'noise_theta': noise_theta} # priors

# build & run inference model
sm = CmdStanModel(stan_file = 'noisy_beta_binomial.stan')

fit = sm.sample(data=data);
