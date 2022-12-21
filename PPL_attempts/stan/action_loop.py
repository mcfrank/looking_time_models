# %%
import math
import os
import pickle
import random

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
#import arviz

import multiprocessing
multiprocessing.set_start_method('spawn', True)

from cmdstanpy.model import CmdStanModel
from cmdstanpy.utils import cmdstan_path

from cmdstanpy import CmdStanModel

from scipy.spatial import cKDTree as KDTree
from scipy.stats import entropy, gamma, gaussian_kde, multivariate_normal
#from sklearn.mixture import GaussianMixture
#from sklearn.neighbors import KernelDensity

# from fastkde import fastKDE
# from entropy_estimators import continuous
# import seaborn as sns
# from tqdm.auto import tqdm

from cmdstanpy import rebuild_cmdstan
#rebuild_cmdstan()

# %% [markdown]
# # Helper functions    

# %%
def KLdivergence(x, y):
    # Check the dimensions are consistent
    x = np.atleast_2d(x)
    y = np.atleast_2d(y)

    n,d = x.shape
    m,dy = y.shape

    assert(d == dy)

    # Build a KD tree representation of the samples and find the nearest neighbour
    # of each point in x.
    xtree = KDTree(x)
    ytree = KDTree(y)

  # Get the first two nearest neighbours for x, since the closest one is the
  # sample itself.
    thresh = 1e-4
    #r = xtree.query(x, k=100, eps=.01, p=2)[0]
    #s = ytree.query(x, k=100, eps=.01, p=2)[0]

    r = xtree.query(x, k=100, eps=.01, p=2)[0]
    s = ytree.query(x, k=100, eps=.01, p=2)[0]

    r = r[np.arange(r.shape[0]), np.argmax(r > thresh, axis=1)] 
    s = s[np.arange(s.shape[0]), np.argmax(s > thresh, axis=1)]
    
    if math.isinf(-np.log(r/s).sum() * d / n + np.log(m / (n - 1.))):
        a = 0;

    # There is a mistake in the paper. In Eq. 14, the right side misses a negative sign
    # on the first term of the right hand side.
    return -np.log(r/s).sum() * d / n + np.log(m / (n - 1.))

def kl_mvn(m0, S0, m1, S1):
 
  # store inv diag covariance of S1 and diff between means
  N = m0.shape[0]
  iS1 = np.linalg.inv(S1)
  diff = m1 - m0

  # kl is made of three terms
  tr_term   = np.trace(iS1 @ S0)
  det_term  = np.log(np.linalg.det(S1)/np.linalg.det(S0)) #np.sum(np.log(S1)) - np.sum(np.log(S0))
  quad_term = diff.T @ np.linalg.inv(S1) @ diff #np.sum( (diff*diff) * iS1, axis=1)
  #print(tr_term,det_term,quad_term)
  return .5 * (tr_term + det_term + quad_term - N) 

def surprisal(dist, sample):
    kde = gaussian_kde(dist)
    prob = kde.evaluate(sample)
    return -np.log(prob)


def EIG(dist, grid, thinning_factor):
        print(np.arange(-1.9, 2, 0.25))
        
        for hypothetical_obs in grid:
            
            data["z"] = np.hstack(np.transpose(sample_data[0:total_samples,:]), hypothetical_obs)

            # get posterior samples
            fit = sm.sampling(data=data, iter=num_iter, chains=1, warmup = num_warmup,control=dict(adapt_delta=0.95));
        
            hyp_posterior = np.hstack((fit['mu'][0:len(fit['mu']):thinning_factor], \
                       fit['sigma'][0:len(fit['mu']):thinning_factor]))
            
            hyp_entropy_reduction = np.abs(continuous.get_h(posterior, k = 200) - continuous.get_h(hyp_posterior, k = 200))
            
            pp =  kde = gaussian_kde(dist)
            prob = kde.evaluate(hypothetical_obs)
            
            EIG += pp * hyp_entropy_reduction
        return EIG
  
  

# %% [markdown]
# Build model

# %%
# whether to recompile the stan program
DO_COMPILE = True

# simple noise or prior on noise
SIMPLE_NOISE = False

# stan program path
if SIMPLE_NOISE:
    stan_path = 'multi_feature_simple_noise.stan'
    pkl_file = 'model_simple_noise.pkl'
else:
    stan_path = 'multi_feature.stan'
    pkl_file = 'model.pkl'


def build_model(path, pkl_file=None, do_compile=True):
    if do_compile:
        sm = pystan.StanModel(file=path)
        if pkl_file is not None:
            with open(pkl_file, 'wb') as f:
                pickle.dump(sm, f)

    # if the program hasn't been complied, check that the file already exists
    else: 
        if os.path.isfile(pkl_file):
            sm = pickle.load(open(pkl_file, 'rb'))
        else:
            raise FileNotFoundError
    return sm

sm = CmdStanModel(stan_file = 'multi_feature.stan')



# %% [markdown]
# Model parameters

# %%
# mu
mu_mean = 0
mu_sd = 1

# sd
sigma_alpha = 2
sigma_beta = 2

# lower bound of sigma prior
lower_sigma_bound = 0.1

# noise SD prior
noise_alpha =  2
noise_beta = 2

# noise SD prior
noise_mean =  2
noise_sd = 0.1

# for simple noise 
noise = 0.4

# environmental information
env_info = 1.2

# %% [markdown]
# Create data

# %%
# number of stimuli
sequence_length = 6

# number of features 
num_features = 1

# number of samples (max)
num_samples = 3000

# allocation of samples to exemplars
exemplar_idx = np.repeat(np.arange(1, sequence_length+1), num_samples/sequence_length)

# background / deviant mean values
background = np.repeat(3, num_features)
deviant = np.repeat(6, num_features)

# perceptual noise
sig = np.identity(num_features) * 1;

# deviant position
deviant_pos = 6

# stimulus means
exemplar_means = np.tile(background, (sequence_length, 1))
exemplar_means[deviant_pos-1] = deviant

# %% [markdown]
# Simulation parameters

# %%
# number of iterations and warmup per model run
num_iter = 10000
num_warmup = 4000 # can we cash this?

# number of total model runs
num_model_runs = 1

# how much to thin posteriors (value determines how we "retain every nth sample")
thinning_factor = 1

# %% [markdown]
# Initialize flags, variables, iterators 

# %%
# Flags 
sample = True
policy = 'surprisal' # 'kl', 'surprisal', 'entropy' or 'eig'

# Variablesen
model_LT = np.zeros((num_model_runs, sequence_length))

# prior parameters
prior_mu = np.random.multivariate_normal(np.repeat(mu_mean, num_features), np.identity(num_features)*mu_sd, num_iter-num_warmup)

prior_sigma = np.empty((num_iter-num_warmup, num_features))
for i in np.arange(0, num_features):
    
    # compute truncated sigma prior
    nrm=gamma.cdf(100, a = sigma_alpha, scale = 1/sigma_beta)- gamma.cdf(lower_sigma_bound, a = sigma_alpha, scale = 1/sigma_beta)
    yr=np.random.rand(num_iter-num_warmup)*(nrm)+gamma.cdf(lower_sigma_bound,  a = sigma_alpha, scale = 1/sigma_beta)
    prior_sigma[:,i] = gamma.ppf(yr, a = sigma_alpha, scale = 1/sigma_beta)
    
    
prior_z_rep = np.empty((num_iter-num_warmup, num_features))
for i in np.arange(0, num_iter-num_warmup):
    prior_z_rep[i,:] = np.random.multivariate_normal(prior_mu[i,:], np.identity(num_features)*prior_sigma[i,:])

prior = np.hstack((prior_mu[0:len(prior_mu):thinning_factor], prior_sigma[0:len(prior_sigma):thinning_factor]))

data = {"mu_mean": mu_mean , "mu_sd": mu_sd, "sigma_alpha": sigma_alpha, "sigma_beta": sigma_beta, 
"noise_alpha": noise_alpha, "noise_beta": noise_beta, "noise_mean": noise_mean, "noise_sd": noise_sd, "noise": noise, "F": num_features}

stim_info = np.empty((num_model_runs, num_samples))
stim_info[:] = np.nan

# %% [markdown]
# # Action loop

# %%
for run in np.arange(0, num_model_runs):
    
    print('model run: ', run)

    # generate the data
    sim_data = [np.random.multivariate_normal(exemplar_means[idx-1], sig) for idx in exemplar_idx] 
    sim_data = np.asmatrix(sim_data)

    print('sim data')
    print(sim_data)

    # Iterators
    samples_from_current_stim = 1
    total_samples = 1
    exemplar_num = 1

    # initialize data
    sample_data = np.empty((num_samples,num_features))
    sample_data[:] = np.nan

    exemplar_labels = np.empty((num_samples,))
    exemplar_labels[:] = np.nan

    while sample or samples_from_current_stim > 1:
        
        print('stimulus: ', exemplar_num)
        
        print('sample: ', total_samples)

        # sample number
        data["M"] = total_samples

        # exemplar number 
        data["K"] = exemplar_num

        # add sim data
        sample_data[total_samples-1] = sim_data[exemplar_idx == exemplar_num][samples_from_current_stim-1]
        data["z"] = np.transpose(sample_data[0:total_samples,:])

        # add exemplar for each id
        exemplar_labels[total_samples-1] = int(exemplar_num)
        data["exemplar_idx"] = [int(x) for x in exemplar_labels[~np.isnan(exemplar_labels)]]

        # get posterior samples
        fit = sm.sample(data=data, iter_sampling=num_iter, chains=1, iter_warmup = num_warmup, adapt_delta = 0.99);
        
        posterior = np.hstack([fit.stan_variable('mu'), fit.stan_variable('sigma')])

        # fit gmms
        #gmm_p = GaussianMixture(n_components=2, random_state=0).fit(posterior)
        #gmm_q = GaussianMixture(n_components=2, random_state=0).fit(prior)

        if total_samples < 20:
            
            plt.figure(1)
            plt.suptitle('mu')
            plt.rcParams['figure.figsize'] = [18, 12]
            plt.subplot(4,5,total_samples)
            plt.hist(prior[:,0], bins = 100, color = 'b', alpha=0.5);
            plt.hist(posterior[:,0], bins = 100, color = 'r', alpha=0.5);
            plt.title(str(total_samples) + " , " + str(exemplar_num))
            plt.legend('prior', 'posterior')
            
            plt.figure(2)
            plt.suptitle('sigma')
            plt.rcParams['figure.figsize'] = [18, 12]
            plt.subplot(4,5,total_samples)
            plt.hist(prior[:,1], bins = 100, color = 'b', alpha=0.5);
            plt.hist(posterior[:,1], bins = 100, color = 'r', alpha=0.5);            
            plt.title(str(total_samples) + " , " + str(exemplar_num))
            plt.legend('prior', 'posterior')
           
           
        else: 
            plt.show()
            break;
            
        if policy == 'kl':

            stim_info[run,total_samples-1] = KLdivergence(posterior, prior)

            print('KL:',  stim_info[run,total_samples-1])

        elif policy == 'entropy':
            # reduction of entropy
            stim_info[run,total_samples-1] = np.abs(continuous.get_h(prior, k= 250) - continuous.get_h(posterior, k = 250))
            
            print('entropy change:',  stim_info[run,total_samples-1])
            
        elif policy == 'surprisal':   
                            
            # surprisal of current observation given prior
            stim_info[run, total_samples-1] = surprisal(np.squeeze(fit.stan_variable('z_rep')), sample_data[total_samples-1])
            
            print('surprisal:',  stim_info[run,total_samples-1])

        elif policy == 'EIG':
            
            hypothethical_grid = np.arange(-2, 2, 0.1)
            
            stim_info[run,total_samples-1] = EIG(posterior)

        # decision rule
        if stim_info[run,total_samples-1] < env_info:
            model_LT[run, exemplar_num-1] = samples_from_current_stim

            # reset/increment counters
            samples_from_current_stim = 1
            exemplar_num += 1

            if exemplar_num > sequence_length:
                sample = False

        else:
            samples_from_current_stim += 1 

        if (policy == 'kl') | (policy == 'surprisal') | (policy == 'entropy'):
            prior = posterior
        
        total_samples += 1

    # start sampling for next model run
    sample = True

    
plt.show()


font = {'weight' : 'bold',
        'size'   : 22}

plt.rc('font', **font)

plt.plot(np.arange(1,sequence_length+1), np.mean(model_LT, axis = 0).squeeze(), 'k*')

plt.xlabel("stim index")
plt.ylabel("model samples")
plt.title("")

plt.show()


plt.plot(np.arange(1,total_samples+1),np.mean(stim_info, axis = 0)[0:total_samples].squeeze(), markersize = 20)

plt.xlabel("sample index")
plt.ylabel("stim info")
plt.title("")
plt.show()