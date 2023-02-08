import pandas
import pickle
import os
import numpy as np
import matplotlib.pyplot as plt
import arviz
import pystan
import random

# whether to recompile the stan program
DO_COMPILE = True

# stan program path
stan_path = 'multi_feature.stan'

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


sm = build_model(path = stan_path, pkl_file='model.pkl', do_compile=DO_COMPILE)


random.seed(1);

# Simulate our data
num_samples = 100
num_exemplars = 5
num_features = 3
exemplar_idx = np.random.choice(list(range(1, num_exemplars+1)), num_samples, p=[0.2, 0.2, 0.2, 0.2, 0.2]) 

exemplar_means = np.random.randint(10, size = (num_exemplars, num_features))

sig = np.identity(num_features);

sim_data = [np.random.multivariate_normal(exemplar_means[idx-1], sig) for idx in exemplar_idx] 

data = {"F": num_features, "M": num_samples, "K": num_exemplars, "z": np.transpose(sim_data), "exemplar_idx": exemplar_idx}
fit = sm.sampling(data=data, iter=10000, chains=1)

model_fit = arviz.from_pystan(fit)

arviz.plot_posterior(model_fit, kind = 'hist')
print(fit)

plt.show()

