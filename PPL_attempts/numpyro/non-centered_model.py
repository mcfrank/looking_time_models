from jax import random, local_device_count
import numpyro
from numpyro.infer import MCMC, NUTS, Predictive
import numpyro.distributions as dist
from numpyro.infer.reparam import TransformReparam
import numpy as np
import arviz as az
import matplotlib as plt

ncpu = local_device_count()
numpyro.set_platform("cpu")
numpyro.set_host_device_count(4)

# %%

def model_noncentered(z=None, num_samples=None, num_exemplars=None, mu_loc=None, mu_scale=None, tau_scale=None, noise_scale=None):

    num_obs = num_exemplars * num_samples

    if z is not None:
        assert np.array_equal(z.shape, [nsamples, nexemplars])

    # Perceptual noise
    noise = numpyro.sample("noise", dist.HalfCauchy(noise_scale))

    # Concept
    mu = numpyro.sample("mu", dist.Normal(mu_loc, mu_scale))
    tau = numpyro.sample("tau", dist.HalfCauchy(tau_scale))

    with numpyro.plate("num_stim", num_exemplars, dim=-1):
        with numpyro.handlers.reparam(config={'theta': TransformReparam()}):
            theta = numpyro.sample(
                'theta',
                dist.TransformedDistribution(
                    dist.Normal(0., 1.),
                    dist.transforms.AffineTransform(mu, tau)))

        with numpyro.plate("obs", num_samples, dim=-2):
            numpyro.sample("z", dist.Normal(theta, noise), obs=z)


nsamples = 3
nexemplars = 2

data_seed = 1
rng = np.random.default_rng(data_seed)
zobs = rng.normal(0, 1, [nsamples, nexemplars])

nuts_kernel = NUTS(
    model_noncentered,
    target_accept_prob=0.90,
    adapt_step_size=True,
    adapt_mass_matrix=False,
    max_tree_depth=13,
    find_heuristic_step_size=True,
)
mcmc = MCMC(nuts_kernel, num_warmup=6000, num_samples=6000, num_chains=4, chain_method='parallel')
rng_key = random.PRNGKey(0)
mcmc.run(rng_key, z=zobs, num_samples=nsamples, num_exemplars=nexemplars, mu_loc=0., mu_scale=2., tau_scale=5., noise_scale=2., extra_fields=('potential_energy',))

### Fit summary
mcmc.print_summary(exclude_deterministic=False)

# %%

posterior_samples = mcmc.get_samples()
posterior_predictive = Predictive(model_noncentered, posterior_samples)(
    random.PRNGKey(1), num_samples=nsamples, num_exemplars=nexemplars, mu_loc=0., mu_scale=2., tau_scale=5., noise_scale=2.
)
prior_predictive = Predictive(model_noncentered, num_samples=500)(
    random.PRNGKey(2), num_samples=nsamples, num_exemplars=nexemplars, mu_loc=0., mu_scale=2., tau_scale=5., noise_scale=2.
)

# %%

az_data = az.from_numpyro(
    mcmc,
    prior=prior_predictive,
    posterior_predictive=posterior_predictive,
    coords={"exemplars": np.arange(nexemplars), "samples": np.arange(nsamples)},
    dims={"theta": ["exemplars"], "z": ["samples", "exemplars"]},
)

az.plot_pair(
    az_data,
    var_names=["mu", "tau", "theta", "noise"],
    # coords=coords,
    divergences=True,
    textsize=25,
)

plt.show()
