# Import libraries.
using Turing, DynamicHMC, StatsPlots, Random, Distributions, StatsBase, LinearAlgebra

# Set the priors on mean and SD of the concept

# mu
μ_mean = 0.5
μ_sd = 1

# sd
σ_alpha = 1
σ_beta = 1

# noise SD
ϵ = 0.5

# Set up data from a Bernoulli distribution, i.e. draw heads or tails.
num_samples = 100
num_exemplars = 5
num_features = 2

# Random initialization
Random.seed!(1)
idx_weights = fill(1/num_exemplars, num_exemplars)

# generate Data
exemplar_means = [0.8, 5.2, 1.2, 1.5, -0.4, 0.8];

# multifeature means (currently same across features)
exemplar_means = [fill(mean, num_features) for mean in exemplar_means];

# check that there are enough exemplar means
if length(exemplar_means) < num_exemplars
    error("exemplar means less than num_exemplars")
end

exemplar_idx = sample(1:num_exemplars, ProbabilityWeights(idx_weights), num_samples) 

# generate the data
data = [rand(MultivariateNormal(exemplar_means[idx], μ_sd)) for idx in exemplar_idx]

# construct dict for data argument:
model_input = Dict(:μ_mean => μ_mean,
                   :μ_sd => μ_sd,
                   :σ_alpha => σ_alpha,
                   :σ_beta => σ_beta,
                   :ϵ => ϵ,
                   :num_samples=>num_samples, 
                   :num_exemplars=>num_exemplars, 
                   :num_features=>num_features, 
                   :cats=>exemplar_idx, 
                   :data=>data);


# Declare our Turing model.
@model function noisy_concept_learning(model_input)

    μ = Vector{Float64}(undef, model_input[:num_features])
    σ = Vector{Float64}(undef, model_input[:num_features])
    
    # loop through feautures
    for f in 1:model_input[:num_features]

        μ[f] ~ Normal(model_input[:μ_mean], model_input[:μ_sd])
        σ[f] ~ Gamma(model_input[:σ_alpha],model_input[:σ_beta])

        y = Matrix{Float64}(undef, model_input[:num_features], model_input[:num_exemplars])

        # loop through y's
        for i in 1:model_input[:num_exemplars]
            y[f, i] ~ Normal(μ[f], σ[f])
        end
        
        # loop through z's
        for i in 1:model_input[:num_samples]
            model_input[:data][i][f] ~ Normal(y[model_input[:cats][i]], model_input[:ϵ])
        end

    end
    
    return μ, σ
end

# Settings of the Hamiltonian Monte Carlo (HMC) sampler.
iterations = 10000
ϵ = 0.05
τ = 10

# Start sampling.
chain = sample(noisy_concept_learning(model_input), DynamicNUTS(), iterations)

# Plot a summary of the sampling process for the parameter p, i.e. the probability of heads in a coin.
p = plot(chain)
