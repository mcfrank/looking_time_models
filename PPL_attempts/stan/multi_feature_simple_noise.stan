data {
    int<lower=1> F; // number of features
    int<lower=1> M; // total number of noisy samples
    int<lower=1> K; // number of y's (exemplars)
    matrix[F, M] z; // noisy samples (rows are features, columns are samples)

    int<lower=1> exemplar_idx[M]; // list of indices of size M

    // hyper priors
    real mu_mean;
    real<lower=0> mu_sd;

    real<lower=0> sigma_alpha;
    real<lower=0> sigma_beta;

    real<lower=0> noise;

}
parameters {
    vector[F] mu;
    vector<lower=0>[F] sigma;
    matrix[F, K] y;

}
model {

    // loop through features
    for (f in 1:F){

        sigma[f] ~ gamma(sigma_alpha, sigma_beta);
        mu[f] ~ normal(mu_mean, mu_sd);


        // loop through y's
        for (k in 1:K){
            y[f, k] ~ normal(mu[f], sigma[f]);
        }

        // multiple z observations
        for (m in 1:M){
            z[f, m] ~ normal(y[f, exemplar_idx[m]], noise);
        }
    }
}
generated quantities {

vector[F] z_rep;

for (f in 1:F){
        z_rep[f] = y[f, K] + normal_rng(0, noise);
 }

}
