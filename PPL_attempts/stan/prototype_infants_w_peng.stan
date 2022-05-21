data {
    int<lower=1> M; // total number of noisy samples
    int<lower=1> K; // number of y's (exemplars)
    real z[M]; // noisy samples
    int<lower=1> exemplar_idx[M]; // list of indices of size M
    exemplar_idx = [1 1 2 2]
    z =[1 2 3 4]
}
parameters {
    real mu;
    real<lower=0> sigma;
    real y[K];
}
model {
    mu ~ normal(0, 1);
    sigma ~ gamma(1, 1);

    // loop through y's
    for (k in 1:K){
        y[k] ~ normal(mu, sigma);
    }
    
    // multiple z observations
    for (m in 1:M){
        z[m] ~ normal(y[exemplar_idx[m]], 0.5);
    }

}