data {
    int<lower=1> M; // total number of noisy samples
    int<lower=1> K; // number of y's (exemplars)
    int z[M]; // noisy samples (rows are features, columns are samples)
    int exemplar_idx[M]; // list of indices of size M

    // hyper priors
    real alpha;
    real beta;

    real<lower=0, upper=1> noise_theta;


}
parameters {
    real<lower=0, upper=1> theta;
    real y[K];

}
transformed parameters{
    
}
model {

        theta ~ beta(alpha, beta);

        // loop through y's
        for (k in 1:K){
            y[k] ~ bernoulli(theta);
        }

        // multiple z observations
        for (m in 1:M){
            z[m] ~ fabs(y[exemplar_idx[m]] - bernoulli(noise_theta));

        }
    
}