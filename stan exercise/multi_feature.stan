data {
    int<lower=1> F; // number of features
    int<lower=1> M; // total number of noisy samples
    int<lower=1> K; // number of y's (exemplars)
    matrix[F, M] z; // noisy samples (rows are features, columns are samples)

    int<lower=1> exemplar_idx[M]; // list of indices of size M

    // if i want to change epsilon, put it here

}
parameters {
    vector[F] mu;
    vector<lower=0>[F] sigma;
    matrix[F, K] y;
}
model {
    for (f in 1:F){

        mu[f] ~ normal(0,4);
        sigma[f] ~ gamma(1,1);

        // loop through y's
        for (k in 1:K){
            y[f, k] ~ normal(mu[f], sigma[f]);
        }

        // multiple z observations
        for (m in 1:M){
            z[f, m] ~ normal(y[f, exemplar_idx[m]], 0.5);
        }
    } 



}