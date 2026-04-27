data{
  int<lower = 0> N_shifts; 
  int<lower = 1> N_players;
  vector[N_shifts] y;
  matrix[N_shifts, N_players] X_players; 
  vector<lower=0>[N_shifts] wgts;     // Weights (stint length)
  vector[N_players] prior_means;      // Means from ES posteriors
}

parameters{
  real<lower = 0> sigma_shifts;
  real<lower = 0> sigma_players; 
  vector[N_players] beta;
}

model{
  // 1. Weighted Observation-level likelihood
  y ~ normal(X_players * beta, sigma_shifts ./ sqrt(wgts));
  
  // 2. Player level effects with informative prior mean
  beta ~ normal(prior_means, sigma_players);
  
  // 3. Priors for the variances:
  sigma_shifts ~ normal(0, 5);
  sigma_players ~ normal(0, 5);
}
