// Fit a Bayesian version of the RAPM model
// This file does xG

data{
  // Set-up the context in terms of dataset size
  int<lower = 0> N_shifts; 
  int<lower = 1> N_players;
  // Response variable y
  vector[N_shifts] y;
  // Matrix of player indicators
  matrix[N_shifts, N_players] X_players; 
}

// Now define the parameters we'll consider
parameters{
  // Shift-level variance
  real<lower = 0> sigma_shifts;
  // Player-level variance
  real<lower = 0> sigma_players; 
  // Vector of coefficients for players
  vector[N_players] beta;
}

// And now write out the model
model{
  
  // Observation-level
  y ~ normal(X_players * beta, sigma_shifts);
  
  // Player level effects
  beta ~ normal(0, sigma_players);
  
  // Priors for the variances:
  sigma_shifts ~ normal(0, 5);
  sigma_players ~ normal(0, 5);
}
