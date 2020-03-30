data {
  int<lower = 0> N_obs;
  int<lower = 0> N_countries;
  int country[N_obs];
  int N[N_obs];
  int S[N_obs];
  int I[N_obs];
  int R[N_obs];
  int new_cases[N_obs];
  int new_deaths[N_obs];
  int new_recovered[N_obs];
}


parameters {
  vector<lower = 0>[N_countries] beta;
  vector<lower = 0>[N_countries] gamma;
  vector<lower = 0>[N_countries] mu;
  
  real<lower = 0> mu_beta;
  real<lower = 0> sigma_beta;
  real<lower = 0> mu_gamma;
  real<lower = 0> sigma_gamma;
  real<lower = 0> mu_mu;
  real<lower = 0> sigma_mu;
  
} 

model {
  
  sigma_beta ~ exponential(1);
  sigma_gamma ~ exponential(1);
  sigma_mu ~ exponential(1);
  
  beta ~ normal(mu_beta, sigma_beta);
  gamma ~ normal(mu_gamma, sigma_gamma);
  mu ~ normal(mu_mu, sigma_mu);
  
  for (t in 1:N_obs) {
    new_cases[t] ~ poisson(beta[country[t]] * I[t] * S[t] / N[t]);
    new_recovered[t] ~ poisson(gamma[country[t]] * I[t]);
    new_deaths[t] ~ poisson(mu[country[t]] * I[t]);
  }
  
    
}


generated quantities {
  vector<lower = 0>[N_countries] r0 = beta ./ (gamma + mu);
  vector<lower = 0>[N_countries] infectious_period = inv(gamma + mu);
}