data {
  int<lower = 0> N_obs;
  int<lower = 0> N_countries;
  int cases[N_obs];
  vector[N_obs] days;
  int country[N_obs];
  vector[N_countries] pop;
}

parameters {
  vector[N_countries] alpha;
  vector[N_countries] beta;
  
  real<lower = 0> phi;
  
  real mu_alpha;
  real<lower = 0> sigma_sq_alpha;
  real mu_beta;
  real<lower = 0> sigma_sq_beta;
}

model {
  vector[N_obs] rate;
  alpha ~ normal(mu_alpha, sigma_sq_alpha);
  beta ~ normal(mu_beta, sigma_sq_beta);
  
  rate = alpha[country] + beta[country] .* days;
  
  cases ~ neg_binomial_2(exp(rate) .* pop[country], phi);
}