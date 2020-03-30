data {
  int<lower = 0> N_obs;
  int country[N_obs];
  vector[N_obs] days;
  int new_cases[N_obs];
  
  int<lower = 0> N_countries;
  int<lower = 0> total_cases[N_countries];
  int<lower = 0> total_deaths[N_countries];
  vector[N_countries] pop;
}

parameters {
  vector<lower = 0>[N_countries] beta;
  vector[N_countries] alpha;
  vector<lower = 0, upper = 1>[N_countries] S;
  
  real<lower = 0> mu_beta;
  real<lower = 0> sigma_beta;
  
  real mu_alpha;
  real<lower = 0> sigma_alpha;
  
  real<lower = 0, upper = 1> mu_s;
  real<lower = 0> kappa_s;
  
  vector<lower = 0>[N_countries] phi_inv_sqrt;
  real<lower = 0> sigma_phi_inv_sqrt;
  
}

transformed parameters {
  vector[N_obs] linear = alpha[country] + beta[country] .* days;
  vector<lower = 0>[N_obs] difference;
  vector<lower = 0>[N_countries] phi_inv = square(phi_inv_sqrt);
  vector<lower = 0>[N_countries] phi = inv(phi_inv);
  real<lower = 0> a_s = mu_s * kappa_s;
  real<lower = 0> b_s = (1 - mu_s) * kappa_s;

  for (i in 1:N_obs) {
    difference[i] = beta[country[i]] * S[country[i]] * exp(-linear[i]) / square(exp(-linear[i]) + 1);
  }
}

model {
  
  mu_alpha ~ normal(-3.6, 0.5);
  sigma_alpha ~ exponential(1);
  alpha ~ normal(mu_alpha, sigma_alpha);
  
  mu_beta ~ normal(0.1, 0.05);
  sigma_beta ~ exponential(2);
  beta ~ normal(mu_beta, sigma_beta);
  
  mu_s ~ beta(2, 200);
  kappa_s ~ cauchy(0, 5);
  S ~ beta(a_s, b_s);
  
  
  phi_inv_sqrt ~ exponential(sigma_phi_inv_sqrt);
  sigma_phi_inv_sqrt ~ exponential(1);
  
  new_cases ~ neg_binomial_2(difference .* pop[country], phi[country]);
}

