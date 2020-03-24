data {
  int<lower = 0> N_obs;
  int country[N_obs];
  vector[N_obs] days;
  int obs_cases[N_obs];
  
  int<lower = 0> N_countries;
  vector[N_countries] pop;
}

parameters {
  vector<lower = 0>[N_countries] beta;
  vector[N_countries] alpha;
  vector<lower = 0, upper = 1>[N_countries] maximum;
  
  real<lower = 0> mu_beta;
  real<lower = 0> sigma_sq_beta;
  
  real mu_alpha;
  real<lower = 0> sigma_sq_alpha;
  
  real<lower = 0> beta_a;
  real<lower = 0> beta_b;
  
  real<lower = 0> phi_inv[N_countries];
  real<lower = 0> mu_phi_inv;
  
}

transformed parameters {
  vector[N_obs] linear = alpha[country] + beta[country] .* days;
  vector<lower = 0, upper = 1>[N_obs] difference;
  real<lower = 0> phi[N_countries] = inv(phi_inv);
  for (i in 1:N_obs) {
    difference[i] = beta[country[i]] * maximum[country[i]] * exp(-linear[i]) / square(exp(-linear[i]) + 1);
  }
}

model {
  
  sigma_sq_beta ~ inv_chi_square(2);
  sigma_sq_alpha ~ inv_chi_square(2);
  
  maximum ~ beta(beta_a, beta_b);
  
  beta ~ normal(mu_beta, sigma_sq_beta);
  alpha ~ normal(mu_alpha, sigma_sq_alpha);
  
  phi_inv ~ exponential(mu_phi_inv);
  
  obs_cases ~ neg_binomial_2(difference .* pop[country], phi[country]);
}


