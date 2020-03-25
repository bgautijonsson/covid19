data {
  int<lower = 0> N_obs;
  int country[N_obs];
  vector[N_obs] days;
  int new_cases[N_obs];
  int total_cases[N_obs];
  int<lower = 0> N_countries;
  vector[N_countries] pop;
}

parameters {
  vector<lower = 0>[N_countries] beta;
  vector[N_countries] alpha;
  vector<lower = 0, upper = 1>[N_countries] maximum;
  
  real<lower = 0> mu_beta;
  real<lower = 0> sigma_beta;
  
  real mu_alpha;
  real<lower = 0> sigma_alpha;
  
  real<lower = 0> beta_a;
  real<lower = 0> beta_b;
  
  // vector<lower = 0>[N_countries] phi_inv;
  // real<lower = 0> mu_phi_inv;
  
}

transformed parameters {
  vector[N_obs] linear = alpha[country] + beta[country] .* days;
  // vector<lower = 0, upper = 1>[N_obs] rate;
  vector<lower = 0, upper = 1>[N_obs] difference;
  real<lower = 0> sigma_sq_alpha = square(sigma_alpha);
  real<lower = 0> sigma_sq_beta = square(sigma_beta);
  // vector<lower = 0>[N_countries] phi = inv(phi_inv);
  for (i in 1:N_obs) {
    difference[i] = beta[country[i]] * maximum[country[i]] * exp(-linear[i]) / square(exp(-linear[i]) + 1);
    // rate[i] = maximum[country[i]] / (1 + exp(-linear[i]));
  }
}

model {
  
  sigma_beta ~ chi_square(2);
  sigma_alpha ~ chi_square(2);
  
  mu_beta ~ normal(0.2, 0.1);
  mu_alpha ~ normal(-3, 0.5);
  
  maximum ~ beta(beta_a, beta_b);
  
  beta ~ normal(mu_beta, sigma_sq_beta);
  alpha ~ normal(mu_alpha, sigma_sq_alpha);
  
  // phi_inv ~ exponential(mu_phi_inv);
  // mu_phi_inv ~ exponential(1);
  
  // new_cases ~ neg_binomial_2(difference .* pop[country], phi[country]);
  new_cases ~ poisson(difference .* pop[country]);
  // total_cases ~ poisson(rate .* pop[country]);
}

