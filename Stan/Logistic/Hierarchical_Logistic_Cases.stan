data {
  int<lower = 0> N_obs;
  vector[N_obs] obs_case_rate;
  vector[N_obs] days;
  int country[N_obs];
  int obs_cases[N_obs];
  
  
  int<lower = 0> N_countries;
  vector[N_countries] pop;
  vector[N_countries] country_max;
  int<lower = 0> max_case_rate;
}

parameters {
  vector[N_countries] beta;
  vector[N_countries] alpha;
  vector<lower = 0, upper = 1>[N_countries] maximum_pre;
  
  real mu_beta;
  real<lower = 0> sigma_sq_beta;
  
  real mu_alpha;
  real<lower = 0> sigma_sq_alpha;
  
  vector<lower = 0>[N_countries] sigma_sq_obs;
  real<lower = 0> a_sigma_obs;
  real<lower = 0> b_sigma_obs;
  
  real<lower = 0> beta_a;
  real<lower = 0> beta_b;
  
  vector<lower = 0>[N_countries] phi;
  real<lower = 0> mu_phi;
}

transformed parameters {
  vector[N_countries] maximum = maximum_pre * 1000;
  // vector<lower = 0>[N_countries] phi = inv(phi_inv);
  vector[N_obs] linear = alpha[country] + beta[country] .* days;
  vector<lower = 0, upper = 1000>[N_obs] true_rate;
  for (i in 1:N_obs) true_rate[i] = maximum[country[i]] .* 1 / (1 + exp(-linear[i]));
}

model {
  
  sigma_sq_beta ~ inv_chi_square(1);
  sigma_sq_alpha ~ inv_chi_square(1);
  sigma_sq_obs ~ inv_gamma(a_sigma_obs, b_sigma_obs);
  
  maximum_pre ~ beta(beta_a, beta_b);
  
  beta ~ normal(mu_beta, sigma_sq_beta);
  alpha ~ normal(mu_alpha, sigma_sq_alpha);
  
  phi ~ exponential(mu_phi);
  
  // case_rate ~ normal(true_rate, sigma_sq_obs[country]);
  obs_cases ~ neg_binomial_2(true_rate .* pop[country], phi[country]);
  
}


