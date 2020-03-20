data {
  int<lower = 0> N_obs;
  int<lower = 0> N_countries;
  vector[N_obs] case_rate;
  vector[N_obs] cases;
  vector[N_obs] days;
  int country[N_obs];
  vector[N_countries] pop;
  vector[N_countries] country_max;
  vector[N_countries] country_max_cases;
  vector[N_countries] country_min_case_rate;
  vector[N_countries] country_n_days;
  int<lower = 0> max_case_rate;
  real<lower = 0> beta_scale[N_countries];
}

transformed data {
  real<lower = 0> scale[N_countries];
  for (i in 1:N_countries) scale[i] = inv(beta_scale[i]);
}

parameters {
  vector[N_countries] beta;
  vector<lower = 0, upper = 1>[N_countries] maximum_pre;
  
  real mu_beta;
  real<lower = 0> sigma_sq_beta;
  
  vector<lower = 0>[N_countries] sigma_sq_obs;
}

transformed parameters {
  vector[N_countries] maximum = maximum_pre .* (max_case_rate - country_max) + country_max;
}

model {
  vector[N_obs] linear = log(case_rate ./ (maximum[country] - case_rate));
  vector[N_obs] intercept = log(country_min_case_rate[country] ./ (maximum[country] - country_min_case_rate[country]));
  

  sigma_sq_beta ~ inv_chi_square(1);
  sigma_sq_obs ~ inv_chi_square(1);
  
  for (i in 1:N_countries) maximum_pre[i] ~ beta(country_max_cases[i] * scale[i], pop[i] * scale[i]);
  
  beta ~ normal(mu_beta, sigma_sq_beta);
  
  
  linear ~ normal(intercept + beta[country] .* days, sigma_sq_obs[country]);
  
}

