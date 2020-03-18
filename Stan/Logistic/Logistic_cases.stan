data {
  int<lower = 0> N_obs;
  int<lower = 0> N_countries;
  vector[N_obs] cases;
  vector[N_obs] days;
  int country[N_obs];
  vector[N_countries] pop;
  vector[N_countries] country_max;
  vector[N_countries] country_min;
  real<lower = 0> beta_scale[N_countries];
}

transformed data {
  real<lower = 0> scale[N_countries];
  for (i in 1:N_countries) scale[i] = inv(beta_scale[i]);
}

parameters {
  // vector[N_countries] alpha;
  vector[N_countries] beta;
  vector<lower = 0, upper = 1>[N_countries] maximum_pre;
  
  
  real mu_beta;
  real<lower = 0> sigma_sq_beta;
  
  real<lower = 0> sigma_sq_obs[N_countries];
}

transformed parameters {
  vector[N_countries] maximum = maximum_pre .* (pop - country_max - 1) + country_max + 1;
}

model {
  vector[N_obs] linear = log(cases ./ (maximum[country] - cases));
  vector[N_obs] intercept = log(country_min[country] ./ (maximum[country] - country_min[country]));
  
  sigma_sq_beta ~ inv_chi_square(1);
  sigma_sq_obs ~ inv_chi_square(1);
  
  for (i in 1:N_countries) maximum_pre[i] ~ beta(country_max[i] * scale[i], pop[i] * scale[i]);
  beta ~ normal(mu_beta, sigma_sq_beta);
  
  
  linear ~ normal(intercept + beta[country] .* days, sigma_sq_obs[country]);
  
}

