data {
  int<lower = 0> N_obs;
  int<lower = 0> N_countries;
  int country[N_obs];
  int new_cases[N_obs];
  int old_cases[N_obs];
  int recovered[N_obs];
  int deaths[N_obs];
  vector[N_countries] pop;
  
  int<lower = 0> start_value;
  int<lower = 0> simulation_days;
  int<lower = 0> simulation_country;
}

transformed data {
  vector[N_obs] old_cases_vec = to_vector(old_cases);
}

parameters {
  real log_mu_growth;
  real<lower = 0> sigma_sq_growth;
  vector[N_countries] log_growth;
  
  real logit_mu_recovery;
  real<lower = 0> sigma_sq_recovery;
  vector[N_countries] logit_recovery;
  
  real logit_mu_mortality;
  real<lower = 0> sigma_sq_mortality;
  vector[N_countries] logit_mortality;
} 

transformed parameters {
  vector<lower = 0>[N_countries] growth = exp(log_growth);
  vector<lower = 0, upper = 1>[N_countries] recovery = inv_logit(logit_recovery);
  vector<lower = 0, upper = 1>[N_countries] mortality = inv_logit(logit_mortality);
  
  real<lower = 0, upper = 1> mu_recovery = inv_logit(logit_mu_recovery);
  real<lower = 0> mu_growth = exp(log_mu_growth);
  real<lower = 0, upper = 1> mu_mortality = inv_logit(logit_mu_mortality);
  
}

model {
  
  sigma_sq_growth ~ inv_chi_square(1);
  sigma_sq_recovery ~ inv_chi_square(1);
  sigma_sq_mortality ~ inv_chi_square(1);
  
  log_growth ~ normal(log_mu_growth, sigma_sq_growth);
  logit_recovery ~ normal(logit_mu_recovery, sigma_sq_recovery);
  logit_mortality ~ normal(logit_mu_mortality, sigma_sq_mortality);
  
  new_cases ~ poisson(growth[country] .* old_cases_vec);
  recovered ~ binomial(old_cases, recovery[country]);
  deaths ~ binomial(old_cases, mortality[country]);
}

generated quantities {
  int<lower = 0> S[simulation_days];
  int<lower = 0> I[simulation_days];
  int<lower = 0> R[simulation_days];
  
  int<lower = 0> pred_cases[simulation_days];
  int<lower = 0> pred_recovered[simulation_days];
  int<lower = 0> pred_deaths[simulation_days];
  
  pred_recovered[1] = binomial_rng(start_value, recovery[simulation_country]);
  pred_deaths[1] = binomial_rng(start_value, mortality[simulation_country]);
  pred_cases[1] = start_value + poisson_rng(growth[simulation_country] * start_value) - pred_recovered[1] - pred_deaths[1];
  
  for (i in 2:simulation_days) {
    pred_recovered[i] = binomial_rng(pred_cases[i - 1], recovery[simulation_country]);
    pred_deaths[i] = binomial_rng(pred_cases[i - 1], mortality[simulation_country]);
    pred_cases[i] = pred_cases[i - 1] + poisson_rng(growth[simulation_country] * pred_cases[i - 1]) - pred_recovered[i] - pred_deaths[i];
  }
}

