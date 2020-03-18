data {
  int<lower = 0> N_obs;
  int<lower = 0> N_preds;
  vector[N_obs] cases;
  vector[N_obs] days;
  vector[N_preds] pred_days;
}

parameters {
  real alpha;
  real beta;
  real<lower = 0, upper = 1> maximum_pre;
  
  
  real<lower = 0> sigma_sq_obs;
}

transformed parameters {
  real maximum = maximum_pre .* (300000 - 220) + 220;
}

model {
  vector[N_obs] linear = log(cases ./ (maximum - cases));
  // vector[N_obs] intercept = log(country_min[country] ./ (maximum[country] - country_min[country]));
  
  // sigma_sq_beta ~ inv_chi_square(1);
  sigma_sq_obs ~ inv_chi_square(1);
  
  // for (i in 1:N_countries) maximum_pre[i] ~ beta(country_max[i] * scale[i], pop[i] * scale[i]);
  // beta ~ normal(mu_beta, sigma_sq_beta);
  
  
  linear ~ normal(alpha + days * beta, sigma_sq_obs);
}

generated quantities {
  vector[N_preds] pred_linear;
  vector[N_preds] pred_cases;
  
  for (i in 1:N_preds) {
    pred_linear[i] = alpha + pred_days[i] * beta;
    pred_cases[i] = maximum * 1 / (1 + exp(-pred_linear[i]));
  }
}

