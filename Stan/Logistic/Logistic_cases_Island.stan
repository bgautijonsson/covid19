data {
  int<lower = 0> N_obs;
  int<lower = 0> N_preds;
  int<lower = 0> Upper_Limit;
  int<lower = 0> Max_Cases;
  int<lower = 0> a_beta;
  int<lower = 0> b_beta;
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
  real maximum = maximum_pre .* (Upper_Limit - Max_Cases) + Max_Cases;
  vector[N_obs] linear = log(cases ./ (maximum - cases));
}

model {
  
  // vector[N_obs] intercept = log(country_min[country] ./ (maximum[country] - country_min[country]));
  
  // sigma_sq_beta ~ inv_chi_square(1);
  sigma_sq_obs ~ inv_chi_square(1);
  
  // for (i in 1:N_countries) maximum_pre[i] ~ beta(country_max[i] * scale[i], pop[i] * scale[i]);
  // beta ~ normal(mu_beta, sigma_sq_beta);
  
  maximum_pre ~ beta(a_beta, b_beta);
  
  
  linear ~ normal(alpha + days * beta, sigma_sq_obs);
  
  for (i in 1:N_obs) {
    target += log(maximum) - log((maximum * cases[i] - cases[i]^2));
  }
}

generated quantities {
  vector[N_preds] pred_linear;
  vector[N_preds] pred_cum_cases;
  vector[N_preds] pred_active_cases;
  
  for (i in 1:N_preds) {
    pred_linear[i] = alpha + pred_days[i] * beta;
    pred_cum_cases[i] = maximum * 1 / (1 + exp(-pred_linear[i]));
    if (i > 21) pred_active_cases[i] = pred_cum_cases[i] - pred_cum_cases[i - 21];
    else pred_active_cases[i] = pred_cum_cases[i];
  }
}

