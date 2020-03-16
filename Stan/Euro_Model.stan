data {
  int<lower = 0> N_obs;
  int<lower = 0> N_countries;
  int cases[N_obs];
  vector[N_obs] days;
  int country[N_obs];
  vector[N_countries] pop;
  
  
  int<lower = 0> N_preds;
  vector[N_preds] pred_days;
  int pred_country[N_preds];
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


generated quantities {
  int pred_cases[N_preds];
  vector[N_preds] pred_rate;
  
  pred_rate = alpha[pred_country] + beta[pred_country] .* pred_days;
  
  pred_cases = neg_binomial_2_rng(exp(pred_rate) .* pop[pred_country], phi);
}