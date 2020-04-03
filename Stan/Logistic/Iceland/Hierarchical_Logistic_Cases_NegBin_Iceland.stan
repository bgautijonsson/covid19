data {
  int<lower = 0> N_obs;
  int<lower = 0> N_preds;
  vector[N_obs] days;
  int new_cases[N_obs];
  vector[N_preds] pred_days;
  int<lower = 0> pop;
}

parameters {
  real<lower = 0> beta;
  real alpha;
  real<lower = 0, upper = 1> S;
  
  
  real<lower = 0, upper = 1> mu_s;
  real<lower = 0> kappa_s;
  
  real<lower = 0> phi_inv_sqrt;
  
}

transformed parameters {
  vector[N_obs] linear = alpha + beta * days;
  vector<lower = 0>[N_obs] difference;
  real<lower = 0> phi_inv = square(phi_inv_sqrt);
  real<lower = 0> phi = inv(phi_inv);
  real<lower = 0> a_s = mu_s * kappa_s;
  real<lower = 0> b_s = (1 - mu_s) * kappa_s;
  
  for (i in 1:N_obs) {
    difference[i] = beta * S * exp(-linear[i]) / square(exp(-linear[i]) + 1);
  }
}

model {
  alpha ~ normal(-3, 1);
  
  beta ~ lognormal(log(0.1), 0.1);
  
  
  mu_s ~ beta(1, 99);
  kappa_s ~ exponential(0.001);
  S ~ beta(a_s, b_s);
  
  
  phi_inv_sqrt ~ exponential(1);
  
  new_cases ~ neg_binomial_2(difference * pop, phi);
}

generated quantities {
  int pred_cases[N_preds];
  vector[N_preds] linear_pred;
  vector[N_preds] difference_pred;
  
  for (i in 1:N_obs) {
    linear_pred[i] = alpha + beta * pred_days[i];
    difference_pred[i] = beta * S * exp(-linear_pred[i]) / square(exp(-linear_pred[i]) + 1);
  }
  
  for (i in (N_obs + 1):N_preds) {
    linear_pred[i] = alpha + beta * pred_days[i];
    difference_pred[i] = beta * S * exp(-linear_pred[i]) / square(exp(-linear_pred[i]) + 1);
  }
  
  pred_cases = neg_binomial_2_rng(difference_pred * pop, phi);
}

