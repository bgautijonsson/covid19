data {
  int<lower = 0> N_obs;
  int<lower = 0> N_preds;
  int<lower = 0> Upper_Limit;
  int<lower = 0> Max_Cases;
  int<lower = 0> a_beta;
  int<lower = 0> b_beta;
  vector<lower = 0>[N_obs] cases;
  vector[N_obs] days;
  vector[N_preds] pred_days;
}

parameters {
  real alpha;
  real beta;
  
  
  real<lower = 0> sigma;
}

model {
  for (i in 1:N_obs) cases[i] ~ logistic(alpha + beta * days[i], sigma);
}

generated quantities {
  vector<lower = 0>[N_preds] pred_cases;
  
  for (i in 1:N_preds) pred_cases[i] = logistic_rng(alpha + beta * pred_days[i], sigma);
  
}

