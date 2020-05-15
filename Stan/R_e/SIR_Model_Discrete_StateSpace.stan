data {
  int<lower = 0> N_days;
  real<lower = 0> N;
  
  vector<lower = 0>[N_days] S;
  vector<lower = 0>[N_days] I;
  int<lower = 0> dI_dt[N_days];
  
  real<lower=0> days[N_days];
}

transformed data {
  vector[N_days] log_S = log(S);
  vector[N_days] log_I = log(I);
  vector[N_days] log_N = rep_vector(log(N), N_days);
}


parameters {
  real<lower = 0> phi_inv_sqrt;
  real<lower = 0> infectious_period;

  
  real<lower = 0> sigma_beta;
  
  real log_beta1;
  real log_beta2;
  vector[N_days - 2] error;
} 

transformed parameters {
  vector[N_days] log_beta;
  vector<lower = 0>[N_days] beta;
  real<lower = 0> phi = inv(square(phi_inv_sqrt));
  log_beta[1] = log_beta1;
  log_beta[2] = log_beta2;
  for (t in 3:N_days) {
    log_beta[t] = 2 * log_beta[t - 1] - log_beta[t - 2] + sigma_beta * error[t - 2];
  }
  
  beta = exp(log_beta);
}

model {
  sigma_beta ~ exponential(1);
  error ~ std_normal();
  
  infectious_period ~ gamma(4.95, 0.66);
  phi_inv_sqrt ~ exponential(1);
  
  log_beta1 ~ normal(0, 1);
  log_beta2 ~ normal(0, 1);
  
  
  dI_dt ~ neg_binomial_2_log(log_beta + log_I + log_S - log_N, phi);
  
  
}

generated quantities {
  vector<lower = 0>[N_days] r = beta * infectious_period;
  vector[N_days - 1] slope_log_beta = log_beta[2:N_days] - log_beta[1:(N_days - 1)];
  int<lower = 0> pred_cases[N_days];
  pred_cases = neg_binomial_2_rng(beta .* I .* S / N, phi);
}



