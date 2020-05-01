data {
  int<lower = 0> N_days;
  real<lower = 0> N;
  vector<lower = 0>[N_days] S;
  vector<lower = 0>[N_days] I;
  int<lower = 0> dI_dt[N_days];
  
  
  real<lower=0> days[N_days];
  vector[N_days] days_since_first_ban;
}

transformed data {
  vector[N_days] log_S = log(S);
  vector[N_days] log_I = log(I);
  vector[N_days] log_N = rep_vector(log(N), N_days);
}


parameters {
  vector[N_days] eta;
  
  real<lower = 0> rho;
  real<lower = 0> alpha;
  
  real log_beta_intercept;
  
  real first_ban_effect;
  real<lower = 0> ban_speed;
  
  real<lower = 0> phi_inv_sqrt;
  real<lower = 0> infectious_period;
  
} 

transformed parameters {
  vector[N_days] log_beta;
  vector<lower = 0>[N_days] beta;
  vector[N_days] ban_var = first_ban_effect * (1 - exp(-days_since_first_ban * ban_speed));
  vector[N_days] f;
  matrix[N_days, N_days] L_K;
  matrix[N_days, N_days] K = cov_exp_quad(days, alpha, rho);
  real<lower = 0> phi = inv(square(phi_inv_sqrt));
  
  // diagonal elements
  for (n in 1:N_days) {
    K[n, n] = K[n, n] + 1e-4;
  }
  
  L_K = cholesky_decompose(K);
  f = L_K * eta;
  
  log_beta = log_beta_intercept + ban_var + f;
  beta = exp(log_beta);
}


model {
  rho ~ inv_gamma(2, 2);
  alpha ~ normal(0, 4);
  eta ~ std_normal();
  
  log_beta_intercept ~ normal(-1.5, 1);
  
  first_ban_effect ~ normal(0, 2);
  ban_speed ~ gamma(2, 50);
  
  infectious_period ~ gamma(30, 2.5);
  phi_inv_sqrt ~ exponential(1);
  
  
  dI_dt ~ neg_binomial_2_log(log_beta + log_I + log_S - log_N, phi);
  
  
}

generated quantities {
  vector<lower = 0>[N_days] r = beta * infectious_period;
  real<lower = 0> r0 = exp(log_beta_intercept) * infectious_period;
  vector[N_days] pred_beta = log_beta_intercept + f;
  int pred_dI[N_days] = neg_binomial_2_log_rng(pred_beta + log_I + log_S - log_N, phi);
}



