data {
  int<lower = 0> N_days;
  int<lower = 0> N;
  int<lower = 0> S[N_days];
  int<lower = 0> I[N_days];
  int<lower = 0> dI_dt[N_days];
  
  
  real<lower=0> days[N_days];
}


parameters {
  vector[N_days] log_beta;
  vector[N_days] eta;
  real<lower = 0> sigma_beta;
  real<lower = 0> rho;
  real<lower = 0> alpha;
  
  
  
  real<lower = 0> serialinterval;
  
  real<lower = 0> sigma_obs;
  real<lower = 0> phi_inv_sqrt;
} 

transformed parameters {
  vector<lower = 0>[N_days] beta = exp(log_beta);
  real<lower = 0> phi_inv = square(phi_inv_sqrt);
  real<lower = 0> phi = inv(phi_inv);
}


model {
  matrix[N_days, N_days] L_K;
  matrix[N_days, N_days] K = cov_exp_quad(days, alpha, rho);
  vector[N_days] f;
  
  for (i in 1:N_days) {
    K[i, i]  = K[i, i] + 1e-9;
  }
  
  L_K = cholesky_decompose(K);
  
  f = L_K * eta;
  
  rho ~ inv_gamma(5, 5);
  alpha ~ exponential(1);
  sigma_beta ~ exponential(1);
  eta ~ std_normal();
  
  phi_inv_sqrt ~ exponential(1);
  
  
  log_beta ~ normal(f, sigma_beta);
  serialinterval ~ gamma(30, 2.5);
  
  dI_dt[1] ~ neg_binomial_2(beta[1] * I[1] * S[1] / N, phi);
  
  for (t in 2:N_days) {
    dI_dt[t] ~ neg_binomial_2(beta[t] * I[t] * S[t] / N, phi);
  }
  
  
}

generated quantities {
  vector<lower = 0>[N_days] r0 = beta * serialinterval;
}



