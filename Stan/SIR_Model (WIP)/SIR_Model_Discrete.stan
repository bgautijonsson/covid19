data {
  int<lower = 0> N_days;
  real<lower = 0> N;
  real<lower = 0> S[N_days];
  real<lower = 0> I[N_days];
  int<lower = 0> dI_dt[N_days];
  
  
  real<lower=0> days[N_days];
}

transformed data {
  real<lower = 0> log_S[N_days] = log(S);
  real<lower = 0> log_I[N_days] = log(I);
  real<lower = 0> log_dIdt[N_days] = log(dI_dt);
  real<lower = 0> log_N = log(N);
}


parameters {
  vector[N_days] log_beta;
  vector[N_days] eta;
  real<lower = 0> sigma_beta;
  real<lower = 0> rho;
  real<lower = 0> alpha;
  
  
  
  real<lower = 0> serialinterval;
  
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
    K[i, i]  = K[i, i] + square(sigma_beta);
  }
  
  L_K = cholesky_decompose(K);

  
  rho ~ inv_gamma(5, 5);
  alpha ~ std_normal();
  sigma_beta ~ std_normal();
  eta ~ std_normal();
  
  phi_inv_sqrt ~ std_normal();
  
  
  log_beta ~ multi_normal_cholesky(rep_vector(0, N_days), L_K);
  serialinterval ~ gamma(30, 2.5);
  
  dI_dt[1] ~ neg_binomial_2_log(log_beta[1] + log_I[1] + log_S[1] - log_N, phi);
  
  for (t in 2:N_days) {
    dI_dt[t] ~ neg_binomial_2_log(log_beta[t] + log_I[t] + log_S[t] - log_N, phi);
  }
  
  
}

generated quantities {
  vector<lower = 0>[N_days] r0 = beta * serialinterval;
}



