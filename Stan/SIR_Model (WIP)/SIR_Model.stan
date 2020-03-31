data {
  int<lower = 0> N_days;
  int<lower = 0> N_countries;
  int N[N_days, N_countries];
  int S[N_days, N_countries];
  int I[N_days, N_countries];
  int new_cases[N_days, N_countries];
  
  real<lower = 0> a_gamma;
  real<lower = 0> b_gamma;
}


parameters {
  matrix[N_days, N_countries] log_beta;
  
  
  real<lower = 0> mu_log_beta;
  real<lower = 0> sigma_log_beta;
  
  
  matrix[N_days, N_countries] trend_log_beta;
  real mu_trend_log_beta;
  real<lower = 0> sigma_trend_log_beta;
  
  row_vector[N_countries] trend2_log_beta;
  real mu_trend2_log_beta;
  real<lower = 0> sigma_trend2_log_beta;
  
  vector<lower = 0>[N_countries] sigma_state;
  real<lower = 0> mu_sigma_state;
  
  real<lower = 0> gamma;
  
} 

transformed parameters {
  matrix[N_days, N_countries] beta = exp(log_beta);
}

model {
  
  gamma ~ gamma(a_gamma, b_gamma);
  
  sigma_trend2_log_beta ~ exponential(1);
  trend2_log_beta ~ normal(mu_trend2_log_beta, sigma_trend2_log_beta);
  
  sigma_trend_log_beta ~ exponential(1);
  trend_log_beta[1, ] ~ normal(mu_trend_log_beta, sigma_trend_log_beta);
  
  sigma_log_beta ~ exponential(1);
  log_beta[1, ] ~ normal(mu_log_beta, sigma_log_beta);
  
  sigma_state ~ exponential(mu_sigma_state);
  
  for(i in 1:N_countries) {
    new_cases[1, i] ~ poisson(beta[1, i] * I[1, i] * S[1, i] / N[1, i]);
    for (t in 2:N_days) {
      trend_log_beta[t, i] ~ normal(trend_log_beta[t - 1, i] + trend2_log_beta[i], sigma_trend_log_beta);
      log_beta[t, i] ~ normal(log_beta[t - 1, i] + trend_log_beta[t, i], sigma_state[i]);
      new_cases[t, i] ~ poisson(beta[t, i] * I[t, i] * S[t, i] / N[t, i]);
    }
  }
}


generated quantities {
  matrix[N_days, N_countries] r0 = beta * gamma;
}


