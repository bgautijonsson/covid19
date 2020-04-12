data {
  int<lower = 0> N_days;
  int<lower = 0> N_countries;
  int new_cases[N_days, N_countries];
  
  vector[N_countries] pop;
  vector[N_countries] min_rate;
}

parameters {
  matrix<lower = 0>[N_days, N_countries] beta;
  vector[N_countries] alpha;
  vector<lower = 0, upper = 1>[N_countries] S;
  
  real<lower = 0> mu_beta;
  real<lower = 0> sigma_beta;
  real<lower = 0> beta_walk;
  
  // real mu_alpha;
  real<lower = 0> sigma_alpha;
  
  real<lower = 0, upper = 1> mu_s;
  real<lower = 0> kappa_s;
  
  vector<lower = 0>[N_countries] phi_inv_sqrt;
  real<lower = 0> sigma_phi_inv_sqrt;
  
}

transformed parameters {
  matrix[N_days, N_countries] linear;
  matrix<lower = 0>[N_days, N_countries] difference;
  vector[N_countries] intercept = log(S ./ (S - min_rate));
  vector<lower = 0>[N_countries] phi_inv = square(phi_inv_sqrt);
  vector<lower = 0>[N_countries] phi = inv(phi_inv);
  real<lower = 0> a_s = mu_s * kappa_s;
  real<lower = 0> b_s = (1 - mu_s) * kappa_s;
  
  for (t in 1:N_days) {
    for (i in 1:N_countries) {
      linear[t, i] = alpha[i] + beta[t, i] * (t - 1);
      difference[t, i] = beta[t, i] * S[i] * exp(-linear[t, i]) / square(exp(-linear[t, i]) + 1);
    }
  }
}

model {
  
  sigma_alpha ~ exponential(1);
  alpha ~ normal(intercept, sigma_alpha);
  
  mu_beta ~ normal(0.1, 0.1);
  sigma_beta ~ exponential(1);
  beta[1, ] ~ normal(mu_beta, sigma_beta);
  beta_walk ~ exponential(10);
  
  mu_s ~ beta(1, 99);
  kappa_s ~ exponential(0.01);
  S ~ beta(a_s, b_s);
  
  
  phi_inv_sqrt ~ exponential(sigma_phi_inv_sqrt);
  sigma_phi_inv_sqrt ~ exponential(1);
  
  for (i in 1:N_countries) {
    new_cases[1, i] ~ neg_binomial_2(difference[1, i] * pop[i], phi[i]);
    for (t in 2:N_days) {
      beta[t, i] ~ normal(beta[t - 1, i], beta_walk);
      new_cases[t, i] ~ neg_binomial_2(difference[t, i] * pop[i], phi[i]);
    }
  }
}

