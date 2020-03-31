
data {
  int<lower = 0> N_days;
  int<lower = 0> N_groups;
  real pop[N_groups];
  int cases[N_days, N_groups];
}

parameters {
  vector[N_days] kappa_pre;
  real kappa0;
  real<lower = 0> sigma_sq_state;
  
  real theta;
  real<lower = 0> sigma_sq_theta;
  
  
  
  vector<lower = 0>[N_groups] alpha_pre;
  
  real<lower = 0> sigma_sq_beta;
  vector[N_groups] beta_pre;
  
  real<lower = 0> a_alpha;
  real<lower = 0> b_alpha;
  
}

transformed parameters {
  vector[N_groups] alpha = log(alpha_pre);
  vector[N_groups] beta = beta_pre - mean(beta_pre);
  vector[N_groups] change =  inv(N_groups) * (1 + beta);
  vector[N_days] kappa = kappa_pre - mean(kappa_pre);
}

model {
  alpha_pre ~ gamma(a_alpha, b_alpha);
  a_alpha ~ cauchy(0, 1);
  b_alpha ~ cauchy(0, 1);
  
  sigma_sq_beta ~ inv_chi_square(1);
  beta ~ normal(0, sigma_sq_beta);
  
  sigma_sq_theta ~ inv_chi_square(1);
  theta ~ normal(0, sigma_sq_theta);
  
  sigma_sq_state ~ inv_chi_square(1);
  kappa[1] ~ normal(kappa0, sigma_sq_state);
  
  cases[1, ] ~ poisson_log(alpha + change * kappa[1] + log(to_vector(pop)));
  
  for (t in 2:N_days) {
    kappa[t] ~ normal(kappa[t - 1] + theta, sigma_sq_state);
    cases[t, ] ~ poisson_log(alpha + change * kappa[t] + log(to_vector(pop)));
  }
}

