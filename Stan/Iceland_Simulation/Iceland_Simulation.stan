functions {
  vector daily_rate(real alpha, real beta, real maximum, vector t, int N_t) {
    vector[N_t] rate;
    vector[N_t] linear = alpha + beta * t;
    for (i in 1:N_t) rate[i] = beta * maximum * exp(-linear[i]) / square(exp(-linear[i]) + 1);
    return rate;
  }
}

data {
  int<lower = 0> N_days;
  int<lower = 0> N_agegroups;
  int<lower = 0> pop;
  int<lower = 0> age_cases[N_agegroups];
  vector[N_days] days;
  vector[2] alpha_prior;
  vector[2] beta_prior;
  vector[2] maximum_prior;
  vector[2] phi_prior;
  
}

parameters {
  simplex[N_agegroups] theta;
  real alpha;
  real<lower = 0> beta;
  real<lower = 0, upper = 1> maximum;
  real<lower = 0> phi;
}

transformed parameters {
  vector[N_days] pred_rate = daily_rate(alpha, beta, maximum, days, N_days);
}

model {
  
  alpha ~ normal(alpha_prior[1], alpha_prior[2]);
  beta ~ normal(beta_prior[1], beta_prior[2]);
  maximum ~ normal(maximum_prior[1], maximum_prior[2]);
  phi ~ normal(phi_prior[1], phi_prior[2]);
  
  age_cases ~ multinomial(theta);
}


generated quantities {
  int new_cases[N_days];
  
  new_cases = neg_binomial_2_rng(pred_rate * pop, phi);
  
}


