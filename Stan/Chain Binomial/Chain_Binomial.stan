
data {
  int<lower = 0> N_obs;
  int<lower = 0> N_countries;
  vector<lower = 0>[N_obs] I_N;
  int<lower = 0> country[N_obs];
  int<lower = 0> S[N_obs];
  int<lower = 0> new_cases[N_obs];
}



parameters {
  real<lower = 0, upper = 1> beta[N_countries];
  real<lower = 0> a;
  real<lower = 0> b;
}


model {
  
  beta ~ beta(a, b);
  
  for (i in 1:N_obs) {
    new_cases[i] ~ binomial(S[i], 1 - exp(- beta[country[i]] * I_N[i]));
  }
  
}

