data {
  int<lower = 0> N_obs;
  vector[N_obs] days;
  int obs_cases[N_obs];
  real<lower = 0> pop;
  real<lower = 0> a;
  real<lower = 0> b;
}

parameters {
  real beta;
  real alpha;
  real<lower = 0, upper = 1> maximum;
}

transformed parameters {
  vector[N_obs] linear = alpha + beta * days;
  vector<lower = 0, upper = 1>[N_obs] rate;
  for (i in 1:N_obs) rate[i] = maximum .* 1 / (1 + exp(-linear[i]));
}

model {
  
  maximum ~ beta(a, b);
  
  
  obs_cases ~ poisson(rate * pop);
}


