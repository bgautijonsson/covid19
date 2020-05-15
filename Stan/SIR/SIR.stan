functions {
  real[] sir( real t,           
                  real[] y,          
                  real[] theta,             
                  real[] x_r, int[] x_i) {  
  real S = y[1];
  real I = y[2];
  real R = y[3];
  real N = x_i[1];
  real beta;
  real gamma = theta[3];
  real dS_dt;
  real dR_dt;
  real dI_dt;
  if (t > 11) beta = theta[1]; 
  else beta = theta[2];
  
  
  dS_dt = - beta * I * S/N;
  dR_dt = gamma * I;
  dI_dt = -(dS_dt + dR_dt);
  return {dS_dt, dI_dt, dR_dt};
  }
}

data {
  int<lower = 1> N_days;             
  int<lower=0> counts[N_days,3];    
  real<lower = 0> y0[3];
  int<lower = 0> N;
}

transformed data{
  real times_measured[N_days];   
  real x_r[0];
  int x_i[1] = {N};
  for (i in 1:N_days) times_measured[i] = i;
}

parameters {
  real<lower = 0> beta[2];
  real<lower = 0> gamma;
}

transformed parameters {
  real<lower=0> theta[3] = {beta[1], beta[2], gamma};
  real y[N_days, 3];
  y = integrate_ode_rk45(sir, y0, 0, times_measured, theta, x_r, x_i);
}

model {
  // priors
  beta ~ normal(0.5, 0.2);
  
  gamma ~ normal(0.1, 0.2);
  for (i in 2:3) counts[, i] ~ poisson(col(to_matrix(y), i));
}

generated quantities {
  real<lower = 0> recovery_time = 1 / gamma;
  real<lower = 0> R0[2];
  for (i in 1:2) R0[i] = beta[i] * recovery_time;
}

