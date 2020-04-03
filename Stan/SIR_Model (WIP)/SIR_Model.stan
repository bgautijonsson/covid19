functions {
  real[] dSIR_dt(real t,
    real[] count_init,
    real[] pars,
    real[] x_r, int[] x_i
    ) {
      real S = count_init[1];
      real I = count_init[2];
      real R = count_init[3];
      real N = 340000;
      
      real beta = pars[1];
      real gamma = pars[2];
      
      real dS_dt = -beta * I * S / N;
      real dI_dt = beta * I * S / N - gamma * I;
      real dR_dt = gamma * I;
      
      return {dS_dt, dI_dt, dR_dt};
    }
    
}

data {
  int<lower = 0> N_days;
  real<lower = 0> obs_counts[N_days, 3];
}

transformed data {
  real times_measured[N_days - 1];
  for (i in 2:N_days) times_measured[i - 1] = i;
}


parameters {
  real<lower = 0> counts_init[3];
  real<lower = 0> pars[2];
  real<lower = 0> sigma[3];
} 

transformed parameters {
  real true_counts[N_days, 3];
  true_counts[1, 1] = counts_init[1];
  true_counts[1, 2] = counts_init[2];
  true_counts[1, 3] = counts_init[3];
  
  true_counts[2:N_days, 1:3] = integrate_ode_rk45(dSIR_dt, counts_init, 0, times_measured, pars, rep_array(0.0, 0), rep_array(0, 0));
}

model {
  
  pars ~ exponential(2);
  sigma ~ exponential(1);
  counts_init ~ lognormal(log(10), 1);
  
  for (t in 1:N_days) {
    for (i in 1:3) {
      obs_counts[t, i] ~ lognormal(log(true_counts[t, i]), sigma[i]);
    }
  }
  
}



