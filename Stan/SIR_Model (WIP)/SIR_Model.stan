functions {
  real dSIR_dt(real t,
    real[] count_init,
    real beta,
    real gamma
    ) {
      real S0 = count_init[1];
      real I0 = count_init[2];
      real R0 = count_init[3];
      
      
      return(1);
    }
    
}

data {
  int<lower = 0> N_days;
  real<lower = 0> obs_counts[N_days, 3];
}


parameters {
  real<lower = 0> counts_init[3];
  
} 

transformed parameters {
  real true_counts[N_days, 3];
  true_counts[1, 1] = counts_init[1];
  true_counts[1, 2] = counts_init[2];
  true_counts[1, 3] = counts_init[3];
  
}

model {
  
}



