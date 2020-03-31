
data {
    int<lower = 0> N_days;
    int<lower = 0> N_countries;
    int total_cases[N_days, N_countries];
    vector[N_countries] pop;
}


parameters {
    vector[N_days] kappa_pre;
    real kappa0;
    real<lower = 0> sigma_sq_state;
    
    real theta;
    real<lower = 0> sigma_sq_theta;
    
    
    
    vector<lower = 0>[N_countries] alpha_pre;
    
    real<lower = 0> sigma_sq_beta;
    vector[N_countries] beta_pre;
}

transformed parameters {
    vector[N_days] kappa = kappa_pre - mean(kappa_pre);
    vector[N_countries] alpha = log(alpha_pre);
    vector[N_countries] beta = beta_pre - mean(beta_pre);
    vector[N_countries] change =  inv(N_countries) * (1 + beta);
    
    
    
}

model {
    sigma_sq_beta ~ inv_chi_square(1);
    beta ~ normal(0, sigma_sq_beta);
    
    sigma_sq_theta ~ inv_chi_square(1);
    theta ~ normal(0, sigma_sq_theta);
    
    sigma_sq_state ~ inv_chi_square(1);
    kappa[1] ~ normal(kappa0, sigma_sq_state);
    
    total_cases[1, ] ~ poisson_log(alpha + change * kappa[1] + log(to_vector(pop)));
    
    for (t in 2:N_days) {
        kappa[t] ~ normal(kappa[t - 1] + theta, sigma_sq_state);
        total_cases[t, ] ~ poisson_log(alpha + change * kappa[t] + log(to_vector(pop)));
    }
    
    
}
