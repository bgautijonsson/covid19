
data {
    int<lower = 0> N_days;
    int<lower = 0> N_countries;
    int<lower = 0> N_preds;
    int total_cases[N_days, N_countries];
}


parameters {
    matrix[N_days, N_countries] log_beta;
    
    
    real mu_log_beta;
    real<lower = 0> sigma_log_beta;
    
    row_vector[N_countries] trend_beta;
    real mu_trend_beta;
    real<lower = 0> sigma_trend_beta;
    
    real<lower = 0> sigma_state;
}

transformed parameters {
    matrix[N_days, N_countries] beta = exp(log_beta);
    
}

model {
    
    sigma_log_beta ~ exponential(1);
    sigma_trend_beta ~ exponential(1);
    sigma_state ~ exponential(1);
    
    mu_log_beta ~ normal(0, 1);
    mu_trend_beta ~ normal(0, 1);
    
    log_beta[1, ] ~ normal(mu_log_beta, sigma_log_beta);
    trend_beta ~ normal(mu_trend_beta, sigma_trend_beta);
    
    
    for (t in 2:N_days) {
        log_beta[t, ] ~ normal(log_beta[t - 1, ] + trend_beta, sigma_state);
        total_cases[t, ] ~ poisson_log(to_vector(log(total_cases[t - 1, ])) + to_vector(beta[t, ]));
    }
    
}

// generated quantities {
//     int pred_cases[N_preds, N_countries];
//     matrix[N_preds, N_countries] pred_log_beta;
//     
//     
//     pred_log_beta[1, ] = log_beta[N_days, ] + trend_beta;
//     pred_cases[1, ] = poisson_log_rng(to_vector(log(total_cases[N_days, ])) + to_vector(exp(pred_log_beta[1, ])));
//     
//     for (t in 2:N_preds) {
//         pred_log_beta[t, ] = pred_log_beta[t - 1, ] + trend_beta;
//         pred_cases[t, ] = poisson_log_rng(to_vector(log(total_cases[t - 1, ])) + to_vector(exp(pred_log_beta[t, ])));
//     }
// }

