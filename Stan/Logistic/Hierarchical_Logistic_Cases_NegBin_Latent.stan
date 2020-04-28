data {
  int<lower = 0> N_obs;
  // country[N_obs] takes on value i for country number i etc.
  int country[N_obs];
  // days is how many days since case rate per 1000 exceeded a limit we have chosen
  vector[N_obs] days;
  // We model the daily number of newly diagnosed cases instead of cumulative numbers
  int new_cases[N_obs];
  
  int<lower = 0> N_countries;
  vector[N_countries] pop;
  int total_deaths[N_obs];
}

parameters {
  // Since we use a non-centered parametrisation we first create normal(0, 1) variables for alpha and beta
  
  // Beta parameters
  vector[N_countries] z_beta;
  real mu_beta;
  real<lower = 0> sigma_beta;
  
  // Alpha parameters
  vector[N_countries] z_alpha;
  real mu_alpha;
  real<lower = 0> sigma_alpha;
  
  //  Asymptote/saturation parameters
  // We model the beta distribution in terms of mean and sample size instead of a and b.
  vector<lower = 0, upper = 1>[N_countries] S;
  real<lower = 0, upper = 1> mu_s;
  real<lower = 0> kappa_s;

  
  real<lower = 0, upper = 1> mu_death_rate;
  real<lower = 0> kappa_death_rate;
  vector<lower = 0, upper = 1>[N_countries] death_rate;
  
  real<lower = 0, upper = 1> mu_detected;
  real<lower = 0> kappa_detected;
  vector<lower = 0, upper = 1>[N_countries] country_mu_detected;
  vector<lower = 0>[N_countries] country_kappa_detected;
  vector<lower = 0, upper = 1>[N_obs] perc_detected;
  
}

transformed parameters {
  // real<lower = 0> a_detected = mu_detected * kappa_detected;
  // real<lower = 0> b_detected = (1 - mu_detected) * kappa_detected;
  // Non-Centerd parametrizations
  // If B ~ normal(mu_b, sigma_b) then B = mu_b + sigma_b * normal(0, 1)
  vector[N_countries] log_beta = mu_beta + sigma_beta * z_beta;
  vector<lower = 0>[N_countries] beta = exp(log_beta);
  vector[N_countries] alpha = mu_alpha + sigma_alpha * z_alpha;
  // Asymptote hyperparameters
  real<lower = 0> a_s = mu_s * kappa_s;
  real<lower = 0> b_s = (1 - mu_s) * kappa_s;
  // Death Rate hyperparameters
  real<lower = 0> a_death_rate = mu_death_rate * kappa_death_rate;
  real<lower = 0> b_death_rate = (1 - mu_death_rate) * kappa_death_rate;
  // Death Rate hyperparameters
  real<lower = 0> country_a_detected = mu_detected * kappa_detected;
  real<lower = 0> country_b_detected = (1 - mu_detected) * kappa_detected;
  vector<lower = 0>[N_obs] a_detected = country_mu_detected[country] .* country_kappa_detected[country];
  vector<lower = 0>[N_obs] b_detected = (1 - country_mu_detected[country]) .* country_kappa_detected[country];
  // Logistic equation calculations
  vector[N_obs] linear = alpha[country] + beta[country] .* days;
  vector<lower = 0>[N_obs] f;
  vector<lower = 0>[N_obs] dfdt;
  for (i in 1:N_obs) {
    
    f[i] = S[country[i]] / (1 + exp(-linear[i]));
    dfdt[i] = beta[country[i]] * f[i] * (1 - f[i] / S[country[i]]);
    
  }
}

model {
  // Alpha parameters
  mu_alpha ~ normal(-2.5, 3);
  sigma_alpha ~ exponential(1);
  z_alpha ~ std_normal();
  
  // Beta parameters
  mu_beta ~ normal(-3, 1);
  sigma_beta ~ exponential(1);
  z_beta ~ std_normal();
  
  // Asymptote parameters
  mu_s ~ beta(1, 99);
  kappa_s ~ exponential(0.001);
  S ~ beta(a_s, b_s);
  
  // Death rate parameters
  mu_death_rate ~ beta(6, 1000);
  kappa_death_rate ~ exponential(0.001);
  death_rate ~ beta(a_death_rate, b_death_rate);
  
  // Diagnostic rate parameters
  mu_detected ~ beta(2, 2);
  kappa_detected ~ exponential(1);
  country_kappa_detected ~ exponential(0.1);
  country_mu_detected ~ beta(country_a_detected, country_b_detected);
  perc_detected ~ beta(a_detected, b_detected);
  
  //  Likelihood
  total_deaths ~ poisson(death_rate[country] .* f .* pop[country]);
  new_cases ~ poisson(dfdt .* perc_detected .* pop[country]);
}

