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
}

parameters {
  // Since we use a non-centered parametrisation we first create normal(0, 1) variables for alpha and beta
  
  // Beta parameters
  vector[N_countries] z_beta;
  real mu_beta;
  real<lower = 0> sigma_beta;
  
  vector[N_countries] z_beta2;
  real mu_beta2;
  real<lower = 0> sigma_beta2;
  
  // Alpha parameters
  vector[N_countries] z_alpha;
  real mu_alpha;
  real<lower = 0> sigma_alpha;
  
  vector[N_countries] z_alpha2;
  real mu_alpha2;
  real<lower = 0> sigma_alpha2;
  
  //  Asymptote/saturation parameters
  // We model the beta distribution in terms of mean and sample size instead of a and b.
  vector<lower = 0, upper = 1>[N_countries] S;
  real<lower = 0, upper = 1> mu_s;
  real<lower = 0> kappa_s;
  
  vector<lower = 0, upper = 1>[N_countries] S2;
  real<lower = 0, upper = 1> mu_s2;
  real<lower = 0> kappa_s2;
  
  //  Overdispersion parameters. One for each country which is dependent on hyperparameter
  vector<lower = 0>[N_countries] z_phi_inv_sqrt;
  real<lower = 0> sigma_phi_inv_sqrt;
  
}

transformed parameters {
   // Non-Centerd parametrizations
   // If B ~ normal(mu_b, sigma_b) then B = mu_b + sigma_b * normal(0, 1)
  vector<lower = 0>[N_countries] beta = exp(mu_beta + sigma_beta * z_beta);
  vector[N_countries] alpha = mu_alpha + sigma_alpha * z_alpha;
  
  vector<lower = 0>[N_countries] beta2 = exp(mu_beta2 + sigma_beta2 * z_beta2);
  vector[N_countries] alpha2 = mu_alpha2 + sigma_alpha2 * z_alpha2;
   // If X ~ exponential(lambda) then X ~ lambda * exponential(1)
  vector<lower = 0>[N_countries] phi_inv_sqrt = sigma_phi_inv_sqrt * z_phi_inv_sqrt;
   // Overdispersion parameters
  vector<lower = 0>[N_countries] phi_inv = square(phi_inv_sqrt);
  vector<lower = 0>[N_countries] phi = inv(phi_inv);
   // Asymptote hyperparameters
  real<lower = 0> a_s = mu_s * kappa_s;
  real<lower = 0> b_s = (1 - mu_s) * kappa_s;
  
  real<lower = 0> a_s2 = mu_s2 * kappa_s2;
  real<lower = 0> b_s2 = (1 - mu_s2) * kappa_s2;
   // Logistic equation calculations
  vector[N_obs] linear = alpha[country] + beta[country] .* days;
  vector[N_obs] linear2 = alpha2[country] + beta2[country] .* days;
  vector<lower = 0>[N_obs] difference;
  vector<lower = 0>[N_obs] difference2;
  vector<lower = 0>[N_obs] total_difference;
  for (i in 1:N_obs) {
    difference[i] = beta[country[i]] * S[country[i]] * exp(-linear[i]) / square(exp(-linear[i]) + 1);
    difference2[i] = beta2[country[i]] * S2[country[i]] * exp(-linear2[i]) / square(exp(-linear2[i]) + 1);
    
    total_difference[i] = difference[i] + difference2[i];
  }
}

model {
   // Alpha parameters
  mu_alpha ~ normal(0, 3);
  sigma_alpha ~ exponential(1);
  z_alpha ~ std_normal();
  
  mu_alpha2 ~ normal(0, 3);
  sigma_alpha2 ~ exponential(1);
  z_alpha2 ~ std_normal();
  
   // Beta parameters
  mu_beta ~ normal(0, 3);
  sigma_beta ~ exponential(1);
  z_beta ~ std_normal();
  
  
  mu_beta2 ~ normal(0, 3);
  sigma_beta2 ~ exponential(1);
  z_beta2 ~ std_normal();
  
   // Asymptote parameters
  mu_s ~ beta(1, 99);
  kappa_s ~ exponential(0.001);
  S ~ beta(a_s, b_s);
  
  
  mu_s2 ~ beta(1, 9);
  kappa_s2 ~ exponential(0.001);
  S2 ~ beta(a_s, b_s);
  
   // Overdispersion parameters
  z_phi_inv_sqrt ~ exponential(1);
  sigma_phi_inv_sqrt ~ exponential(1);
  
  //  Likelihood
  new_cases ~ neg_binomial_2(total_difference .* pop[country], phi[country]);
}

