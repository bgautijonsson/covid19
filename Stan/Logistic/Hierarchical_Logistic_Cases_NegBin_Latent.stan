data {
  int<lower = 0> N_obs;
  // country[N_obs] takes on value i for country number i etc.
  int country[N_obs];
  // days is how many days since case rate per 1000 exceeded a limit we have chosen
  vector[N_obs] days;
  // We model the daily number of newly diagnosed cases instead of cumulative numbers
  int new_cases[N_obs];
  // Model the daily number of deaths
  int new_deaths[N_obs];
  
  int<lower = 0> N_countries;
  vector[N_countries] pop;
}

transformed data {
  vector[N_obs] log_pop = log(pop[country]);
}

parameters {
  // Since we use a non-centered parametrisation we first create normal(0, 1) variables for alpha and beta
  
  // Beta parameters
  // Cases
  vector[N_countries] z_beta_cases;
  real mu_beta_cases;
  real<lower = 0> sigma_beta_cases;
  // Deaths
  vector[N_countries] z_beta_deaths;
  real mu_beta_deaths;
  real<lower = 0> sigma_beta_deaths;
  
  // Alpha parameters
  // Cases
  vector[N_countries] z_alpha_cases;
  real mu_alpha_cases;
  real<lower = 0> sigma_alpha_cases;
  // Deaths
  vector[N_countries] z_alpha_deaths;
  real mu_alpha_deaths;
  real<lower = 0> sigma_alpha_deaths;
  
  //  Asymptote/saturation parameters
  // We model the beta distribution in terms of mean and sample size instead of a and b.
  // Cases
  vector<lower = 0, upper = 1>[N_countries] S_cases;
  real<lower = 0, upper = 1> mu_s_cases;
  real<lower = 0> kappa_s_cases;
  // Deaths
  vector<lower = 0, upper = 1>[N_countries] S_deaths;
  real<lower = 0, upper = 1> mu_s_deaths;
  real<lower = 0> kappa_s_deaths;
  
  //  Overdispersion parameters. One for each country which is dependent on hyperparameter
  // Cases
  vector<lower = 0>[N_countries] z_phi_inv_sqrt_cases;
  real<lower = 0> sigma_phi_inv_sqrt_cases;
  // Deaths
  vector<lower = 0>[N_countries] z_phi_inv_sqrt_deaths;
  real<lower = 0> sigma_phi_inv_sqrt_deaths;
}

transformed parameters {
  // Non-Centerd parametrizations
  // If B ~ normal(mu_b, sigma_b) then B = mu_b + sigma_b * normal(0, 1)
  vector[N_countries] log_beta_cases = mu_beta_cases + sigma_beta_cases * z_beta_cases;
  vector[N_countries] log_beta_deaths = mu_beta_deaths + sigma_beta_deaths * z_beta_deaths;
  vector<lower = 0>[N_countries] beta_cases = exp(log_beta_cases);
  vector<lower = 0>[N_countries] beta_deaths = exp(log_beta_deaths);
  vector[N_countries] alpha_cases = mu_alpha_cases + sigma_alpha_cases * z_alpha_cases;
  vector[N_countries] alpha_deaths = mu_alpha_deaths + sigma_alpha_deaths * z_alpha_deaths;
  // If X ~ exponential(lambda) then X ~ lambda * exponential(1)
  vector<lower = 0>[N_countries] phi_inv_sqrt_cases = sigma_phi_inv_sqrt_cases * z_phi_inv_sqrt_cases;
  vector<lower = 0>[N_countries] phi_inv_sqrt_deaths = sigma_phi_inv_sqrt_deaths * z_phi_inv_sqrt_deaths;
  // Overdispersion parameters
  vector<lower = 0>[N_countries] phi_inv_cases = square(phi_inv_sqrt_cases);
  vector<lower = 0>[N_countries] phi_inv_deaths = square(phi_inv_sqrt_deaths);
  vector<lower = 0>[N_countries] phi_cases = inv(phi_inv_cases);
  vector<lower = 0>[N_countries] phi_deaths = inv(phi_inv_deaths);
  // Asymptote hyperparameters
  real<lower = 0> a_s_cases = mu_s_cases * kappa_s_cases;
  real<lower = 0> b_s_cases = (1 - mu_s_cases) * kappa_s_cases;
  real<lower = 0> a_s_deaths = mu_s_deaths * kappa_s_deaths;
  real<lower = 0> b_s_deaths = (1 - mu_s_deaths) * kappa_s_deaths;
  vector[N_obs] linear_cases = alpha_cases[country] + beta_cases[country] .* days;
  vector[N_obs] linear_deaths = alpha_deaths[country] + beta_deaths[country] .* days;
  vector<lower = 0>[N_obs] f_cases;
  vector<lower = 0>[N_obs] f_deaths;
  vector<lower = 0>[N_obs] dfdt_cases;
  vector<lower = 0>[N_obs] dfdt_deaths;
  for (i in 1:N_obs) {
    f_cases[i] = S_cases[country[i]] / (1 + exp(-linear_cases[i]));
    f_deaths[i] = S_deaths[country[i]] / (1 + exp(-linear_deaths[i]));
    dfdt_cases[i] = beta_cases[country[i]] * f_cases[i] * (1 - f_cases[i] / S_cases[country[i]]);
    dfdt_deaths[i] = beta_deaths[country[i]] * f_deaths[i] * (1 - f_deaths[i] / S_deaths[country[i]]);
    
  }
}

model {
  // Alpha parameters
  // Cases
  mu_alpha_cases ~ normal(0, 3);
  sigma_alpha_cases ~ exponential(0.2);
  z_alpha_cases ~ std_normal();
  // Deaths
  mu_alpha_deaths ~ normal(0, 3);
  sigma_alpha_deaths ~ exponential(0.2);
  z_alpha_deaths ~ std_normal();
  
  // Beta parameters
  // Cases
  mu_beta_cases ~ normal(-2, 1);
  sigma_beta_cases ~ exponential(0.5);
  z_beta_cases ~ std_normal();
  // Deaths
  mu_beta_deaths ~ normal(-2, 1);
  sigma_beta_deaths ~ exponential(0.5);
  z_beta_deaths ~ std_normal();
  
  // Asymptote parameters
  // Cases
  mu_s_cases ~ beta(1, 99);
  kappa_s_cases ~ exponential(0.001);
  S_cases ~ beta(a_s_cases, b_s_cases);
  // Deaths
  mu_s_deaths ~ beta(1, 99);
  kappa_s_deaths ~ exponential(0.001);
  S_deaths ~ beta(a_s_cases, b_s_cases);
  
  // Overdispersion parameters
  // Cases
  z_phi_inv_sqrt_cases ~ exponential(1);
  sigma_phi_inv_sqrt_cases ~ exponential(0.5);
  // Deaths
  z_phi_inv_sqrt_deaths ~ exponential(1);
  sigma_phi_inv_sqrt_deaths ~ exponential(0.5);
  
  //  Likelihood
  new_cases ~ neg_binomial_2(dfdt_cases .* pop[country], phi_cases[country]);
  new_deaths ~ neg_binomial_2(dfdt_deaths .* pop[country], phi_deaths[country]);
}

