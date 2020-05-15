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

transformed data {
  vector[N_obs] log_pop = log(pop[country]);
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
  
  //  Overdispersion parameters. One for each country which is dependent on hyperparameter
  vector<lower = 0>[N_countries] z_phi_inv_sqrt;
  real<lower = 0> sigma_phi_inv_sqrt;
  
  real mu_nu;
  real<lower = 0> sigma_nu;
  vector[N_countries] z_nu;
  
}

transformed parameters {
  // Non-Centerd parametrizations
  // If B ~ normal(mu_b, sigma_b) then B = mu_b + sigma_b * normal(0, 1)
  vector[N_countries] log_beta = mu_beta + sigma_beta * z_beta;
  vector<lower = 0>[N_countries] beta = exp(log_beta);
  vector[N_countries] alpha = mu_alpha + sigma_alpha * z_alpha;
  // If X ~ exponential(lambda) then X ~ lambda * exponential(1)
  vector<lower = 0>[N_countries] phi_inv_sqrt = sigma_phi_inv_sqrt * z_phi_inv_sqrt;
  // Overdispersion parameters
  vector<lower = 0>[N_countries] phi_inv = square(phi_inv_sqrt);
  vector<lower = 0>[N_countries] phi = inv(phi_inv);
  // Asymptote hyperparameters
  real<lower = 0> a_s = mu_s * kappa_s;
  real<lower = 0> b_s = (1 - mu_s) * kappa_s;
  vector[N_countries] log_S = log(S);
  // Generalized Logistic Parameters
  vector[N_countries] log_nu = mu_nu + sigma_nu * z_nu;
  vector<lower = 0>[N_countries] nu = exp(log_nu);
}

model {
  // Logistic equation calculations
  vector[N_obs] linear = nu[country] .* (alpha[country] +  beta[country] .* days);
  vector[N_obs] extra = (inv(nu[country]) + 1) .* log(1 + exp(-linear));
  vector[N_obs] log_dfdt = log_beta[country] + log_S[country] - linear - extra;
  // Alpha parameters
  mu_alpha ~ normal(0, 3);
  sigma_alpha ~ exponential(0.5);
  z_alpha ~ std_normal();
  
  // Beta parameters
  mu_beta ~ normal(-2, 1);
  sigma_beta ~ exponential(1);
  z_beta ~ std_normal();
  
  // Asymptote parameters
  mu_s ~ beta(1, 99);
  kappa_s ~ exponential(0.01);
  S ~ beta(a_s, b_s);
  
  // Overdispersion parameters
  z_phi_inv_sqrt ~ std_normal();
  sigma_phi_inv_sqrt ~ std_normal();
  
  // Generalized Logistic parameters
  mu_nu ~ normal(0, 1);
  sigma_nu ~ exponential(1);
  z_nu ~ std_normal();
  
  //  Likelihood
  new_cases ~ neg_binomial_2_log(log_dfdt + log_pop, phi[country]);
}

generated quantities {
  vector[N_obs] linear = nu[country] .* (alpha[country] +  beta[country] .* days);
  vector[N_obs] extra = (inv(nu[country]) + 1) .* log(1 + exp(-linear));
  vector[N_obs] log_dfdt = log_beta[country] + log_S[country] - linear - extra;
  real log_lik[N_obs];
  // int sim_cases[N_obs]= neg_binomial_2_rng(dfdt .* pop[country], phi[country]);
  for (i in 1:N_obs) log_lik[i] = neg_binomial_2_log_lpmf(new_cases[i] | log_dfdt[i] + log_pop[i], phi[country[i]]);
}
