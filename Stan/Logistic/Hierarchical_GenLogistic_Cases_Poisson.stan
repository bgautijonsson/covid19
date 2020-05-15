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
  
  // Alpha parameters
  vector[N_countries] z_alpha;
  real mu_alpha;
  real<lower = 0> sigma_alpha;
  
  //  Asymptote/saturation parameters
  // We model the beta distribution in terms of mean and sample size instead of a and b.
  vector<lower = 0, upper = 1>[N_countries] S;
  real<lower = 0, upper = 1> mu_s;
  real<lower = 0> kappa_s;
  
  // Asymmetric parameters
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
  // Asymptote hyperparameters
  real<lower = 0> a_s = mu_s * kappa_s;
  real<lower = 0> b_s = (1 - mu_s) * kappa_s;
  // Generalized Logistic Parameters
  vector<lower = 0>[N_countries] nu = exp(mu_nu + sigma_nu * z_nu);
  // Logistic equation calculations
  vector[N_obs] linear = alpha[country] + nu[country] .* beta[country] .* days;
  // vector[N_obs] linear = alpha[country] + beta[country] .* days;
  vector<lower = 0>[N_obs] f;
  vector<lower = 0>[N_obs] dfdt;
  for (i in 1:N_obs) {
    f[i] = S[country[i]] / pow(1 + exp(-linear[i]), inv(nu[country[i]]));
    dfdt[i] = beta[country[i]] * f[i] * (1 - pow(f[i] / S[country[i]], nu[country[i]]));
    
  }
}

model {
  // Alpha parameters
  mu_alpha ~ normal(0, 1);
  sigma_alpha ~ exponential(0.2);
  z_alpha ~ std_normal();
  
  // Beta parameters
  mu_beta ~ normal(-2, 1);
  sigma_beta ~ exponential(0.5);
  z_beta ~ std_normal();
  
  // Asymptote parameters
  mu_s ~ beta(1, 99);
  kappa_s ~ exponential(0.001);
  S ~ beta(a_s, b_s);
  
  // Generalized Logistic parameters
  mu_nu ~ normal(0, 2);
  sigma_nu ~ exponential(0.5);
  z_nu ~ std_normal();
  
  //  Likelihood
  new_cases ~ poisson(dfdt .* pop[country]);
}

generated quantities {
  real log_lik[N_obs];
  int sim_cases[N_obs]= poisson_rng(dfdt .* pop[country]);
  for (i in 1:N_obs) log_lik[i] = poisson_log_lpmf(new_cases[i] | dfdt[i] .* pop[country[i]]);
}
