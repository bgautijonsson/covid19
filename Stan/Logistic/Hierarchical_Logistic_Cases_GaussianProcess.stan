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
  
  real X_GP[N_obs];
}

transformed data {
  vector[N_countries] log_pop = log(pop);
  real<lower = 0> delta = 1e-9;
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
  
  // GP parameters
  real<lower = 0> rho_gp;
  real<lower = 0> eta_gp;
  real<lower = 0> sigma_gp;
  vector[N_obs] log_error_gp;
  vector[N_obs] z_gp;
  
}

transformed parameters {
  // Non-Centered parametrizations
  // If B ~ normal(mu_b, sigma_b) then B = mu_b + sigma_b * normal(0, 1)
  vector[N_countries] log_beta = mu_beta + sigma_beta * z_beta;
  vector<lower = 0>[N_countries] beta = exp(log_beta);
  vector[N_countries] alpha = mu_alpha + sigma_alpha * z_alpha;
  // Asymptote hyperparameters
  real<lower = 0> a_s = mu_s * kappa_s;
  real<lower = 0> b_s = (1 - mu_s) * kappa_s;
  vector[N_obs] linear = alpha[country] + beta[country] .* days;
  vector<lower = 0>[N_obs] f;
  vector<lower = 0>[N_obs] dfdt;
  vector[N_obs] log_dfdt;
  for (i in 1:N_obs) {
    f[i] = S[country[i]] / (1 + exp(-linear[i]));
    dfdt[i] = beta[country[i]] * f[i] * (1 - f[i] / S[country[i]]);
  }
  
  log_dfdt = log(dfdt);
}

model {
  vector[N_obs] f_gp;
  {
    matrix[N_obs, N_obs] L_K;
    matrix[N_obs, N_obs] K = cov_exp_quad(X_GP, eta_gp, rho_gp);

    // diagonal elements
    for (n in 1:N_obs)
      K[n, n] = K[n, n] + delta;

    L_K = cholesky_decompose(K);
    f_gp = L_K * z_gp;
  }
  
  z_gp ~ std_normal();
  rho_gp ~ inv_gamma(5, 5);
  eta_gp ~ std_normal();
  sigma_gp ~ exponential(1);
  
  log_error_gp ~ normal(f_gp, sigma_gp);
  
  
  // Alpha parameters
  mu_alpha ~ normal(-1.5, 3);
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
  
  //  Likelihood
  new_cases ~ poisson_log(log_dfdt + log_pop[country]  + log_error_gp);
}

