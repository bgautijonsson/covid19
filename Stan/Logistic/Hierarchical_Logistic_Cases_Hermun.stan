data {
  int<lower = 0> N_obs;
  int country[N_obs];
  vector[N_obs] days;
  int obs_cases[N_obs];
  
  int<lower = 0> N_countries;
  vector[N_countries] pop;
  
  int<lower = 0> iceland_id;
  int<lower = 0> N_preds;
  int<lower = 0> N_agegroups;
  vector[N_preds] pred_days;
  simplex[9] age_dist;
  row_vector<lower = 0, upper = 1>[9] hospital_dist;
  row_vector<lower = 0, upper = 1>[9] icu_dist;
}

parameters {
  vector[N_countries] beta;
  vector[N_countries] alpha;
  vector<lower = 0, upper = 1>[N_countries] maximum;
  
  real mu_beta;
  real<lower = 0> sigma_sq_beta;
  
  real mu_alpha;
  real<lower = 0> sigma_sq_alpha;
  
  real<lower = 0> beta_a;
  real<lower = 0> beta_b;
}

transformed parameters {
  vector[N_obs] linear = alpha[country] + beta[country] .* days;
  vector<lower = 0, upper = 1>[N_obs] rate;
  for (i in 1:N_obs) rate[i] = maximum[country[i]] .* 1 / (1 + exp(-linear[i]));
}

model {
  
  sigma_sq_beta ~ inv_chi_square(2);
  sigma_sq_alpha ~ inv_chi_square(2);
  
  maximum ~ beta(beta_a, beta_b);
  
  beta ~ normal(mu_beta, sigma_sq_beta);
  alpha ~ normal(mu_alpha, sigma_sq_alpha);
  
  
  obs_cases ~ poisson(rate .* pop[country]);
}

generated quantities {
  
  vector[N_preds] cumul_linear;
  vector<lower = 0, upper = 1>[N_preds] cumul_rate;
  
  vector<lower = 0>[N_preds] cumul_expected;
  matrix<lower = 0>[N_preds, 9] cumul_agegroup_expected;
  matrix<lower = 0>[N_preds, 9] cumul_hospital_expected;
  matrix<lower = 0>[N_preds, 9] cumul_icu_expected;
  
  int<lower = 0> cumul_cases[N_preds];
  int<lower = 0> cumul_agegroups[N_preds, 9];
  int<lower = 0> cumul_hospital[N_preds, 9];
  int<lower = 0> cumul_icu[N_preds, 9];
  
  vector[N_preds] active_linear;
  vector<lower = 0, upper = 1>[N_preds] active_rate;
  
  vector<lower = 0>[N_preds] active_expected;
  matrix<lower = 0>[N_preds, 9] active_agegroup_expected;
  matrix<lower = 0>[N_preds, 9] active_hospital_expected;
  matrix<lower = 0>[N_preds, 9] active_icu_expected;
  
  int<lower = 0> active_cases[N_preds];
  int<lower = 0> active_agegroups[N_preds, 9];
  int<lower = 0> active_hospital[N_preds, 9];
  int<lower = 0> active_icu[N_preds, 9];
  
  cumul_linear = alpha[iceland_id] + beta[iceland_id] * pred_days;
  cumul_rate = maximum[iceland_id] ./ (1 + exp(-cumul_linear));
  cumul_expected = cumul_rate * pop[iceland_id];
  
  
  
  active_linear = alpha[iceland_id] + beta[iceland_id] * (pred_days - 21);
  active_rate = maximum[iceland_id] ./ (1 + exp(-active_linear));
  active_expected = active_rate * pop[iceland_id];
  
  
  
  cumul_cases = poisson_rng(cumul_expected);
  active_cases = poisson_rng(active_expected);
  
  for (i in 1:N_preds) {
    active_agegroup_expected[i, ] = to_row_vector(active_expected[i] * age_dist);
    
    cumul_agegroup_expected[i, ] = to_row_vector(cumul_expected[i] * age_dist);
    cumul_hospital_expected[i, ] = cumul_agegroup_expected[i, ] .* hospital_dist;
    cumul_icu_expected[i, ] = cumul_hospital_expected[i, ] .* icu_dist;
    
    if (i <= 10) {
      active_hospital_expected[i, ] = active_agegroup_expected[i, ] .* hospital_dist;
      active_icu_expected[i, ] = active_hospital_expected[i, ] .* icu_dist;
    } else if (i <= 14) {
      active_hospital_expected[i, ] = active_agegroup_expected[i, ] .* hospital_dist;
      active_icu_expected[i, ] = (active_hospital_expected[i, ] - active_hospital_expected[i - 10, ]) .* icu_dist;
    } else {
      active_hospital_expected[i, ] = (active_agegroup_expected[i, ] - active_agegroup_expected[i - 14, ]) .* hospital_dist;
      active_icu_expected[i, ] = (active_hospital_expected[i, ] - active_hospital_expected[i - 10, ]) .* icu_dist;
    }
    
    cumul_agegroups[i, ] = poisson_rng(cumul_agegroup_expected[i, ]);
    cumul_hospital[i, ] = poisson_rng(cumul_hospital_expected[i, ]);
    cumul_icu[i, ] = poisson_rng(cumul_icu_expected[i, ]);
    
    active_agegroups[i, ] = poisson_rng(active_agegroup_expected[i, ]);
    active_hospital[i, ] = poisson_rng(active_hospital_expected[i, ]);
    active_icu[i, ] = poisson_rng(active_icu_expected[i, ]);
  }
}


