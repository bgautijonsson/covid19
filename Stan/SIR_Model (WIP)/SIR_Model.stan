data {
  int<lower = 0> N_obs;
  int<lower = 0> N_countries;
  int country[N_obs];
  int new_cases[N_obs];
  int new_deaths[N_obs];
  
  int total_cases[N_obs];
  int total_deaths[N_obs];
  vector[N_countries] pop;
}


parameters {
  
  
} 

transformed parameters {
  
}

model {
  
    
}


