
data {
  int<lower = 0> N_agegroups;
  int<lower = 0> age_cases[N_agegroups];
  
}

parameters {
  simplex[N_agegroups] theta;
}


model {
  age_cases ~ multinomial(theta);
}


