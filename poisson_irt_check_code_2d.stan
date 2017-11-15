data {
  int<lower=1> J;              // number of elites
  int<lower=1> K;              // number of citizens
  int<lower=1> N;              // number of observations
  int id_num_high;
  int id_num_low;
  int<lower=1,upper=J> jj[N];  // elite for observation n
  int<lower=1,upper=K> kk[N];  // student for observation n

  int<lower=0> y[N];   // outcome for observation n
  vector[4] start_vals;
}
parameters {    
  vector[K-2] delta_free_1d;                  // discriminations
  vector[K-2] delta_free_2d;
  real<lower=0> mean_beta;     //mean citizen response
  vector[J] alpha_1d;               // ability of student j - mean ability
  vector[J] alpha_2d;
  vector[K] beta;                // difficulty of question k
  vector<lower=0>[2] adj;
  // vector<lower=0>[4] ts_sigma;
  vector<lower=0>[2] gamma1;
  vector<lower=0>[2] gamma2;
  real mean_delta;
  //real gamma_par1;
  //real gamma_par2;
  real<lower=0> sigma_beta;
  real<lower=0> sigma_delta;
  real<lower=0> islamist;
  real<lower=0> country;
  vector[2] free_params;
}

transformed parameters {
  vector[K] delta_1d;
  vector[K] delta_2d;
  vector[4] constrain_params;
  
  constrain_params[1] = islamist;
  constrain_params[2] = free_params[1];
  constrain_params[3] = free_params[2];
  constrain_params[4] = country;
  
  delta_1d = append_row(constrain_params[1:2],delta_free_1d);
  delta_2d = append_row(constrain_params[3:4],delta_free_2d);
}

model {
  //alpha[1,] ~ normal(start_vals,.1);
// delta_con_low ~ normal(0,3);
// delta_con_high ~ normal(0,3);
  //gamma_par1 ~ normal(0,2);
  //gamma_par2 ~ normal(0,2);
  mean_beta ~ normal(0,1);
  gamma1 ~ exponential(2);
  gamma2 ~ exponential(2);
  //mean_beta ~ normal(0,1);
  //for(c in 1:(C-2)) 
    //steps[c+1] - steps[c] ~ normal(0,5); 
  adj ~ exponential(1);
  mean_delta ~ normal(0,1);
  sigma_beta ~ exponential(.1);
  sigma_delta ~ exponential(.1);
  alpha_1d ~ normal(0,1);
  alpha_2d ~ normal(0,1);
  islamist ~ exponential(1);
  country ~ exponential(1);
  free_params ~ normal(0,5);
  
  //post-coup gammas
  
  beta ~ normal(0,sigma_beta);          

  delta_free_1d ~ normal(0,5);   
  delta_free_2d ~ normal(0,5);

  for(n in 1:N)
    y[n] ~ poisson_log(delta_1d[kk[n]]*alpha_1d[jj[n]] + 
                        delta_2d[kk[n]]*alpha_2d[jj[n]] - beta[kk[n]]);
}
