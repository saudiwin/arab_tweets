data {
  int<lower=1> J;              // number of elites
  int<lower=1> K;              // number of citizens
  int<lower=1> N;              // number of observations
  int<lower=1> T;  //number of time points
  int C;
  int id_num_high;
  int id_num_low;
  int<lower=1,upper=J> jj[N];  // elite for observation n
  int<lower=1,upper=K> kk[N];  // student for observation n
  int country_num[N];
  int<lower=1> tt[N]; // t for observation N
  int<lower=0> y[N];   // outcome for observation n
  vector[4] start_vals;
  int coup; // when the coup happens
  int time_gamma[T-1];
}
parameters {    
  vector[K] delta_d1;                  // discriminations
  vector[K] delta_d2;                  // discriminations
  real<lower=0> mean_beta;     //mean citizen response
  matrix[T,J] alpha_d2;               // ability of student j - mean ability
  vector[J] alpha_d1;
  vector[K] beta;                // difficulty of question k
  vector[2] adj;
  vector[2] country;
  ordered[C-1] steps;
  // vector<lower=0>[4] ts_sigma;
  vector<lower=0,upper=1>[2] gamma1;
  vector<lower=0,upper=1>[2] gamma2;
  real mean_delta;
  real<lower=0> sigma_time1;
  real<lower=0> sigma_time2;
  //real gamma_par1;
  //real gamma_par2;
  real<lower=0> sigma_beta;
  real<lower=0> sigma_delta;
}

transformed parameters {
  // vector[K] beta;
  // 
  // beta = append_row(-sum(beta_free),beta_free);
  // vector[K] delta;
  // delta=append_row(delta_con_high,append_row(delta_con_low,delta_free));
}

model {
  //alpha_d2[1,1] ~ normal(start_vals[1],0.01);
  alpha_d2[1,] ~ normal(0,5);
  //alpha_d1[1,] ~ normal(0,5);
  //alpha_d1[1] ~ normal(1,0.01);
  //alpha_d1[4] ~ normal(-1,0.01);
  //alpha_d1[2:3] ~ normal(0,5);
// delta_con_low ~ normal(0,3);
// delta_con_high ~ normal(0,3);
  //gamma_par1 ~ normal(0,2);
  //gamma_par2 ~ normal(0,2);
  mean_beta ~ normal(0,1);
  gamma1 ~ normal(0,3);
  gamma2 ~ normal(0,3);
  //mean_beta ~ normal(0,1);
  for(c in 1:(C-2)) 
    steps[c+1] - steps[c] ~ normal(0,5); 
  adj ~ normal(1,.25);
  mean_delta ~ normal(0,1);
  sigma_time1 ~ exponential(.1);
  sigma_time2 ~ exponential(.1);
  sigma_beta ~ exponential(.1);
  sigma_delta ~ exponential(.1);
  alpha_d1 ~ normal(0,1);
  //add auto-regressive priors for the country-level variation
  //to_vector(alpha_d1[2:T,]) ~ normal(to_vector(alpha_d1[1:(T-1),]),sigma_time1);

  alpha_d2[2:T,1] ~ normal(alpha_d2[1:(T-1),1] - gamma1[time_gamma].*(alpha_d2[1:(T-1),1] - (adj[1])*alpha_d2[1:(T-1),2]),
sigma_time2);
  alpha_d2[2:T,2] ~ normal(alpha_d2[1:(T-1),2] - gamma1[time_gamma].*(alpha_d2[1:(T-1),2] - (1/adj[1])*alpha_d2[1:(T-1),1]),
sigma_time2);
  alpha_d2[2:T,3] ~ normal(alpha_d2[1:(T-1),3] - gamma2[time_gamma].*(alpha_d2[1:(T-1),3] - (adj[2])*alpha_d2[1:(T-1),4]),
      sigma_time2);
  alpha_d2[2:T,4] ~ normal(alpha_d2[1:(T-1),4] - gamma2[time_gamma].*(alpha_d2[1:(T-1),4] - (1/adj[2])*alpha_d2[1:(T-1),3]),
        sigma_time2);

  
  //post-coup gammas
  
  beta ~ normal(0,sigma_beta);          

  delta_d2 ~ normal(0,5); 
  delta_d1 ~ normal(0,5);

  for(n in 1:N)
    y[n] ~ ordered_logistic(delta_d1[kk[n]]*alpha_d1[jj[n]] + delta_d2[kk[n]]*alpha_d2[tt[n],jj[n]] - beta[kk[n]],steps);
}
