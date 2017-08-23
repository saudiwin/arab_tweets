data {
  int<lower=1> J;              // number of elites
  int<lower=1> K;              // number of citizens
  int<lower=1> N;              // number of observations
  int<lower=1> T;  //number of time points
  int<lower=1,upper=J> jj[N];  // elite for observation n
  int<lower=1,upper=K> kk[N];  // student for observation n
  int<lower=1> tt[N]; // t for observation N
  int<lower=0> y[N];   // outcome for observation n
  vector[4] start_vals;
  int coup; // when the coup happens
  int time_gamma[T-1];
}
transformed data {
  /*
  vector[2] adj;
  
  adj[1] = 0.3;
  adj[2] = 0.7;
  */
  vector[2] time_counter;
  
  time_counter[1] = 0.0;
  time_counter[2] = 1.0;
  
}
parameters {    
  vector[K] delta;                  // discriminations
  //real mean_beta;     //mean citizen response
  matrix[T,J] alpha;               // ability of student j - mean ability
  vector[K-1] beta_free;                // difficulty of question k
  vector[2] adj;
  // vector<lower=0>[4] ts_sigma;
  vector<lower=0,upper=1>[2] gamma1;
  vector<lower=0,upper=1>[2] gamma2;
  real mean_delta;
  //real gamma_par1;
  //real gamma_par2;
  real<lower=0> sigma_beta;
  real<lower=0> sigma_delta;
}

transformed parameters {
  vector[K] beta;
  
  beta = append_row(-sum(beta_free),beta_free);
}

model {
  alpha[1,] ~ normal(start_vals,0.001);
  //gamma_par1 ~ normal(0,2);
  //gamma_par2 ~ normal(0,2);
  gamma1 ~ cauchy(0,1);
  gamma2 ~ cauchy(0,1);

  // ts_sigma ~ normal(-0.5,1);
  adj ~ normal(1,.25);
  mean_delta ~ normal(0,2);
  sigma_beta ~ exponential(1.5);
  sigma_delta ~ exponential(1.5);
  //pre-coup gammas

  alpha[2:T,1] ~ normal(alpha[1:(T-1),1] - gamma1[time_gamma].*(alpha[1:(T-1),1] - (adj[1])*alpha[1:(T-1),2]),
.25);
  alpha[2:T,2] ~ normal(alpha[1:(T-1),2] - gamma1[time_gamma].*(alpha[1:(T-1),2] - (1/adj[1])*alpha[1:(T-1),1]),
.25);
  alpha[2:T,3] ~ normal(alpha[1:(T-1),3] - gamma2[time_gamma].*(alpha[1:(T-1),3] - (adj[2])*alpha[1:(T-1),4]),
      .25);
  alpha[2:T,4] ~ normal(alpha[1:(T-1),4] - gamma2[time_gamma].*(alpha[1:(T-1),4] - (1/adj[2])*alpha[1:(T-1),3]),
        .25);

  
  //post-coup gammas
  
  beta_free ~ normal(0,2);          
  delta ~ normal(mean_delta,sigma_delta);       
  for(n in 1:N)
    y[n] ~ poisson_log(delta[kk[n]]*alpha[tt[n],jj[n]] - beta[kk[n]]);
}
