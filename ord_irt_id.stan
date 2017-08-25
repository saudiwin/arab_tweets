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
  int<lower=1> tt[N]; // t for observation N
  int<lower=0> y[N];   // outcome for observation n
  vector[4] start_vals;
  int coup; // when the coup happens
  int time_gamma[T-1];
}
parameters {    
  vector[K-(id_num_low+id_num_high)] delta_free;                  // discriminations
  //real mean_beta;     //mean citizen response
  matrix[T,J] alpha;               // ability of student j - mean ability
  vector[K] beta;                // difficulty of question k
  vector[2] adj;
  ordered[C-1] steps;
  // vector<lower=0>[4] ts_sigma;
  vector<lower=0,upper=1>[2] gamma1;
  vector<lower=0,upper=1>[2] gamma2;
  real mean_delta;
  //real gamma_par1;
  //real gamma_par2;
  real<lower=0> sigma_beta;
  real<lower=0> sigma_delta;
  vector<upper=0>[id_num_low] delta_con_low;
  vector<lower=0>[id_num_high] delta_con_high;
}

transformed parameters {
  // vector[K] beta;
  // 
  // beta = append_row(-sum(beta_free),beta_free);
  vector[K] delta;
  delta=append_row(delta_con_high,append_row(delta_con_low,delta_free));
}

model {
  alpha[1,] ~ normal(start_vals,0.01);
delta_con_low ~ normal(0,3);
delta_con_high ~ normal(0,3);
  //gamma_par1 ~ normal(0,2);
  //gamma_par2 ~ normal(0,2);
  gamma1 ~ normal(0,3);
  gamma2 ~ normal(0,3);
  
  for(c in 1:(C-2)) 
    steps[c+1] - steps[c] ~ normal(0,3); 
  adj ~ normal(1,.25);
  mean_delta ~ normal(0,2);
  sigma_beta ~ exponential(1);
  sigma_delta ~ exponential(1);

  alpha[2:T,1] ~ normal(alpha[1:(T-1),1] - gamma1[time_gamma].*(alpha[1:(T-1),1] - (adj[1])*alpha[1:(T-1),2]),
.25);
  alpha[2:T,2] ~ normal(alpha[1:(T-1),2] - gamma1[time_gamma].*(alpha[1:(T-1),2] - (1/adj[1])*alpha[1:(T-1),1]),
.25);
  alpha[2:T,3] ~ normal(alpha[1:(T-1),3] - gamma2[time_gamma].*(alpha[1:(T-1),3] - (adj[2])*alpha[1:(T-1),4]),
      .25);
  alpha[2:T,4] ~ normal(alpha[1:(T-1),4] - gamma2[time_gamma].*(alpha[1:(T-1),4] - (1/adj[2])*alpha[1:(T-1),3]),
        .25);

  
  //post-coup gammas
  
  beta ~ normal(0,sigma_beta);          
  delta_free ~ normal(mean_delta,sigma_delta);   

  for(n in 1:N)
    y[n] ~ ordered_logistic(delta[kk[n]]*alpha[tt[n],jj[n]] - beta[kk[n]],steps);
}
