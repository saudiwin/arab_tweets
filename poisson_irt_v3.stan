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
parameters {    
  vector[K] delta;                  // discriminations
  //real mean_beta;     //mean citizen response
  matrix[T,J] alpha;               // ability of student j - mean ability
  vector[K] beta;                // difficulty of question k
  vector[2] adj;
  // vector<lower=0>[4] ts_sigma;
  vector[2] gamma11;
  vector[2] gamma12;
  vector[2] gamma21;
  vector[2] gamma22;
  vector<lower=0>[4] sigma_time;
  real mean_delta;
  //real gamma_par1;
  //real gamma_par2;
  real<lower=0> sigma_beta;
  real<lower=0> sigma_delta;
  // vector<upper=0>[2] delta_con_low;
  // vector<lower=0>[2] delta_con_high;
}

transformed parameters {
  // vector[K] beta;
  // 
  // beta = append_row(-sum(beta_free),beta_free);
  // vector[K] delta;
  // delta=append_row(delta_con_low,append_row(delta_con_high,delta_free));
}

model {
  alpha[1,1] ~ normal(start_vals[1],0.01);
  alpha[1,3] ~ normal(start_vals[3],.01);
  alpha[1,2] ~ normal(0,1);
  alpha[1,4] ~ normal(0,1);

  //gamma_par1 ~ normal(0,2);
  //gamma_par2 ~ normal(0,2);
  gamma11 ~ normal(0,3);
  gamma12 ~ normal(0,3);
  gamma21 ~ normal(0,3);
  gamma22 ~ normal(0,3);
  sigma_time ~ exponential(.1);

  // ts_sigma ~ normal(-0.5,1);
  adj ~ normal(0,5);
  mean_delta ~ normal(0,2);
  sigma_beta ~ exponential(.1);
  sigma_delta ~ exponential(.1);
  // delta_con_low ~ normal(0,2);
  // delta_con_high ~ normal(0,2);
  //pre-coup gammas

  alpha[2:T,1] ~ normal(alpha[1:(T-1),1] - gamma11[time_gamma].*(alpha[1:(T-1),1] - (adj[1])*alpha[1:(T-1),2]),sigma_time[1]);
  alpha[2:T,2] ~ normal(alpha[1:(T-1),2] - gamma12[time_gamma].*(alpha[1:(T-1),2] - (1/adj[1])*alpha[1:(T-1),1]),
sigma_time[2]);
  alpha[2:T,3] ~ normal(alpha[1:(T-1),3] - gamma21[time_gamma].*(alpha[1:(T-1),3] - (adj[2])*alpha[1:(T-1),4]),
      sigma_time[3]);
  alpha[2:T,4] ~ normal(alpha[1:(T-1),4] - gamma22[time_gamma].*(alpha[1:(T-1),4] - (1/adj[2])*alpha[1:(T-1),3]),
        sigma_time[4]);

  
  //post-coup gammas
  
  beta ~ normal(0,2);          
  delta ~ normal(0,2);       
  for(n in 1:N)
    y[n] ~ poisson_log(delta[kk[n]]*alpha[tt[n],jj[n]] - beta[kk[n]]);
}
