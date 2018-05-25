data {
  int<lower=1> J;              // number of elites
  int<lower=1> K;              // number of citizens
  int<lower=1> N;              // number of observations
  int<lower=1> T;  //number of time points
  int id_num_high;
  int id_num_low;
  int<lower=1,upper=J> jj[N];  // elite for observation n
  int<lower=1,upper=K> kk[N];  // student for observation n
  int<lower=1> tt[N]; // t for observation N
  real y[N];   // outcome for observation n
  vector[4] start_vals;
  int coup; // when the coup happens
  int time_gamma[T-1];
}
transformed data {
  vector[T-1] time_betax;
  for(t in 1:(T-1)) {
    if(time_gamma[t]==1) {
      time_betax[t] = 0.0;
    } else {
      time_betax[t] = 1.0;
    }
    
  }
}
parameters {    
  vector[K] delta_1;                  // non-zero discriminations
  vector[K] delta_0;                  // zero discriminations
  matrix[T,J] alpha;               // ability of student j - mean ability
  //vector[K] beta_1;                // non-zero difficulty of question k
  vector[K] beta_0;                // zero difficulty of question k
  vector[2] adj;
  vector<lower=0>[1] gamma11;
  vector<lower=0>[1] gamma22;
  vector<lower=0>[1] gamma12;
  vector<lower=0>[1] gamma21;
  vector[2] trend;
  vector<lower=0>[4] sigma_time;
  real<lower=0> sigma_beta_0;
  real<lower=0> sigma_overall;
  vector[4] betax;
}

model {
  //alpha[1,] ~ normal(start_vals[1],.1);
  alpha[1,] ~ normal(0,1);
// delta_con_low ~ normal(0,3);
// delta_con_high ~ normal(0,3);
  //gamma_par1 ~ normal(0,2);
  //gamma_par2 ~ normal(0,2);
  gamma11 ~ exponential(.1);
  gamma21 ~ exponential(.1);
  gamma12 ~ exponential(.1);
  gamma22 ~ exponential(.1);
  adj ~ normal(0,5);
  betax ~ normal(0,5);
  //mean_delta ~ normal(0,1);
  sigma_time ~ exponential(.1);
  sigma_overall ~ exponential(.1);
  sigma_beta_0 ~ exponential(.1);
  //sigma_delta ~ exponential(.1);

  alpha[2:T,1] ~ normal(alpha[1:(T-1),1] - gamma11[1]*(alpha[1:(T-1),1] - (adj[1])*alpha[1:(T-1),2]) + betax[1]*time_betax + trend[1],
sigma_time[1]);
  alpha[2:T,2] ~ normal(alpha[1:(T-1),2] - gamma12[1]*(alpha[1:(T-1),2] - (1/adj[1])*alpha[1:(T-1),1]) + betax[2]*time_betax,
sigma_time[2]);
  alpha[2:T,3] ~ normal(alpha[1:(T-1),3] - gamma21[1]*(alpha[1:(T-1),3] - (adj[2])*alpha[1:(T-1),4]) + betax[3]*time_betax + trend[1],
      sigma_time[3]);
  alpha[2:T,4] ~ normal(alpha[1:(T-1),4] - gamma22[1]*(alpha[1:(T-1),4] - (1/adj[2])*alpha[1:(T-1),3]) + betax[4]*time_betax,
        sigma_time[4]);

  
  //post-coup gammas
  
  beta_1 ~ normal(0,sigma_beta_1);          
  beta_0 ~ normal(0,sigma_beta_0);
  delta_1 ~ normal(0,5);   
  delta_0 ~ normal(0,5);
    for(n in 1:N) {
      if(y[n]==0.0) {
        1 ~ bernoulli_logit(delta_0[kk[n]]*alpha[tt[n],jj[n]] - beta_0[kk[n]]);
      } else {
        0 ~ bernoulli_logit(delta_0[kk[n]]*alpha[tt[n],jj[n]] - beta_0[kk[n]]);
        y[n] ~ normal(delta_1[kk[n]]*alpha[tt[n] - beta_1[kk[n]],jj[n]],sigma_overall);
      }
    }
}
