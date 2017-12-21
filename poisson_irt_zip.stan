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
  vector[K] delta_1;                  // non-zero discriminations
  vector[K] delta_0;                  // zero discriminations
  matrix[T,J] alpha;               // ability of student j - mean ability
  vector[K] beta_1;                // non-zero difficulty of question k
  vector[K] beta_0;                // zero difficulty of question k
  vector[2] adj;
  vector[2] gamma11;
  vector[2] gamma22;
  vector[2] gamma12;
  vector[2] gamma21;
  vector<lower=0>[4] sigma_time;
  real<lower=0> sigma_beta_0;
  real<lower=0> sigma_beta_1;
}

transformed parameters {
  // vector[K] beta;
  // 
  // beta = append_row(-sum(beta_free),beta_free);
  // vector[K] delta;
  // delta=append_row(delta_con_high,append_row(delta_con_low,delta_free));
}

model {
  alpha[1,1] ~ normal(start_vals[1],.1);
  alpha[1,2:4] ~ normal(0,1);
// delta_con_low ~ normal(0,3);
// delta_con_high ~ normal(0,3);
  //gamma_par1 ~ normal(0,2);
  //gamma_par2 ~ normal(0,2);
  gamma11 ~ normal(0,5);
  gamma21 ~ normal(0,5);
  gamma12 ~ normal(0,5);
  gamma22 ~ normal(0,5);
  adj ~ normal(0,5);
  //mean_delta ~ normal(0,1);
  sigma_time ~ exponential(.1);
  sigma_beta_1 ~ exponential(.1);
  sigma_beta_0 ~ exponential(.1);
  //sigma_delta ~ exponential(.1);

  alpha[2:T,1] ~ normal(alpha[1:(T-1),1] - gamma11[time_gamma].*(alpha[1:(T-1),1] - (adj[1])*alpha[1:(T-1),2]),
sigma_time[1]);
  alpha[2:T,2] ~ normal(alpha[1:(T-1),2] - gamma12[time_gamma].*(alpha[1:(T-1),2] - (1/adj[1])*alpha[1:(T-1),1]),
sigma_time[2]);
  alpha[2:T,3] ~ normal(alpha[1:(T-1),3] - gamma21[time_gamma].*(alpha[1:(T-1),3] - (adj[2])*alpha[1:(T-1),4]),
      sigma_time[3]);
  alpha[2:T,4] ~ normal(alpha[1:(T-1),4] - gamma22[time_gamma].*(alpha[1:(T-1),4] - (1/adj[2])*alpha[1:(T-1),3]),
        sigma_time[4]);

  
  //post-coup gammas
  
  beta_1 ~ normal(0,sigma_beta_1);          
  beta_0 ~ normal(0,sigma_beta_0);
  delta_1 ~ normal(0,5);   
  delta_0 ~ normal(0,5);
    for(n in 1:N) {
      if(y[n]==0) {
        1 ~ bernoulli_logit(delta_0[kk[n]]*alpha[tt[n],jj[n]] - beta_0[kk[n]]);
      } else {
        0 ~ bernoulli_logit(delta_0[kk[n]]*alpha[tt[n],jj[n]] - beta_0[kk[n]]);
        y[n] ~ poisson_log(delta_1[kk[n]]*alpha[tt[n],jj[n]] - beta_1[kk[n]]);
      }
    }
}
