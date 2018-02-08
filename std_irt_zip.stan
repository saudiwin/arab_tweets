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
  int country_code[N]; //indicator for Tunisia
  real y[N];   // outcome for observation n
  vector[4] start_vals;
  int coup; // when the coup happens
  int time_gamma[T-1];
}
parameters {    
  vector[K] delta_1;                  // non-zero discriminations
  vector[K] delta_0;                  // zero discriminations
  matrix[T,J] alpha;               // ability of student j - mean ability
  //vector[K] beta_1;                // non-zero difficulty of question k
  vector[K] beta_0;                // zero difficulty of question k
  vector[4] adj;
  vector<lower=0>[2] gamma11;
  vector<lower=0>[2] gamma22;
  vector<lower=0>[2] gamma12;
  vector<lower=0>[2] gamma21;
  real<lower=0> country;
  vector<lower=0>[4] sigma_time;
  real<lower=0> sigma_beta_0;
  real<lower=0> sigma_overall;
}

transformed parameters {
  // vector[K] beta;
  // 
  // beta = append_row(-sum(beta_free),beta_free);
  // vector[K] delta;
  // delta=append_row(delta_con_high,append_row(delta_con_low,delta_free));
}

model {
  //alpha[1,1] ~ normal(start_vals[1],.1);
  alpha[1,] ~ normal(0,1);
// delta_con_low ~ normal(0,3);
// delta_con_high ~ normal(0,3);
  //gamma_par1 ~ normal(0,2);
  //gamma_par2 ~ normal(0,2);
  gamma11 ~ exponential(.1);
  gamma21 ~ exponential(.1);
  gamma12 ~ exponential(.1);
  gamma22 ~ exponential(.1);
  country ~ exponential(.1);
  adj ~ normal(0,2);
  //mean_delta ~ normal(0,1);
  sigma_time ~ exponential(.1);
  sigma_overall ~ exponential(.1);
  sigma_beta_0 ~ exponential(.1);
  //sigma_delta ~ exponential(.1);

  alpha[2:T,1] ~ normal(alpha[1:(T-1),1] - gamma11[time_gamma].*(alpha[1:(T-1),1] + (adj[1]/adj[2])*alpha[1:(T-1),2]),
sigma_time[1]);
  alpha[2:T,2] ~ normal(alpha[1:(T-1),2] - gamma12[time_gamma].*(alpha[1:(T-1),2] + (adj[2]/adj[1])*alpha[1:(T-1) ,1]),
sigma_time[2]);
  alpha[2:T,3] ~ normal(alpha[1:(T-1),3] - gamma21[time_gamma].*(alpha[1:(T-1),3] + (adj[3]/adj[4])*alpha[1:(T-1),4]),
      sigma_time[3]);
  alpha[2:T,4] ~ normal(alpha[1:(T-1),4] - gamma22[time_gamma].*(alpha[1:(T-1),4] + (adj[4]/adj[3])*alpha[1:(T-1),3]),
        sigma_time[4]);

  
  //post-coup gammas
  
  //beta_1 ~ normal(0,sigma_beta_1);          
  beta_0 ~ normal(0,sigma_beta_0);
  delta_1 ~ normal(0,5);   
  delta_0 ~ normal(0,5);
    for(n in 1:N) {
      if(y[n]==-9999.0) {
        1 ~ bernoulli_logit(delta_0[kk[n]]*alpha[tt[n],jj[n]] + 
                      delta_1[kk[n]]*country*country_code[n] - beta_0[kk[n]]);
      } else {
        0 ~ bernoulli_logit(delta_0[kk[n]]*alpha[tt[n],jj[n]] + 
                      delta_1[kk[n]]*country*country_code[n] - beta_0[kk[n]]);
        y[n] ~ normal(delta_1[kk[n]]*alpha[tt[n],jj[n]] + 
                      delta_1[kk[n]]*country*country_code[n],
                      sigma_overall);
      }
    }
}
