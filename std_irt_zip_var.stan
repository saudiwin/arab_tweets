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
parameters {    
  vector[K] delta_1;                  // non-zero discriminations
  vector[K] delta_0;                  // zero discriminations
  matrix[T,J] alpha;               // ability of student j - mean ability
  //vector[K] beta_1;                // non-zero difficulty of question k
  vector[K] beta_0;                // zero difficulty of question k
  vector[4] adj_in;
  vector[4] adj_out;
  vector[3] alpha_int;
  real<lower=0> alpha_int_high;
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
  //alpha[1,2:4] ~ normal(0,1);
  alpha_int ~ normal(0,1);
  adj_in ~ normal(0,1);
  adj_out ~ normal(0,1);
  alpha_int_high ~ exponential(.1);
  sigma_time ~ exponential(.1);
  sigma_overall ~ exponential(.1);
  sigma_beta_0 ~ exponential(.1);
  
  //VAR priors with constraints on who influences who
  
  alpha[2:T,1] ~ normal(alpha_int_high + 
                        adj_in[1]*alpha[1:(T-1),1] + 
                        adj_out[1]*alpha[1:(T-1),2],
                sigma_time[1]);
  alpha[2:T,2] ~ normal(alpha_int[1] + 
                        adj_in[2]*alpha[1:(T-1),2] + 
                        adj_out[2]*alpha[1:(T-1),1],
                sigma_time[2]);
  alpha[2:T,3] ~ normal(alpha_int[2] + 
                        adj_in[3]*alpha[1:(T-1),3] + 
                        adj_out[3]*alpha[1:(T-1),4],
                sigma_time[3]);
  alpha[2:T,4] ~ normal(alpha_int[3] + 
                        adj_in[4]*alpha[1:(T-1),4] + 
                        adj_out[4]*alpha[1:(T-1),3],
                sigma_time[4]);

//citizen priors
  beta_0 ~ normal(0,sigma_beta_0);
  delta_1 ~ normal(0,5);   
  delta_0 ~ normal(0,5);
    for(n in 1:N) {
      if(y[n]==-9999) {
        1 ~ bernoulli_logit(delta_0[kk[n]]*alpha[tt[n],jj[n]] - beta_0[kk[n]]);
      } else {
        0 ~ bernoulli_logit(delta_0[kk[n]]*alpha[tt[n],jj[n]] - beta_0[kk[n]]);
        y[n] ~ normal(delta_1[kk[n]]*alpha[tt[n],jj[n]],sigma_overall);
      }
    }
}
