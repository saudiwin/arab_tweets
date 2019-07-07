data {
  int<lower=1> J1;              // number of elites
  int<lower=1> J2;              // number of elites
  int<lower=1> K;              // number of citizens
  int<lower=1> N;              // number of observations
  int id_num_high;
  int id_num_low;
  int<lower=1,upper=J1> jj1[N];  // elite for observation n
  int<lower=1,upper=J2> jj2[N];  // elite for observation n
  int<lower=1,upper=K> kk[N];  // student for observation n
  int y[N];   // outcome for observation n
  vector[N] country_code; //indicator for Egypt
}
parameters {    
  vector[K] delta_11;                  // non-zero discriminations for Islam
  vector[K] delta_21;                  // non-zero discriminations for democracy
  vector[K] delta_10;                  // zero discriminations forIslam
  vector[K] delta_20;                  // non-zero discriminations for democracy
  vector[J1] alpha1;               // ideal points for Islam
  vector[J2-id_num_high-id_num_low] alpha2_free; // ideal points for democracy
  vector[id_num_high] democrats;
  vector[id_num_low] antidem;
  
  vector[K] beta_1;                // non-zero difficulty of question k
  vector[K] beta_0;                // zero difficulty of question k
  real<lower=0> country;
  real<lower=0> sigma_beta_0;
}

transformed parameters {
  vector[J2] alpha2_full;
  
  alpha2_full = append_row(append_row(democrats,antidem),
                            alpha2_free);
}

model {

  alpha2_free ~ normal(0,1); // allow non-constrained to float
  alpha1[1] ~ normal(-1,.001);
  alpha1[4] ~ normal(1,.001);
  alpha1[2] ~ normal(0,3);
  alpha1[3] ~ normal(0,3);

  country ~ exponential(1);
  sigma_beta_0 ~ exponential(1);
  
  democrats[1:4] ~ normal(2,.001);
  antidem[1:2] ~ normal(-2,.001);
  democrats[4:id_num_high] ~ normal(0,3);
  antidem[id_num_low] ~ normal(0,3);

//citizen priors
  //beta_0 ~ normal(0,sigma_beta_0);
  beta_0 ~ normal(0,3);
  beta_1 ~ normal(0,3);
  delta_11 ~ normal(0,3);   
  delta_21 ~ normal(0,3);
  delta_10 ~ normal(0,3);   
  delta_20 ~ normal(0,3);   
    for(n in 1:N) {
      if(y[n]==99999) {
        1 ~ bernoulli_logit(delta_10[kk[n]]*alpha1[jj1[n]] +
                            delta_20[kk[n]]*alpha2_full[jj2[n]] - beta_0[kk[n]]);
      } else {
        0 ~ bernoulli_logit(delta_10[kk[n]]*alpha1[jj1[n]] +
                            delta_20[kk[n]]*alpha2_full[jj2[n]] - beta_0[kk[n]]);
        y[n] ~ poisson_log(delta_11[kk[n]]*alpha1[jj1[n]] +
                      delta_21[kk[n]]*alpha2_full[jj2[n]] - beta_1[kk[n]]);
      }
    }
    
    // y ~ poisson_log(delta_11[kk].*alpha1[jj1] +
    //                 delta_21[kk].*alpha2_full[jj2] - beta_1[kk]);
}
