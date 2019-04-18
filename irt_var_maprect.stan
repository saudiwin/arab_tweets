functions {
 vector overT(vector allparams, vector alpha,
               real[] time_points, int[] allintdata) {
                 
    // load indices
    int J = (num_elements(alpha)-1)/2;
    int K = (num_elements(allparams)-(5*J+1))/4;
    real t = time_points[1];
    int N = num_elements(allintdata)/4; // has to be hard-coded unfortunately, no way to 
                                        // pass other info in
    int gen_out[N] = allintdata[1:N]; // retrieve indices in order
    int country_code[N] = allintdata[(N+1):(2*N)];
    int jj[N] = allintdata[((2*N)+1):(3*N)];
    int kk[N] = allintdata[((3*N)+1):(4*N)];
    
    
    
    
    // get alphas for this time point
    vector[J] alphatm1 = alpha[1:J];
    vector[J] alphat = alpha[(J+1):(2*J)];
    vector[J] alpha_log;
     
    
    //unpack allparams
    vector[K] delta_0 = allparams[1:K];
    vector[K] beta_0 = allparams[(K+1):(2*K)];
    vector[K] delta_1 = allparams[(2*K+1):(3*K)];
    vector[K] beta_1 = allparams[(3*K+1):(4*K)];
    vector[J] sigma_time = allparams[(4*K+1):(4*K+J)];
    vector[J] adj_in = allparams[(4*K+J+1):(4*K+2*J)];
    vector[J] adj_out = allparams[(4*K+2*J+1):(4*K+3*J)];
    vector[J] betax = allparams[(4*K+3*J+1):(4*K+4*J)];
    vector[J] alpha_int = allparams[(4*K+4*J+1):(4*K+5*J)];
    real country = allparams[num_elements(allparams)];
    
    vector[1] lp_out;
    
    

    real time_gamma = alpha[(2*J)+1];

   
    if(t!=1) {
          //VAR priors with constraints on who influences who
  
    alpha_log[1] = normal_lpdf(alphat[1]|alpha_int[1] + adj_in[1]*alphatm1[1] + 
                        adj_out[1]*alphatm1[1] +
                        betax[1]*time_gamma,
                sigma_time[1]);

    alpha_log[2] = normal_lpdf(alphat[2]|alpha_int[2] + adj_in[2]*alphatm1[2] + 
                        adj_out[2]*alphatm1[2] +
                        betax[2]*time_gamma,
                sigma_time[2]);
    alpha_log[3] = normal_lpdf(alphat[3]|alpha_int[3] + adj_in[3]*alphatm1[3] + 
                        adj_out[3]*alphatm1[3] +
                        betax[3]*time_gamma,
                sigma_time[3]);

    alpha_log[4] = normal_lpdf(alphat[4]|alpha_int[4] + adj_in[4]*alphatm1[4] + 
                        adj_out[4]*alphatm1[4] +
                        betax[4]*time_gamma,
                sigma_time[4]);
    } else {
      for(j in 1:J) {
        alpha_log[j] = normal_lpdf(alphat[j]|0,3);
      }
      
    }
   

                
  // loop over outcome
  // use hurdle model for missing data (-9999)
  // conditional on passing hurdle, use normal distribution IRT model with time-varying
  // parameters for elites
  
    for(n in 1:N) {
      if(gen_out[n]==-9999) {
        real lp =  bernoulli_logit_lpmf(1 | delta_0[kk[n]]*(alphat[jj[n]] + 
                                        country*country_code[n])  - beta_0[kk[n]]);
        lp_out = [lp + sum(alpha_log)]';
      } else {
        real lp = bernoulli_logit_lpmf(0 | delta_0[kk[n]]*(alphat[jj[n]] + 
                                        country*country_code[n])  - beta_0[kk[n]]); 
        real ll = poisson_log_lpmf(gen_out|delta_1[kk]*(alphat[jj[n]] + country*country_code[n]) -
                       beta_1[kk[n]]);
        lp_out = [lp + ll + sum(alpha_log)]';
      }
    }
   return lp_out;
 }
}
data {
  int<lower=1> J;              // number of elites
  int<lower=1> K;              // number of citizens
  int<lower=1> N;              // number of observations
  int<lower=1> T;  //number of time points
  int<lower=1> C; //number of total data columns
  int<lower=1> S; //number of shards in data for map_rect
  int alldata[S,N]; // data in shard format
  real time_points[S,1]; // simple time counter
  vector[T-1] time_gamma; //binary vector indicating when coup happens
}
transformed data {
  // calculate how the indices will work for the transformed parameters
  // two time points for each J and two parameters for each K in the vector plus
  // time-series pars for J & county FE
  int vP = 2*J + 1; // add in time_gamma one per t
  int dP = K*4 + J*5 + 1; // ad in ID  vars
  //need a vector to pad the time-varying parameters for the case when T=1
  vector[J] padT = rep_vector(0,J);
  real x_r[S,0]; // nothing vector to fil out map_rect 
}
parameters {    
  vector[K] delta_1;                  // non-zero discriminations
  vector[K] delta_0;                  // zero discriminations
  matrix[T,J] alpha;               // time-varying ideal points for elites
  vector[K] beta_0;                // zero difficulty of question k
  vector[K] beta_1;                // zero difficulty of question k
  vector[4] adj_in;  //adjustment parameters
  vector[4] adj_out; //adjustment parameters
  vector[4] alpha_int; //drift
  vector[4] betax; //effects of coup
  real<lower=0> country; //dummy for country-level fixed effects
  vector<lower=0>[4] sigma_time; //heteroskedastic variance by ideological group
  real<lower=0> sigma_beta_0; //hierarchical variance for zero betas
  real<lower=0> sigma_beta_1; //hierarchical variance for zero betas
  real<lower=0> sigma_overall; //variance for top-level normal distribution
}

transformed parameters {
  // pack all the elite parameters into an array vector for usage in map_rect
  vector[vP] varparams[S];
  //all citizen params are in non-varying vectors
  vector[dP] dparams;
  
  // construct by looping over T and shards
  // where we known that T = number of shards
  
  for(t in 1:T) {
    // append t-1 by t
    // add in time_gamma_
    if(t==1) {
      varparams[t] = append_row(padT,append_row(alpha[t,]',
                                0));
    } else {
      varparams[t] = append_row(alpha[t-1,]',append_row(alpha[t,]',
                                time_gamma[t-1]));
    }
    
  }
  
  // append all other parameters to one big vector that is passed to all shards
  // order: 
  // 1. discrim abs for K
  // 2. diff abs for K
  // 3. discrim obs for K
  // 4. diff obs for K
  // 5. sigmas for J
  // 6. adj_in for J
  // 7. ajd_out for J
  // 8. betax for J
  // 9. alpha_int for J
  // 10. country
  
  
  dparams = append_row(delta_0,
            append_row(beta_0,
            append_row(delta_1,
            append_row(beta_1,
            append_row(sigma_time,
            append_row(adj_in,
            append_row(adj_out,
            append_row(betax,
            append_row(alpha_int,country)))))))));
  
}

model {
  
  alpha_int ~ normal(0,3);
  adj_in ~ normal(0,3);
  adj_out ~ normal(0,3);
  country ~ exponential(.1);
  sigma_time ~ exponential(.1);
  sigma_overall ~ exponential(.1);
  sigma_beta_0 ~ exponential(.1);
  sigma_beta_1 ~ exponential(.1);
  betax ~ normal(0,5);
//citizen priors
  beta_0 ~ normal(0,sigma_beta_0);
  beta_1 ~ normal(0,sigma_beta_1);
  delta_1 ~ normal(0,3);   
  delta_0 ~ normal(0,3);
  
  // parallelize the likelihood with map_rect
  
  target += sum(map_rect(overT, dparams, varparams, time_points, alldata));

}

generated quantities {
  matrix[T,J] alpha_country; //recalculate alpha with country intercepts included  

  alpha_country[,1] = alpha[,1];
  alpha_country[,2] = alpha[,2] + country;
  alpha_country[,3] = alpha[,3];
  alpha_country[,4] = alpha[,4] + country;
}
