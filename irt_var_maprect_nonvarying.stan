functions {
 vector overT(vector allparams, vector cit,
               real[] time_points, int[] allintdata) {
                 
    // load indices
    int J = allintdata[1];
    int T = allintdata[2];
    int coup = allintdata[3];
    real t = time_points[1];
    int N = (num_elements(allintdata)-3)/4; // has to be hard-coded unfortunately, no way to 
                                        // pass other info in
    int gen_out[N] = allintdata[4:(N+3)]; // retrieve indices in order
    int country_code[N] = allintdata[(N+4):(2*N+3)];
    int tt[N] = allintdata[((2*N)+4):(3*N+3)];
    int jj[N] = allintdata[((3*N)+4):(4*N+3)];
    
    // citizen params 
    
    real delta_1 = cit[1];
    real delta_0 = cit[2];
    real beta_1 = cit[3];
    real beta_0 = cit[4];
    
    
    
    
    // get alphas for this time point
    matrix[T,J] alpha = to_matrix(allparams[1:T*J],T,J);
     
    
    //unpack allparams
    vector[J] sigma_time = allparams[(T*J+1):(T*J+J)];
    vector[J] adj_in = allparams[(T*J+J+1):(T*J+2*J)];
    vector[J] adj_out = allparams[(T*J+2*J+1):(T*J+3*J)];
    vector[J] betax = allparams[(T*J+3*J+1):(T*J+4*J)];
    vector[J] alpha_int = allparams[(T*J+4*J+1):(T*J+5*J)];
    real country = allparams[num_elements(allparams)];
    
    vector[N] lp_out;
    vector[N] predict1;
    vector[N] predict2;
                
  // loop over outcome
  // use hurdle model for missing data (-9999)
  // conditional on passing hurdle, use normal distribution IRT model with time-varying
  // parameters for elites
  //print(tt);
  //print(jj);
  
    for(n in 1:N) {
      
      real this_alpha;
      real alpha_log;

      //print(tt[n]);
      //print(jj[n]);

      this_alpha = alpha[tt[n],jj[n]];

    //   if(tt[n]>1) {
    // 
    //     // determine which elite is in the out-group
    //     int other;
    //     if(jj[n]==1) {
    //       other = 2;
    //     } else if(jj[n]==2) {
    //       other=1;
    //     } else if(jj[n]==3) {
    //       other=4;
    //     } else if(jj[n]==4) {
    //       other=3;
    //     }
    // 
    //     if(tt[n]<coup) {
    //       //VAR priors with constraints on who influences who
    //     alpha_log = normal_lpdf(this_alpha|alpha_int[jj[n]] + adj_in[jj[n]]*alpha[tt[n]-1,jj[n]] +
    //                     adj_out[jj[n]]*alpha[tt[n]-1,other],
    //             sigma_time[jj[n]]);
    //     } else {
    //       alpha_log = normal_lpdf(this_alpha|alpha_int[jj[n]] + adj_in[jj[n]]*alpha[tt[n]-1,jj[n]] +
    //                     adj_out[jj[n]]*alpha[tt[n]-1,other] +
    //                     betax[jj[n]],
    //             sigma_time[jj[n]]);
    //     }
    // 
    //   } else {
    //       alpha_log = normal_lpdf(this_alpha|0,3);
    // 
    // }
      
      if(gen_out[n]==99999) {
        real lp =  bernoulli_logit_lpmf(1 | delta_0*(this_alpha + 
                                        country*country_code[n])  - beta_0);
        lp_out[n] = lp;
      } else {
        real lp = bernoulli_logit_lpmf(0 | delta_0*(this_alpha + 
                                        country*country_code[n])  - beta_0); 
        real ll = poisson_log_lpmf(gen_out[n]|delta_1*(this_alpha + country*country_code[n]) -
                       beta_1);
        lp_out[n] = lp + ll;
      }
    }
   return [sum(lp_out)]'; // return joint probability of the shard
 }
}
data {
  int<lower=1> J;              // number of elites
  int<lower=1> K;              // number of citizens
  int<lower=1> N;              // number of observations per shard
  int<lower=1> T;  //number of time points
  int<lower=1> C; //number of total data columns
  int<lower=1> S; //number of shards in data for map_rect = number of citizens
  int alldata[S,N]; // data in shard format
  real time_points[S,1]; // counter for citizen IDs
  int coup; //when the coup happens
  vector[T-1] time_gamma;
}
transformed data {
  // calculate how the indices will work for the transformed parameters
  // varying is easy for citizens
  // static must include *all* time-varying parameters as they *cannot* vary across shards
  int vP = 4; // need all citizen parameters discrimination + difficulty for one citizen = 4 parameters
  int dP = T*J + J*5 + 1; // all alpha parameters plus adjustment/betax/sigmas/country
  //need a vector to pad the time-varying parameters for the case when T=1
  vector[J] padT = rep_vector(0,J);
  real x_r[S,0]; // nothing vector to fil out map_rect 
}
parameters {    
  vector[K] delta_1;                  // non-zero discriminations
  vector[K] delta_0;                  // zero discriminations
  vector[T*J] alpha;               // time-varying ideal points for elites
                                    // flatten it for map_rect
  vector[K] beta_0;                // zero difficulty of question k
  vector[K] beta_1;                // zero difficulty of question k
  vector<lower=-1,upper=1>[4] adj_in;  //adjustment parameters
  vector<lower=-1,upper=1>[4] adj_out; //adjustment parameters
  vector[4] alpha_int; //drift
  vector[4] betax; //effects of coup
  real<lower=0> country; //dummy for country-level fixed effects
  vector<lower=0>[4] sigma_time; //heteroskedastic variance by ideological group
  real<lower=0> sigma_beta_0; //hierarchical variance for zero betas
  real<lower=0> sigma_beta_1; //hierarchical variance for zero betas
  //real<lower=0> sigma_overall; //variance for top-level normal distribution
}

transformed parameters {
  // pack all the citizen parameters into an array vector for usage in map_rect
  vector[vP] varparams[S];
  //all elite params are in non-varying vectors
  vector[dP] dparams;
  
  // construct by looping over T and shards
  // where we known that T = number of shards
  
  for(s in 1:S) {
      //discrim then difficulty
      varparams[s] = append_row(delta_1[s],append_row(delta_0[s],
                                           append_row(beta_1[s],
                                                      [beta_0[s]]')));
    
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
  
  
  dparams = append_row(alpha,
            append_row(sigma_time,
            append_row(adj_in,
            append_row(adj_out,
            append_row(betax,
            append_row(alpha_int,country))))));
  
}

model {
  
  matrix[T,J] alpha_mat = to_matrix(alpha,T,J); // need to reconvert alpha for priors
  
  alpha_int ~ normal(0,3);
  adj_in ~ normal(0,2);
  adj_out ~ normal(0,2);
  country ~ exponential(1);
  sigma_time ~ normal(0,1); // constrain the variance to push for better identification
  //sigma_overall ~ exponential(.1);
  sigma_beta_0 ~ exponential(.1);
  sigma_beta_1 ~ exponential(.1);
  betax ~ normal(0,5);
//citizen priors
  //beta_0 ~ normal(0,sigma_beta_0);
  //beta_1 ~ normal(0,sigma_beta_1);
  beta_0 ~ normal(0,3);
  beta_1 ~ normal(0,3);
  delta_1 ~ normal(0,3);   
  delta_0 ~ normal(0,3);
  
    //VAR priors with constraints on who influences who
  
  alpha_mat[2:T,1] ~ normal(alpha_int[1] + adj_in[1]*alpha_mat[1:(T-1),1] +
                        adj_out[1]*alpha_mat[1:(T-1),2] +
                        betax[1]*time_gamma,
                sigma_time[1]);

  alpha_mat[2:T,2] ~ normal(alpha_int[2] + adj_in[2]*alpha_mat[1:(T-1),2] +
                        adj_out[2]*alpha_mat[1:(T-1),1] +
                        betax[2]*time_gamma,
                sigma_time[2]);
  alpha_mat[2:T,3] ~ normal(alpha_int[3] + adj_in[3]*alpha_mat[1:(T-1),3] +
                        adj_out[3]*alpha_mat[1:(T-1),4] +
                        betax[3]*time_gamma,
                sigma_time[3]);

  alpha_mat[2:T,4] ~ normal(alpha_int[4] + adj_in[4]*alpha_mat[1:(T-1),4] +
                        adj_out[4]*alpha_mat[1:(T-1),3] +
                        betax[4]*time_gamma,
                sigma_time[4]);

  
  // parallelize the likelihood with map_rect
  
  target += sum(map_rect(overT, dparams, varparams, time_points, alldata));

}

generated quantities {
  matrix[T,J] alpha_m = to_matrix(alpha,T,J);
  matrix[T,J] alpha_country; //recalculate alpha with country intercepts included  

  alpha_country[,1] = alpha_m[,1];
  alpha_country[,2] = alpha_m[,2] + country;
  alpha_country[,3] = alpha_m[,3];
  alpha_country[,4] = alpha_m[,4] + country;
}
