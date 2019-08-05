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
    
    real delta_11 = cit[1];
    real delta_10 = cit[2];
    real delta_21 = cit[3];
    real delta_20 = cit[4];
    real beta_1 = cit[5];
    real beta_0 = cit[6];
    
    
    
    
    // get alphas for this time point
    // matrix[T,J] alpha1 = to_matrix(allparams[1:T*J],T,J);
    // matrix[T,J] alpha2 = to_matrix(allparams[(T*J+1):(2*T*J)],T,J);
     
    
    //unpack allparams
    // vector[J] sigma_time1 = allparams[(2*T*J+1):(2*T*J+J)];
    // vector[J] sigma_time2 = allparams[(2*T*J+J+1):(2*T*J+2*J)];
    // vector[J] adj_in1 = allparams[(2*T*J+2*J+1):(2*T*J+3*J)];
    // vector[J] adj_in2 = allparams[(2*T*J+3*J+1):(2*T*J+4*J)];
    // vector[J] adj_out1 = allparams[(2*T*J+4*J+1):(2*T*J+5*J)];
    // vector[J] adj_out2 = allparams[(2*T*J+5*J+1):(2*T*J+6*J)];
    // vector[J] betax1 = allparams[(2*T*J+6*J+1):(2*T*J+7*J)];
    // vector[J] betax2 = allparams[(2*T*J+7*J+1):(2*T*J+8*J)];
    // vector[J] alpha_int1 = allparams[(2*T*J+8*J+1):(2*T*J+9*J)];
    // vector[J] alpha_int2 = allparams[(2*T*J+9*J+1):(2*T*J+10*J)];
    // real country = allparams[num_elements(allparams)];
    
    vector[N] lp_out;
                
  // loop over outcome
  // use hurdle model for missing data (-9999)
  // conditional on passing hurdle, use Poisson distribution for retweet counts
  
    for(n in 1:N) {
      
      real this_alpha1 = allparams[(T*(jj[n]-1))+tt[n]];
      real this_alpha2 = allparams[(T*J + T*(jj[n]-1))+tt[n]];
      // real this_alpha1 = alpha1[tt[n],jj[n]];
      // real this_alpha2 = alpha2[tt[n],jj[n]];
      
      if(gen_out[n]==99999) {
        real lp =  bernoulli_logit_lpmf(1 | delta_10*this_alpha1  +
                                        delta_20*this_alpha2 - beta_0);
        lp_out[n] = lp;
      } else {
        real lp = bernoulli_logit_lpmf(0 | delta_10*this_alpha1  +
                                        delta_20*this_alpha2 - beta_0);
        real ll = poisson_log_lpmf(gen_out[n]|delta_11*this_alpha1 +
                                  delta_21*this_alpha2 -
                                  beta_1);
        lp_out[n] = lp + ll;
      }
    }
   return [sum(lp_out) + normal_lpdf(cit|0,3)]'; // return joint probability of the shard
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
  int vP = 6; // need all citizen parameters discrimination + difficulty for one citizen = 4 parameters
  int dP = 2*T*J; // all alpha parameters plus adjustment/betax/sigmas/country
  //need a vector to pad the time-varying parameters for the case when T=1
  vector[J] padT = rep_vector(0,J);
  real x_r[S,0]; // nothing vector to fil out map_rect 
}
parameters {    
  // vector[K] delta_11;                  // non-zero discriminations
  // vector[K] delta_10;                  // zero discriminations
  // vector[K] delta_21;                  // non-zero discriminations
  // vector[K] delta_20;                  // zero discriminations
  vector[vP] varparams[S];
  vector[dP] dparams;
  // vector[T*J] alpha1;               // dimension 1: islamism vs. secularism
  // vector[T*J] alpha2;               // dimension 2: democracy vs. authoritarianism
  // vector[K] beta_0;                // zero difficulty of question k
  // vector[K] beta_1;                // zero difficulty of question k
  vector<lower=-.9,upper=.9>[4] adj_in1;  //adjustment parameters
  vector<lower=-.9,upper=.9>[4] adj_out1; //adjustment parameters
  vector<lower=-.9,upper=.9>[4] adj_in2;  //adjustment parameters
  vector<lower=-.9,upper=.9>[4] adj_out2; //adjustment parameters
  vector[4] alpha_int1; //drift
  vector[4] alpha_int2; //drift
  vector[4] betax1; //effects of coup
  vector[4] betax2; //effects of coup
  //real<lower=0> country; //dummy for country-level fixed effects
  vector<lower=0>[4] sigma_time1; //heteroskedastic variance by ideological group
  vector<lower=0>[4] sigma_time2; //heteroskedastic variance by ideological group
  //real<lower=0> sigma_beta_0; //hierarchical variance for zero betas
  //real<lower=0> sigma_beta_1; //hierarchical variance for zero betas
  //real<lower=0> sigma_overall; //variance for top-level normal distribution
}

transformed parameters {
  // pack all the citizen parameters into an array vector for usage in map_rect
  
  //all elite params are in non-varying vectors
  
  // construct by looping over T and shards
  // where we known that T = number of shards
  
  // for(s in 1:S) {
  //     //discrim then difficulty
  //     varparams[s] = append_row(delta_11[s],append_row(delta_10[s],
  //                                           append_row(delta_21[s],
  //                                           append_row(delta_20[s],
  //                                          append_row(beta_1[s],
  //                                                     [beta_0[s]]')))));
  //   
  // }
  
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
  
  // 
  // dparams = append_row(alpha1,alpha2);
  
}

model {
  
  matrix[T,J] alpha_mat1 = to_matrix(dparams[1:T*J],T,J); // need to reconvert alpha for priors
  matrix[T,J] alpha_mat2 = to_matrix(dparams[(T*J+1):(2*T*J)],T,J);
  
  //pin the intercepts for D2
  
  alpha_int2[1] ~ normal(-1,.01);
  alpha_int2[2] ~ normal(1,.01);
  alpha_int2[3:4] ~ normal(0,3);
  
  alpha_int1[1] ~ normal(-1,.01);
  alpha_int1[2] ~ normal(1,.01);
  alpha_int1[3:4] ~ normal(0,3);
  adj_out1 ~ normal(0,3);
  adj_in2 ~ normal(0,3);
  adj_out2 ~ normal(0,3);
  adj_in1 ~ normal(0,3);

  sigma_time1 ~ exponential(4); // constrain the variance to push for better identification
  sigma_time2 ~ exponential(4); // constrain the variance to push for better identification
  //sigma_overall ~ exponential(.1);
  betax1 ~ normal(0,5);
  betax2 ~ normal(0,5);
//citizen priors
  //beta_0 ~ normal(0,sigma_beta_0);
  //beta_1 ~ normal(0,sigma_beta_1);
  
  to_vector(alpha_mat1[1,1:J]) ~ normal(0,3);
  to_vector(alpha_mat2[1,1:J]) ~ normal(0,3);
  
    //VAR priors with constraints on who influences who
  
  alpha_mat1[2:T,1] ~ normal(alpha_int1[1] + adj_in1[1]*alpha_mat1[1:(T-1),1] +
                        adj_out1[1]*alpha_mat1[1:(T-1),2] +
                        betax1[1]*time_gamma,
                sigma_time1[1]);

  alpha_mat1[2:T,2] ~ normal(alpha_int1[2] + adj_in1[2]*alpha_mat1[1:(T-1),2] +
                        adj_out1[2]*alpha_mat1[1:(T-1),1] +
                        betax1[2]*time_gamma,
                sigma_time1[2]);
  alpha_mat1[2:T,3] ~ normal(alpha_int1[3] + adj_in1[3]*alpha_mat1[1:(T-1),3] +
                        adj_out1[3]*alpha_mat1[1:(T-1),4] +
                        betax1[3]*time_gamma,
                sigma_time1[3]);

  alpha_mat1[2:T,4] ~ normal(alpha_int1[4] + adj_in1[4]*alpha_mat1[1:(T-1),4] +
                        adj_out1[4]*alpha_mat1[1:(T-1),3] +
                        betax1[4]*time_gamma,
                sigma_time1[4]);
                
  alpha_mat2[2:T,1] ~ normal(alpha_int2[1] + adj_in2[1]*alpha_mat2[1:(T-1),1] +
                        adj_out2[1]*alpha_mat2[1:(T-1),2] +
                        betax2[1]*time_gamma,
                sigma_time2[1]);

  alpha_mat2[2:T,2] ~ normal(alpha_int2[2] + adj_in2[2]*alpha_mat2[1:(T-1),2] +
                        adj_out2[2]*alpha_mat2[1:(T-1),1] +
                        betax2[2]*time_gamma,
                sigma_time2[2]);
  alpha_mat2[2:T,3] ~ normal(alpha_int2[3] + adj_in2[3]*alpha_mat2[1:(T-1),3] +
                        adj_out2[3]*alpha_mat2[1:(T-1),4] +
                        betax2[3]*time_gamma,
                sigma_time2[3]);

  alpha_mat2[2:T,4] ~ normal(alpha_int2[4] + adj_in2[4]*alpha_mat2[1:(T-1),4] +
                        adj_out2[4]*alpha_mat2[1:(T-1),3] +
                        betax2[4]*time_gamma,
                sigma_time2[4]);


  
  // parallelize the likelihood with map_rect
  
  target += sum(map_rect(overT, dparams, varparams, time_points, alldata));

}
