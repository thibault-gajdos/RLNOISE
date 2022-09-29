// from https://github.com/CCS-Lab/hBayesDM/blob/develop/commons/stan_files/igt_pvl_delta.stan
// modified to include learning noise


data {
  int<lower=1> T;
  int choice[T];
  real gain[T];
}
transformed data {
  vector[2] initV;
  //real noise[T];
  
  initV  = rep_vector(1.0, 2); //initialize expected value to 0

  //generate noise 
    //for (t in 1:T){
    //  noise[t] =  normal_rng(0,.1);
    //}
}

parameters {
  //Rraw parameters (for Matt trick)
  real A_pr;
  real cons_pr;
  real zeta_pr;
}

transformed parameters {
  // Transform subject-level raw parameters
  real<lower=0, upper=1>  A;
  real<lower=0, upper=5>  cons;
  real<lower=0, upper=10>  zeta;

  A      = Phi_approx(A_pr);
  cons   = Phi_approx(cons_pr) * 5;
  zeta   = Phi_approx(zeta_pr) * 10;
}

model {
  // Define values
  vector[2] ev;      // expected value
  vector[2] ev_noise;      // noisy expected value
  real curUtil;     // utility of curFb
  real theta;       // theta = 3^c - 1
  
// individual parameters
  A_pr      ~ normal(0, 1);
  cons_pr   ~ normal(0, 1);
  zeta_pr   ~ normal(0, 1);

    
  // Initialize values
  theta = pow(3, cons) -1;
  ev = initV; // initial ev values
  ev_noise = initV;
  
  for (t in 1:T) {
    // softmax choice
    choice[t] ~ categorical_logit(theta * ev);
    
    // update EV
    curUtil = gain[t];    
    ev_noise[1] ~ normal(ev[1] + A*(curUtil-ev[1]),zeta*fabs(ev[1] - curUtil));
    ev_noise[2] ~ normal(ev[2] + A*(curUtil-ev[1]),zeta*fabs(ev[1] - curUtil));
    if (choice[t] == 1)
      ev[1] = ev_noise[1];
    if (choice[t] == 2)
      ev[2] = ev_noise[2];
  }
}


generated quantities {
  // For log likelihood calculation
  real log_lik;

  // For posterior predictive check
  real y_pred[T];

  // Set all posterior predictions to -1 (avoids NULL values)
  for (t in 1:T) {
    y_pred[t] = -1;
  }


  { // local section, this saves time and space
    // Define values
      vector[2] ev;
      vector[2] ev_noise;      // noisy expected value
      real curUtil;     // utility of curFb
      real theta;       // theta = 3^c - 1

      // Initialize values
      log_lik = 0;
      theta      = pow(3, cons) -1;
      ev         = initV; // initial ev values
      ev_noise = initV;

      for (t in 1:T) {
        // softmax choice
        log_lik += categorical_logit_lpmf(choice[t] | theta * ev);

        // generate posterior prediction for current trial
        y_pred[t] = categorical_rng(softmax(theta * ev));

        curUtil = gain[t];

	// choice
	ev_noise[1] = normal_rng(ev[1] + A*(curUtil-ev[1]),zeta*fabs(ev[1] - curUtil));
	ev_noise[2] = normal_rng(ev[2] + A*(curUtil-ev[1]),zeta*fabs(ev[1] - curUtil));
	if (choice[t] == 1)
	  ev[1] = ev_noise[1];
	if (choice[t] == 2)
	  ev[2] = ev_noise[2];
      }
  }
}


