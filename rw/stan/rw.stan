// from https://github.com/CCS-Lab/hBayesDM/blob/develop/commons/stan_files/igt_pvl_delta.stan
// modified to include learning noise


data {
  int<lower=1> T;
  int choice[T];
  real gain[T];
}
transformed data {
  vector[2] initV;
  
  initV  = rep_vector(1.0, 2); //initialize expected value to 0
}

parameters {
  //Rraw parameters (for Matt trick)
  real A_pr;
  real cons_pr;
}

transformed parameters {
  // Transform subject-level raw parameters
  real<lower=0, upper=2>  alpha;
  real<lower=0, upper=5>  cons;

  A      = Phi_approx(A_pr);
  cons   = Phi_approx(cons_pr) * 5;
}

model {
  // Define values
  vector[2] ev;      // expected value
  vector[2] p;       // perseveration bias
  real curUtil;     // utility of curFb
  real theta;       // theta = 3^c - 1
  
// individual parameters
  A_pr      ~ normal(0, 1);
  cons_pr   ~ normal(0, 1);

  // Initialize values
  theta = pow(3, cons) -1;
  ev = initV; // initial ev values
  
  for (t in 1:T) {
    // softmax choice
    choice[t] ~ categorical_logit(theta * ev);

    curUtil = gain[t];
    
    // update EV
    for (k in 1:2){
      ev[k] += A * (curUtil - ev[k]);
    }
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
      real curUtil;     // utility of curFb
      real theta;       // theta = 3^c - 1

      // Initialize values
      log_lik = 0;
      theta      = pow(3, cons) -1;
      ev         = initV; // initial ev values

      for (t in 1:T) {
        // softmax choice
        log_lik += categorical_logit_lpmf(choice[t] | theta * ev);

        // generate posterior prediction for current trial
        y_pred[t] = categorical_rng(softmax(theta * ev));

        curUtil = gain[t];

	// choice
	for (k in 1:2){
	    ev[k] += A * (curUtil - ev[k]);
	}	  
      }
  }
}

