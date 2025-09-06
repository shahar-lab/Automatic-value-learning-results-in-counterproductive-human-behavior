data {
  
 int<lower=1> Ndata;      // Total number of trials (for all subjects)
  int<lower=1> Nsubjects; //number of subjects

  int<lower=2> Narms; //number of overall alternatives

  int<lower=2> Nraffle; //number of cards offered per trial
  
  array [Ndata] int<lower=1, upper=Nsubjects> subject_trial; // Which subject performed each trial

  //Behavioral data:

  array[Ndata] int<lower=0> ch_card; //index of which card was chosen coded 1 to 4

  array[Ndata] int<lower=0> ch_key; //index of which card was chosen coded 1 (left) to 2 (right)

  array[Ndata] int<lower=0> reward; //outcome (0 unrewarded or 1 rewarded)

  array[Ndata] int<lower=0> card_left; //offered card in left location

  array[Ndata] int<lower=0> card_right; //offered card in right location

  array [Ndata] int <lower=0,upper=1> first_trial_in_session; //binary indicator of whether it is the first trial in the session to reset values
  
  array[Ndata] int<lower=0> selected_offer;
  
  array[Ndata] int<lower=0> session; //number of the current session (1-3)

}

parameters {
    // Group-level (population) parameters, seperately estimated for each session
  //learning rates
  real mu_alpha1; 
  real mu_alpha2; 
  real mu_alpha3; 
  
  //inverse temperature
  real mu_beta1; 
  real mu_beta2; 
  real mu_beta3;
  
  //degree of outcome-irrelevant learning
  real mu_lambda1;
  real mu_lambda2;
  real mu_lambda3;
  
  ////reward-independent tendency to repeat the same card
  real mu_explore_card1;
  real mu_explore_card2;
  real mu_explore_card3;
  
  //reward-independent tendency to repeat the same location
  real mu_explore_key1;
  real mu_explore_key2;
  real mu_explore_key3;
  
  // a decay parameter for explore card
  real mu_decay_explore_card1;
  real mu_decay_explore_card2;
  real mu_decay_explore_card3;
  
  // a decay parameter for explore key
  real mu_decay_explore_key1;
  real mu_decay_explore_key2;
  real mu_decay_explore_key3;
  
  // Group-level standard deviations (for subject-level variability)
  real<lower=0> sigma_alpha1;         
  real<lower=0> sigma_alpha2;         
  real<lower=0> sigma_alpha3;         
  real<lower=0> sigma_beta1;
  real<lower=0> sigma_beta2;
  real<lower=0> sigma_beta3;
  real<lower=0> sigma_lambda1;        
  real<lower=0> sigma_lambda2;
  real<lower=0> sigma_lambda3;
  real<lower=0> sigma_explore_card1;
  real<lower=0> sigma_explore_card2;
  real<lower=0> sigma_explore_card3;
  real<lower=0> sigma_explore_key1;
  real<lower=0> sigma_explore_key2;
  real<lower=0> sigma_explore_key3;
  real<lower=0> sigma_decay_explore_card1;
  real<lower=0> sigma_decay_explore_card2;
  real<lower=0> sigma_decay_explore_card3;
  real<lower=0> sigma_decay_explore_key1;
  real<lower=0> sigma_decay_explore_key2;
  real<lower=0> sigma_decay_explore_key3;
// Individual-level parameters (random effects in standard normal space)
  vector[Nsubjects] alpha1_raw;
  vector[Nsubjects] alpha2_raw;
  vector[Nsubjects] alpha3_raw;
  vector[Nsubjects] beta1_raw;
  vector[Nsubjects] beta2_raw;
  vector[Nsubjects] beta3_raw;
  vector[Nsubjects] lambda1_raw;
  vector[Nsubjects] lambda2_raw;
  vector[Nsubjects] lambda3_raw;
  vector[Nsubjects] explore_card1_raw;
  vector[Nsubjects] explore_card2_raw;
  vector[Nsubjects] explore_card3_raw;
  vector[Nsubjects] explore_key1_raw;
  vector[Nsubjects] explore_key2_raw;
  vector[Nsubjects] explore_key3_raw;
  vector[Nsubjects] decay_explore_card1_raw;
  vector[Nsubjects] decay_explore_card2_raw;
  vector[Nsubjects] decay_explore_card3_raw;
  vector[Nsubjects] decay_explore_key1_raw;
  vector[Nsubjects] decay_explore_key2_raw;
  vector[Nsubjects] decay_explore_key3_raw;

}


transformed parameters {
  //individual-level values
  vector<lower=0,upper=1>[Nsubjects] alpha1_sbj;
  vector<lower=0,upper=1>[Nsubjects] alpha2_sbj;
  vector<lower=0,upper=1>[Nsubjects] alpha3_sbj;
  vector[Nsubjects] beta1_sbj;
  vector[Nsubjects] beta2_sbj;
  vector[Nsubjects] beta3_sbj;
  vector<lower=0,upper=1>[Nsubjects] lambda1_sbj; 
  vector<lower=0,upper=1>[Nsubjects] lambda2_sbj;
  vector<lower=0,upper=1>[Nsubjects] lambda3_sbj;
  vector[Nsubjects] explore_card1_sbj;
  vector[Nsubjects] explore_card2_sbj;
  vector[Nsubjects] explore_card3_sbj;
  vector[Nsubjects] explore_key1_sbj;
  vector[Nsubjects] explore_key2_sbj;
  vector[Nsubjects] explore_key3_sbj;
  vector<lower=0,upper=1>[Nsubjects] decay_explore_card1_sbj;
  vector<lower=0,upper=1>[Nsubjects] decay_explore_card2_sbj;
  vector<lower=0,upper=1>[Nsubjects] decay_explore_card3_sbj;
  vector<lower=0,upper=1>[Nsubjects] decay_explore_key1_sbj;
  vector<lower=0,upper=1>[Nsubjects] decay_explore_key2_sbj;
  vector<lower=0,upper=1>[Nsubjects] decay_explore_key3_sbj;
  
 alpha1_sbj = inv_logit(mu_alpha1 +sigma_alpha1* alpha1_raw);
 alpha2_sbj = inv_logit(mu_alpha2 +sigma_alpha2* alpha2_raw);
 alpha3_sbj = inv_logit(mu_alpha3 +sigma_alpha3* alpha3_raw);
 beta1_sbj = (mu_beta1 +sigma_beta1* beta1_raw);
 beta2_sbj = (mu_beta2 +sigma_beta2* beta2_raw);
 beta3_sbj = (mu_beta3 +sigma_beta3* beta3_raw);
 lambda1_sbj = inv_logit(mu_lambda1 + sigma_lambda1 * lambda1_raw);
 lambda2_sbj = inv_logit(mu_lambda2 + sigma_lambda2 * lambda2_raw);
 lambda3_sbj = inv_logit(mu_lambda3 + sigma_lambda3 * lambda3_raw);
 explore_card1_sbj = (mu_explore_card1 + sigma_explore_card1 * explore_card1_raw);
 explore_card2_sbj = (mu_explore_card2 + sigma_explore_card2 * explore_card2_raw);
 explore_card3_sbj = (mu_explore_card3 + sigma_explore_card3 * explore_card3_raw);
 explore_key1_sbj = (mu_explore_key1 + sigma_explore_key1 * explore_key1_raw);
 explore_key2_sbj = (mu_explore_key2 + sigma_explore_key2 * explore_key2_raw);
 explore_key3_sbj = (mu_explore_key3 + sigma_explore_key3 * explore_key3_raw);
 decay_explore_card1_sbj = inv_logit(mu_decay_explore_card1 + sigma_decay_explore_card1 * decay_explore_card1_raw);
 decay_explore_card2_sbj = inv_logit(mu_decay_explore_card2 + sigma_decay_explore_card2 * decay_explore_card2_raw);
 decay_explore_card3_sbj = inv_logit(mu_decay_explore_card3 + sigma_decay_explore_card3 * decay_explore_card3_raw);
 decay_explore_key1_sbj = inv_logit(mu_decay_explore_key1 + sigma_decay_explore_key1 * decay_explore_key1_raw);
 decay_explore_key2_sbj = inv_logit(mu_decay_explore_key2 + sigma_decay_explore_key2 * decay_explore_key2_raw);
 decay_explore_key3_sbj = inv_logit(mu_decay_explore_key3 + sigma_decay_explore_key3 * decay_explore_key3_raw);

  real alpha_t; //trial-by-trial parameter based on the current subject
	real beta_t;					 
  real lambda_t;
  real explore_card_t;
  real explore_key_t;
  real decay_explore_card_t;
  real decay_explore_key_t;

  //feature-specific prediction errors
  real PE_card;
  real  PE_key;
  
  //the difference in net value between the two offered options (right - left)
  vector [Ndata] Qnet_diff;
  
  vector [Narms] Q_cards; //vector of values for the 4 cards
  vector [Nraffle] Q_cards_offered; //vector of values for the 2 currently offered cards
  vector [Nraffle] Q_keys; // vector of values for left and for right
  
    vector [Narms] E_cards; //vector of accumulated exploration/perseveration card tendencies
  vector [Nraffle] E_cards_offered;
  vector [Nraffle] E_keys; //vector of accumulated exploration/perseveration key tendencies
  
  vector [Nraffle] Qnet;
 
    for (t in 1:Ndata) {
    //loop on all trials in dataset, using the parameter of the current individual
    // in the current session
		
		
		if(session[t]==1){
		alpha_t = alpha1_sbj[subject_trial[t]];
		beta_t = beta1_sbj[subject_trial[t]];
		lambda_t=lambda1_sbj[subject_trial[t]];
		explore_card_t=explore_card1_sbj[subject_trial[t]];
		explore_key_t=explore_key1_sbj[subject_trial[t]];
		decay_explore_card_t=decay_explore_card1_sbj[subject_trial[t]];
		decay_explore_key_t=decay_explore_key1_sbj[subject_trial[t]];
		
		}
		else if (session[t]==2){
		alpha_t = alpha2_sbj[subject_trial[t]];
		beta_t = beta2_sbj[subject_trial[t]];
		lambda_t=lambda2_sbj[subject_trial[t]]; 
		explore_card_t=explore_card2_sbj[subject_trial[t]];
		explore_key_t=explore_key2_sbj[subject_trial[t]];
		decay_explore_card_t=decay_explore_card2_sbj[subject_trial[t]];
		decay_explore_key_t=decay_explore_key2_sbj[subject_trial[t]];
		}
		
		else {
		alpha_t = alpha3_sbj[subject_trial[t]];
		beta_t = beta3_sbj[subject_trial[t]];
		lambda_t=lambda3_sbj[subject_trial[t]];
		explore_card_t=explore_card3_sbj[subject_trial[t]];
		explore_key_t=explore_key3_sbj[subject_trial[t]];
		decay_explore_card_t=decay_explore_card3_sbj[subject_trial[t]];
		decay_explore_key_t=decay_explore_key3_sbj[subject_trial[t]];
		}
		
  if (first_trial_in_session[t] == 1) {
    //reset every beginning of session
      Q_cards=rep_vector(0.5, Narms);
      Q_keys=rep_vector(0.5, Nraffle);
      
      E_cards=rep_vector(0, Narms);
      E_keys=rep_vector(0, Nraffle);
    }
          Q_cards_offered[1]=Q_cards[card_left[t]];
          Q_cards_offered[2]=Q_cards[card_right[t]];
          
          E_cards_offered[1]=E_cards[card_left[t]];
          E_cards_offered[2]=E_cards[card_right[t]];
          
          //integrating all values to a net value
          Qnet=beta_t*(lambda_t*Q_cards_offered+(1-lambda_t)*Q_keys)+E_cards_offered+E_keys; //We compound the value of the card appearing on the left and the value of the left key.

        Qnet_diff[t]  = Qnet[2]-Qnet[1]; //higher values of Qdiff mean higher chance to choose right option.
        
 //calculating PEs
 PE_card =reward[t] - Q_cards[ch_card[t]];
 PE_key =reward[t] - Q_keys[ch_key[t]];
 
//Update values
 Q_cards[ch_card[t]] += alpha_t * PE_card; //update card_value according to reward
 Q_keys[ch_key[t]]   += alpha_t * PE_key; //update key value according to reward
 

  E_cards *= decay_explore_card_t;

  E_keys *= decay_explore_key_t;
 
 E_cards[ch_card[t]]+=explore_card_t;
 E_keys[ch_key[t]]+=explore_key_t;
 

}
}

model {
  
  // Priors for group-level parameters
  mu_alpha1 ~ normal(0, 3);
  mu_alpha2 ~ normal(0, 3);
  mu_alpha3 ~ normal(0, 3);
  mu_beta1 ~ normal(0, 3);
  mu_beta2 ~ normal(0, 3);
  mu_beta3 ~ normal(0, 3);
  mu_lambda1 ~ normal(0, 3);
  mu_lambda2 ~ normal(0, 3);
  mu_lambda3 ~ normal(0, 3);
  mu_explore_card1 ~ normal(0, 3);
  mu_explore_card2 ~ normal(0, 3);
  mu_explore_card3 ~ normal(0, 3);
  mu_explore_key1 ~ normal(0, 3);
  mu_explore_key2 ~ normal(0, 3);
  mu_explore_key3 ~ normal(0, 3);
  mu_decay_explore_card1 ~ normal(0, 3);
  mu_decay_explore_card2 ~ normal(0, 3);
  mu_decay_explore_card3 ~ normal(0, 3);
  mu_decay_explore_key1 ~ normal(0, 3);
  mu_decay_explore_key2 ~ normal(0, 3);
  mu_decay_explore_key3 ~ normal(0, 3);
  
  // Priors for group-level standard deviations
  sigma_alpha1 ~ normal(0, 2);
  sigma_alpha2 ~ normal(0, 2);
  sigma_alpha3 ~ normal(0, 2);
  sigma_beta1 ~ normal(0, 2);
  sigma_beta2 ~ normal(0, 2);
  sigma_beta3 ~ normal(0, 2);
  sigma_lambda1 ~ normal(0, 2);
  sigma_lambda2 ~ normal(0, 2);
  sigma_lambda3 ~ normal(0, 2);
  sigma_explore_card1 ~ normal(0, 2);
  sigma_explore_card2 ~ normal(0, 2);
  sigma_explore_card3 ~ normal(0, 2);
  sigma_explore_key1 ~ normal(0, 2);
  sigma_explore_key2 ~ normal(0, 2);
  sigma_explore_key3 ~ normal(0, 2);
  sigma_decay_explore_card1 ~ normal(0, 2);
  sigma_decay_explore_card2 ~ normal(0, 2);
  sigma_decay_explore_card3 ~ normal(0, 2);
  sigma_decay_explore_key1 ~ normal(0, 2);
  sigma_decay_explore_key2 ~ normal(0, 2);
  sigma_decay_explore_key3 ~ normal(0, 2);
  
  // Priors for subject-specific effect
  alpha1_raw~normal(0,1);
  alpha2_raw~normal(0,1);
  alpha3_raw~normal(0,1);
  beta1_raw~normal(0,1);
  beta2_raw~normal(0,1);
  beta3_raw~normal(0,1);
  lambda1_raw~normal(0,1);
  lambda2_raw~normal(0,1);
  lambda3_raw~normal(0,1);
  explore_card1_raw~normal(0,1);
  explore_card2_raw~normal(0,1);
  explore_card3_raw~normal(0,1);
  explore_key1_raw~normal(0,1);
  explore_key2_raw~normal(0,1);
  explore_key3_raw~normal(0,1);
  decay_explore_card1_raw~normal(0,1);
  decay_explore_card2_raw~normal(0,1);
  decay_explore_card3_raw~normal(0,1);
  decay_explore_key1_raw~normal(0,1);
  decay_explore_key2_raw~normal(0,1);
  decay_explore_key3_raw~normal(0,1);
  
  for (n in 1:Ndata) {
    if(first_trial_in_session[n]!=1){
    //update parameters based on all but the first trial in each session, where all values are reset
    target+= bernoulli_logit_lpmf(selected_offer[n] |  Qnet_diff[n]);
  }
  }
  
  
}

generated quantities {
  vector[Ndata] log_lik;
  for (n in 1:Ndata) {
    if(first_trial_in_session[n]!=1){
    //update parameters based on all but the first trial in each session, where all values are reset
    log_lik[n] = bernoulli_logit_lpmf(selected_offer[n] | Qnet_diff[n]);
  }
  }
}
