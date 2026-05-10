data {
  
 int<lower=1> Ndata;      // Total number of trials (for all subjects)
  int<lower=1> Nsubjects; //number of subjects

  int<lower=2> Narms; //number of overall alternatives

  int<lower=2> Nraffle; //number of cards offered per trial
  
  array [Ndata] int<lower=1, upper=Nsubjects> subject_trial; // Which subject performed each trial

  //Behavioral data:

  array[Ndata] int<lower=0> ch_card; //index of which card was chosen coded 1 to 4

  array[Ndata] int<lower=0> ch_key; //index of which card was chosen coded 1(left) to 2(right)

  array[Ndata] int<lower=0> reward; //outcome (0 - unrewarded or 1 - rewarded)

  array[Ndata] int<lower=0> card_left; //offered card in left location (1-4)

  array[Ndata] int<lower=0> card_right; //offered card in right location (1-4)

  array [Ndata] int <lower=0,upper=1> first_trial_in_block; // binary indicator to reset values

  array[Ndata] int<lower=0> selected_offer; // 0 for left location is chosen or 1 for right location is chosen

}

parameters {
    // Group-level (population) parameters
  real mu_alpha;        //  learning rate
  real mu_beta;          // inverse temperature
  real mu_lambda;        // degree of following instructions (lower means more outcome-irrelevant)
  real mu_explore_card; //reward-independent tendency to repeat the same card
  real mu_explore_key; //reward-independent tendency to repeat the same location
  real mu_decay_explore_card; // a decay parameter for explore card
  real mu_decay_explore_key; // a decay parameter for explore key
 
  // Group-level standard deviations (for subject-level variability)
  real<lower=0> sigma_alpha;          
  real<lower=0> sigma_beta;          
  real<lower=0> sigma_lambda;   
  real<lower=0> sigma_explore_card;
  real<lower=0> sigma_explore_key;
  real<lower=0> sigma_decay_explore_card;
  real<lower=0> sigma_decay_explore_key;
  
// individual-level parameters (random effects in standard normal space)
  vector[Nsubjects] alpha_raw;
  vector[Nsubjects] beta_raw;
  vector[Nsubjects] lambda_raw;
  vector[Nsubjects] explore_card_raw;
  vector[Nsubjects] explore_key_raw;
  vector[Nsubjects] decay_explore_card_raw;
  vector[Nsubjects] decay_explore_key_raw;

}


transformed parameters {
  //individual-level values
  vector<lower=0,upper=1>[Nsubjects] alpha_sbj;
  vector[Nsubjects] beta_sbj; 
  vector<lower=0,upper=1>[Nsubjects] lambda_sbj; 
  vector[Nsubjects] explore_card_sbj;
  vector[Nsubjects] explore_key_sbj;
  vector<lower=0,upper=1>[Nsubjects] decay_explore_card_sbj;
  vector<lower=0,upper=1>[Nsubjects] decay_explore_key_sbj;
  
 alpha_sbj = inv_logit(mu_alpha +sigma_alpha* alpha_raw); //transformed to 0-1
 beta_sbj = (mu_beta +sigma_beta* beta_raw);
 lambda_sbj = inv_logit(mu_lambda + sigma_lambda * lambda_raw);//transformed to 0-1
 explore_card_sbj = (mu_explore_card + sigma_explore_card * explore_card_raw);
 explore_key_sbj = (mu_explore_key + sigma_explore_key * explore_key_raw);
 decay_explore_card_sbj = inv_logit(mu_decay_explore_card + sigma_decay_explore_card * decay_explore_card_raw); //transformed to 0-1
 decay_explore_key_sbj = inv_logit(mu_decay_explore_key + sigma_decay_explore_key * decay_explore_key_raw);//transformed to 0-1

  real alpha_t; //trial-by-trial parameter based on the current subject
	real beta_t;					 
  real lambda_t;
  real explore_card_t;
  real explore_key_t;
  real decay_explore_card_t;
  real decay_explore_key_t;

  //feature-specific prediction errors
  real  PE_card;
  real  PE_key;
  
  //the difference in net value between the two offered options (right - left)
  vector [Ndata] Qnet_diff;
  vector [Ndata] Qcard_diff;
  vector [Ndata] Qkey_diff;
  
  vector [Narms] Q_cards; //vector of values for the 4 cards
  vector [Nraffle] Q_cards_offered; //vector of values for the 2 currently offered cards
  vector [Nraffle] Q_keys; // vector of values for left and for right
  
  vector [Narms] E_cards; //vector of accumulated exploration/perseveration card tendencies
  vector [Nraffle] E_cards_offered;
  vector [Nraffle] E_keys; //vector of accumulated exploration/perseveration key tendencies
  
  vector [Nraffle] Qnet;
 
  for (t in 1:Ndata) {
    //loop on all trials in dataset, using the parameter of the current individual
    alpha_t = alpha_sbj[subject_trial[t]];
		beta_t = beta_sbj[subject_trial[t]];
		lambda_t=lambda_sbj[subject_trial[t]];
		explore_card_t=explore_card_sbj[subject_trial[t]];
		explore_key_t=explore_key_sbj[subject_trial[t]];
		decay_explore_card_t=decay_explore_card_sbj[subject_trial[t]];
		decay_explore_key_t=decay_explore_key_sbj[subject_trial[t]];

  if (first_trial_in_block[t] == 1) {
    //reset all values in a new block
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
 Qnet=beta_t*(lambda_t*Q_cards_offered+(1-lambda_t)*Q_keys)+E_cards_offered+E_keys;
 Qnet_diff[t]  = Qnet[2]-Qnet[1]; //higher values of Qdiff correspond to higher chance to choose right option.

 Qcard_diff[t]=Q_cards[card_right[t]]-Q_cards[card_left[t]];
 Qkey_diff[t]=Q_keys[2]-Q_keys[1];

 //calculating PEs
 PE_card =reward[t] - Q_cards[ch_card[t]];
 PE_key =reward[t] - Q_keys[ch_key[t]];
 
 Q_cards[ch_card[t]] += alpha_t * PE_card; //update card_value according to reward
 Q_keys[ch_key[t]]   += alpha_t * PE_key; //update key value according to reward
 
  E_cards *= decay_explore_card_t; //decay to 0, lower decay values mean faster decay

  E_keys *= decay_explore_key_t;
 
 E_cards[ch_card[t]]+=explore_card_t;
 E_keys[ch_key[t]]+=explore_key_t;
 

}
}

model {
  
  // Priors for group-level parameters
  mu_alpha ~ normal(0, 3);
  mu_beta ~ normal(0, 3);
  mu_lambda ~ normal(0, 3);
  mu_explore_card ~ normal(0, 3);
  mu_explore_key ~ normal(0, 3);
  mu_decay_explore_card ~ normal(0, 3);
  mu_decay_explore_key ~ normal(0, 3);
  
  // Priors for group-level standard deviations
  sigma_alpha ~ normal(0, 2);
  sigma_beta ~ normal(0, 2);
  sigma_lambda ~ normal(0, 2);
  sigma_explore_card ~ normal(0, 2);
  sigma_explore_key ~ normal(0, 2);
  sigma_decay_explore_card ~ normal(0, 2);
  sigma_decay_explore_key ~ normal(0, 2);
  
  // Priors for subject-specific effect
  alpha_raw~normal(0,1);
  beta_raw~normal(0,1);
  lambda_raw~normal(0,1);
  explore_card_raw~normal(0,1);
  explore_key_raw~normal(0,1);
  decay_explore_card_raw~normal(0,1);
  decay_explore_key_raw~normal(0,1);
  
  for (n in 1:Ndata) {
    if(first_trial_in_block[n]!=1){
      //update parameters based on all but the first trial in each block, where all values are reset
    target+= bernoulli_logit_lpmf(selected_offer[n] |  Qnet_diff[n]);
  }
  }
  
  
}

generated quantities {
  vector[Ndata] log_lik;
  for (n in 1:Ndata) {
    if(first_trial_in_block[n]!=1){
    //calculate log likelihood based on all but the first trial in each block, where all values are reset
    log_lik[n] = bernoulli_logit_lpmf(selected_offer[n] | Qnet_diff[n]);
  }
  }
}
