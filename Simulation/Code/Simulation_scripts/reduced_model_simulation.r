#### simulate Rescorla-Wagner block for participant ----
sim.block = function(subject,parameters,cfg){ 
  print(paste('subject',subject))
  
  #pre-allocation
  
  #set parameters
  alpha = plogis(parameters['alpha'])
  beta  = parameters['beta']
  explore_card = parameters['explore_card']
  explore_key = parameters['explore_key']
  decay_explore_card = plogis(parameters['decay_explore_card'])
  decay_explore_key = plogis(parameters['decay_explore_key'])

  
  #set initial var
  Narms              = cfg$Narms
  Ntrials=cfg$Ntrials
  Nraffle            = cfg$Nraffle
  Nblocks            = cfg$Nblocks
  Ndims              = cfg$Ndims
  expvalues          = cfg$rndwlk
  #rownames(expvalues)=c('ev1','ev2','ev3','ev4')
  df                 =data.frame()

  for (block in 1:Nblocks){

    Q_cards= rep(0.5, Narms)
    E_cards= rep(0, Narms)
    E_keys = rep(0, Nraffle)
    
    for (trial in 1:Ntrials){
      #computer offer
      options=sample(1:Narms,2)
      
      #value of offered cards
      Q_cards_offered = Q_cards[options] #use their Q values
      E_cards_offered = E_cards[options]
      Qnet = beta*(Q_cards_offered)+E_cards_offered+E_keys
      
      p= exp(Qnet) / sum(exp(Qnet)) #get prob for each action
      #players choice
      ch_card = sample(options, 1, prob = p) #chose a card according to probs
      unch_card=options[which(options != ch_card)]
      ch_key = which(options == ch_card) #get key of chosen card 1 =left
      unch_key = which(options!=ch_card)
      #outcome 
      reward = sample(0:1, 1, prob = c(1 - expvalues[ch_card, trial], expvalues[ch_card, trial])) #reward according to card
      
      #calc PE
      PE_cards= reward-Q_cards[ch_card]
      
      #save trial's data
      
      #create data for current trials
      dfnew=data.frame(
        subject,
        block,
        trial,
        first_trial_in_block=if_else(trial==1,1,0),
        first_trial=if_else(trial==1&block==1,1,0),
        card_right = options[2],
        card_left = options[1],
        ch_card,
        ch_key,
        selected_offer=ch_key-1,
        reward,
        Q_ch_card = Q_cards[ch_card], #to get previous trial
        Q_unch_card = Q_cards[options[which(options != ch_card)]],
        Q_right_card = Q_cards[options[2]],
        Q_left_card = Q_cards[options[1]],
        exp_val_right=expvalues[options[2], trial],
        exp_val_left=expvalues[options[1], trial],
        exp_val_ch = expvalues[ch_card, trial],
        exp_val_unch = expvalues[options[which(options != ch_card)], trial],
        E_ch_card = E_cards[ch_card],
        E_unch_card = E_cards[options[which(options != ch_card)]],
        E_ch_key = E_keys[ch_key],
        E_unch_key = E_keys[which(options != ch_card)],
        PE_cards,
        alpha,
        beta,
        explore_card,
        explore_key,
        decay_explore_card,
        decay_explore_key
      )
      df=rbind(df,dfnew)
      #updating Qvalues
      
      Q_cards[ch_card] = Q_cards[ch_card]  + alpha * PE_cards
      #Q_cards[unch_card]=Q_cards[unch_card]+alpha *(1-reward-Q_cards[unch_card])

      
      #decay
      E_cards = E_cards*decay_explore_card
      E_keys = E_keys*decay_explore_key
      #updating E values
      E_cards[ch_card] = E_cards[ch_card] + explore_card
      E_keys[ch_key] = E_keys[ch_key] + explore_key
    }
  }     
  
  return (df)
}