# Create Account and Opp Id dictionary
acct_id_dictionary <- my_acct %>% 
  select(account_id, account_name) %>% 
  unique()

opp_id_dictionary <- my_opps %>% 
  select(opp_id, opp_name) %>% 
  unique()

acct_opp_dictionary <- tibble(id = c(my_acct$account_id, my_opps$opp_id),
                              name = c(my_acct$account_name, my_opps$opp_name))