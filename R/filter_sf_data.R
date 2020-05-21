# Filter Accounts, Opps, Tasks

# Filter for my accounts ------------------------------------
my_acct <- acct %>% 
  filter(is_deleted == 0) %>% 
  filter(owner_id == ownerID) %>% 
  select(id, name, type, billing_street, billing_city, billing_state, billing_country,
         shipping_city, shipping_state, shipping_country, industry, last_activity_date) %>% 
  rename(account_id = id,
         account_name = name,
         account_type = type,
         account_last_activity = last_activity_date)


# Filter for my opps ----------------------------------------
my_opps <- opps %>% 
  filter(is_deleted == 0) %>% 
  filter(owner_id == ownerID) %>% 
  filter(type == "Renewal") %>% 
  select(id, account_id, name, amount, stage_name, probability, created_date, close_date, 
         type, forecast_category, forecast_category_name, lead_source, description, 
         last_activity_date, fiscal_quarter, fiscal_year, fiscal) %>% 
  rename(opp_id = id,
         opp_name = name,
         opp_amount = amount,
         opp_stage = stage_name,
         opp_created_date = created_date,
         opp_close_date = close_date,
         opp_description = description,
         opp_last_activity = last_activity_date,
         opp_fiscal_year = fiscal_year,
         opp_fiscal_quarter = fiscal_quarter)


# Add account names to opps ------------------------------------
my_opps_acct <- my_opps %>% 
  left_join(my_acct, by = "account_id") %>% 
  select(opp_id, account_id, account_name, opp_name, opp_amount, opp_stage,
         probability, opp_created_date, opp_close_date, everything())


# Filter for my tasks -----------------------------------------
my_tasks <- tasks %>% 
  filter(owner_id == ownerID,
         status != "Completed") %>% 
  select(id, account_id, what_id, subject, description, activity_date, type) %>% 
  rename(task_id = id,
         task_subject = subject,
         task_description = description,
         task_date = activity_date,
         task_type = type) %>% 
  collect()