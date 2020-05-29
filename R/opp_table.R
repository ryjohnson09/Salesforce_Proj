# Opp table

# Create pre-url hyperlinks
acct_url_pre <- '<a href="https://na39.lightning.force.com/lightning/r/Account/'
opp_url_pre  <- '<a href="https://na39.lightning.force.com/lightning/r/Opportunity/'
task_url_pre <- '<a href="https://na39.lightning.force.com/lightning/r/Task/'

# Opp Table days/color breaks
days_breaks <- c(7, 14, 30, 45, 90)
color_breaks <- rev(c('#ffffb2','#fed976','#feb24c','#fd8d3c','#f03b20','#bd0026'))

# Create Table
my_opps_table <- my_opps_acct %>% 
  filter(!opp_stage %in% c("Closed Won", "Closed Lost")) %>% 
  select(opp_id, account_id, account_name, opp_name, opp_amount, opp_stage, opp_close_date,
         opp_last_activity, account_last_activity) %>% 
  # Make account and opps urls
  mutate(Account = paste0(acct_url_pre, account_id, '/view">', account_name, '</a>')) %>% 
  mutate(Opportunity = paste0(opp_url_pre, opp_id, '/view">', opp_name, '</a>')) %>% 
  # Calculate days to close
  mutate(`Days to Close` = as.double(difftime(ymd(opp_close_date),
                                              ymd(today),
                                              units = "days"))) %>% 
  select(Account, Opportunity, opp_stage, opp_amount, opp_close_date, `Days to Close`) %>% 
  rename(`Opp Amount` = opp_amount,
         `Opp Stage` = opp_stage,
         `Close Date` = opp_close_date) %>% 
  unique() %>% 
  arrange(`Days to Close`)



# View opp table
DT::datatable(my_opps_table, escape = FALSE) %>% 
  formatStyle(columns = "Days to Close",
              backgroundColor = styleInterval(days_breaks, color_breaks)) %>% 
  formatCurrency(columns = "Opp Amount", currency = "$") %>% 
  formatStyle(columns = "Opp Amount", 
              background = styleColorBar(my_opps_table$`Opp Amount`, 'lightblue'),
              backgroundSize = '100% 90%',
              backgroundRepeat = 'no-repeat',
              backgroundPosition = 'center')
