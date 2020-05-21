# RStudio Warehouse Connection

warehouse_UID <- "rjohnson"

# Connect to RStudio Warehouse
con <- DBI::dbConnect(odbc::odbc(), database = "marketing", 
                      Driver = "Redshift",
                      host = "redhouse.cyii7eabibhu.us-east-1.redshift.amazonaws.com", 
                      port = "5439", 
                      UID = warehouse_UID, 
                      PWD = Sys.getenv("WAREHOUSE_PASSWORD"))


# Gather Accounts, Opps, and Partner info
opps <- tbl(con, in_schema("salesforce", "opportunity")) %>% collect()
acct <- tbl(con, in_schema("salesforce", "account")) %>% collect()
partners <- tbl(con, in_schema("salesforce", "account_partner")) %>% collect()
tasks <- tbl(con, in_schema("salesforce", "task"))