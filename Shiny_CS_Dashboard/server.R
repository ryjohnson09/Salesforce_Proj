## server.R ##

server <- function(input, output) {
  
  # Warehouse Connection --------------------------------------------------------
  con <- DBI::dbConnect(odbc::odbc(), 
                        database = "marketing", 
                        Driver = "Redshift",
                        host = "redhouse.cyii7eabibhu.us-east-1.redshift.amazonaws.com", 
                        port = 5439, 
                        UID = Sys.getenv("WAREHOUSE_USER"), 
                        PWD = Sys.getenv("WAREHOUSE_PASSWORD"))
  
  
  # Pull CS Rep Data -------------------------------------------------------------
  cs_table <- reactive({  
    opps <- tbl(con, in_schema("salesforce", "opportunity")) %>% 
        filter(owner_id == local(input$cs_name)) %>% 
        dplyr::rename_all(function(x) paste0("opp_", x)) %>% 
        collect()
        
      acct <- tbl(con, in_schema("salesforce", "account")) %>% 
        filter(owner_id == local(input$cs_name)) %>% 
        dplyr::rename_all(function(x) paste0("acct_", x)) %>% 
        collect()
      
      acct_opps <- opps %>% 
        left_join(acct, by = c("opp_account_id" = "acct_id")) %>% 
        select(opp_account_id, opp_id, acct_name, opp_name, opp_close_date, opp_amount,
               opp_type, opp_stage_name, everything())
      acct_opps
  })
  
  
  
  # CS Opp Plot ------------------------------------------------------
  output$cs_opp_plot <- renderPlotly({
    cs_opp_plot <- cs_table() %>%
      filter(opp_is_deleted == 0) %>%
      filter(year(opp_close_date) == year(today())) %>%
      select(opp_name,
             acct_name,
             opp_amount,
             opp_type,
             opp_stage_name,
             opp_close_date) %>% 
      ggplot() +
      geom_point(aes(
        x = opp_close_date,
        y = opp_amount,
        color = opp_stage_name,
        shape = opp_type,
        label = acct_name
      )) +
      scale_y_continuous(labels = dollar) +
      scale_x_date(labels = date_format("%b-%Y")) +
      theme_classic() + 
      labs(x = 'Close Date', y = "Opp Value") + 
      theme(legend.position = "none")
    
    ggplotly(cs_opp_plot)
  })
  
  
  
  # CS Opp Table ---------------------------------------------------------
  # Create pre-url hyperlinks
  acct_url_pre <- '<a href="https://na39.lightning.force.com/lightning/r/Account/'
  opp_url_pre  <- '<a href="https://na39.lightning.force.com/lightning/r/Opportunity/'
  
  # Opp Table days/color breaks
  days_breaks <- c(7, 14, 30, 45, 90)
  color_breaks <- rev(c('#ffffb2','#fed976','#feb24c','#fd8d3c','#f03b20','#bd0026'))
  
  # Render DT Opp Table
  output$cs_date_table <- renderDT({
    cs_table() %>% 
      filter(opp_is_deleted == 0) %>% 
      select(opp_id, opp_account_id, acct_name, opp_name, opp_amount, 
             opp_type, opp_stage_name, opp_close_date) %>% 
      filter(!opp_stage_name %in% c("Closed Won", "Closed Lost", "Unqualified")) %>%
      
      # Make account and opps urls
      mutate(Account = paste0(acct_url_pre, opp_account_id, '/view">', acct_name, '</a>')) %>% 
      mutate(Opportunity = paste0(opp_url_pre, opp_id, '/view">', opp_name, '</a>')) %>% 
      
      # Calculate days to close
      mutate(`Days to Close` = as.double(difftime(ymd(opp_close_date),
                                                  ymd(today()),
                                                  units = "days"))) %>% 
      select(Account, Opportunity, opp_stage_name, opp_amount, opp_close_date, `Days to Close`) %>% 
      rename(`Opp Amount` = opp_amount,
             `Opp Stage` = opp_stage_name,
             `Close Date` = opp_close_date) %>% 
      unique() %>% 
      arrange(`Days to Close`) %>%
      
      # Format Opp DT Table
      DT::datatable(escape = FALSE) %>% 
      formatStyle(columns = "Days to Close",
                  backgroundColor = styleInterval(days_breaks, color_breaks)) %>% 
      formatCurrency(columns = "Opp Amount", currency = "$") %>% 
      formatStyle(columns = "Opp Amount",
                  backgroundSize = '100% 90%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center')
  })
}

