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
  
  # Value Boxes ---------------------------------------------------------------------
  # Closed Won Total
  output$vbox_won <- renderValueBox({
    
    closed_won_total <- cs_table() %>% 
      filter(opp_is_deleted == 0) %>%
      filter(year(opp_close_date) == year(today())) %>% 
      filter(opp_stage_name == "Closed Won") %>% 
      summarise(won = sum(opp_amount, na.rm = TRUE)) %>% 
      pull()
    
    valueBox(scales::dollar(closed_won_total), 
             "Closed Won", 
             icon=icon("trophy"), 
             color='green')
  })
  
  # Pipeline Total
  output$vbox_pipeline <- renderValueBox({
    
    pipeline <- cs_table() %>% 
      filter(opp_is_deleted == 0) %>%
      filter(year(opp_close_date) == year(today())) %>% 
      filter(opp_stage_name %in% c('Qualifying', 'Quoted', 'Evaluation')) %>% 
      summarise(pipe = sum(opp_amount, na.rm = TRUE)) %>% 
      pull()
    
    valueBox(scales::dollar(pipeline), 
             "Pipeline", 
             icon=icon("money"),
             color='yellow')
  })
  
  # Opps Open This Month
  output$vbox_month_open <- renderValueBox({
    
    opp_open_month <- cs_table() %>% 
      filter(opp_is_deleted == 0) %>% 
      filter(month(opp_close_date) == month(today())) %>% 
      filter(!opp_stage_name %in% c("Closed Won", "Closed Lost")) %>% 
      tally()
       
    valueBox(opp_open_month, 
             'Opps Open This Month', 
             icon = icon("box-open"),
             color='red')
  })
  
  # CS Opp Plot -----------------------------------------------------------------------
  output$cs_opp_plot <- renderPlotly({
    suppressWarnings(
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
      theme(legend.position = "none"))
    
      ggplotly(cs_opp_plot)
  })
  
  
  
  # CS Opp Table --------------------------------------------------------------------------
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
  
  # Acct Contact Sunburst Plot ----------------------------------------------------------------
  output$acct_sunburst <- renderSund2b({
    # get tasks
    tasks <- tbl(con, in_schema("salesforce", "task"))
    
    # Extract all calls from accounts
    all_calls <- tasks %>% 
      filter(task_subtype == "Call", type == "Call") %>% 
      filter(is_deleted == "0") %>% # This line is weird and could cause problems
      filter(status == "Completed") %>%
      select(id, account_id, what_id, activity_date,
             subject, description, status,
             who_id, owner_id) %>%
      collect()
    
    my_sunburst_table <- cs_table() %>%
      filter(opp_is_deleted == 0) %>%
      filter(opp_stage_name %in% c("Order Submitted", "Evaluation", "Quoted")) %>%
      filter(!is.na(opp_amount))%>%
      select(opp_id, opp_account_id, acct_name, opp_name, opp_amount, opp_stage_name, opp_close_date,
             opp_last_activity_date, acct_last_activity_date) %>% 
      # Calculate days to close
      mutate(days_to_close = as.double(difftime(ymd(opp_close_date),
                                                ymd(today()),
                                                units = "days"))) %>% 
      select(acct_name, opp_name, opp_stage_name, opp_amount, opp_close_date, days_to_close) %>% 
      unique() %>% 
      mutate(opp_name_days = paste0(opp_name, " -- Days to close: ", days_to_close)) %>% 
      select(acct_name, opp_name_days, opp_amount, opp_stage_name, days_to_close) %>%
      arrange(days_to_close)
    
    # merge calls into accounts
    calls_account <- cs_table() %>% 
      left_join(all_calls, by = c("opp_account_id" = "account_id")) %>% 
      select(acct_name, activity_date) %>% 
      # Calculate days from last call
      mutate(days_last_call = as.double(difftime(ymd(today()),
                                                 ymd(activity_date),
                                                 units = "days"))) %>% 
      group_by(acct_name) %>% 
      filter(days_last_call == min(days_last_call) | is.na(days_last_call)) %>% 
      unique() %>% 
      ungroup() %>% 
      mutate(account_name_days = paste0(acct_name, " -- Days since last call: ", days_last_call))
    
    # Add to sunburst table and change account name
    my_sunburst_table_call <- my_sunburst_table %>% 
      left_join(calls_account, by = "acct_name") %>% 
      select(account_name_days, everything()) %>% 
      select(-acct_name) %>%
      arrange(days_to_close)
    
    # Add colors conditionally
    calls_account_color <- calls_account %>% 
      mutate(acct_color = ifelse(days_last_call <= 90, "#0571b0",
                          ifelse(days_last_call > 90 & days_last_call <= 180, "#92c5de",
                          ifelse(days_last_call > 180 & days_last_call <= 270, "#f4a582", "#ca0020")))) %>% 
      mutate(acct_color = ifelse(is.na(acct_color), "#ca0020", acct_color))
    
    # Color opps by days to close
    opps_color <- my_sunburst_table_call %>% 
      select(opp_name_days, days_to_close) %>% 
      mutate(opp_color = ifelse(days_to_close <= 0, "#b10026",
                         ifelse(days_to_close > 0 & days_to_close <= 7, "#e31a1c",
                         ifelse(days_to_close > 7 & days_to_close <= 14, "#fc4e2a",
                         ifelse(days_to_close > 14 & days_to_close <= 21, "#fd8d3c",        
                         ifelse(days_to_close > 21 & days_to_close <= 45, "#feb24c",
                         ifelse(days_to_close > 45 & days_to_close <= 90, "#fed976", "#a1d99b")))))))
    
    # Generate sunburs plot
    sunburst_tree <- my_sunburst_table_call %>% 
      select(account_name_days, opp_name_days, opp_amount) %>% 
      d3_nest(value_cols = "opp_amount")
    
    my_colors <- list(range = c(calls_account_color$acct_color, opps_color$opp_color),
                      domain = c(calls_account_color$account_name_days, opps_color$opp_name_days))
    
    sunburst_opps <- sund2b(sunburst_tree, 
                            valueField = "opp_amount", 
                            colors = my_colors, 
                            rootLabel = "All Open Opps", 
                            width = "100%",
                            breadcrumbs = sund2bBreadcrumb(enabled = FALSE))
    sunburst_opps
  })
  
}

