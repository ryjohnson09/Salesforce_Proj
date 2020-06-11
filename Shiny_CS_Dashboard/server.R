## server.R ##

server <- function(input, output) {
  
  # Warehouse Connection --------------------------------------------------------
  con <- src_wh()
  
  
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
  
  output$cs_data_table <- renderDataTable({
    DT::datatable(cs_table(), options = list(scrollX = TRUE))
  })
  
  
  # CS Opp Plot --------------------------------------------
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
  
}

