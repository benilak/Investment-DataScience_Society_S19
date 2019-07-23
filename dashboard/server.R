
# server
server <- function(input, output) {
  
  # stockchart #
  
  dat <- reactive({
    req(input$searchButton)
    tq_get(input$searchText)
  })
  
  output$stock.header <- renderText({
    paste("Stock Chart:", input$searchText, sep = "  ")
  })
  
  output$visual.header <- renderText({
    paste("Custom Visualizations:", input$searchText, sep = "  ")
  })
  
  output$data.header <- renderText({
    paste("Data Tables:", input$searchText, sep = "  ")
  })
  
  start_date <- reactive({
    input$date[1]
  })
  end_date <- reactive({
    input$date[2]
  })
  dat_filt <- reactive({
    dat() %>% 
      filter(between(date, start_date(), end_date()))
  })
  dat_filt1 <- reactive({
    dat() %>%
      filter(between(date, start_date() - input$window1, end_date() + input$window1))
  })
  dat_filt2 <- reactive({
    dat() %>%
      filter(between(date, start_date() - input$window2, end_date() + input$window2))
  })
  
  plot1 <- reactive({
    dat_filt() %>%
      ggplot(aes(x = date, y = adjusted)) +
      geom_line(color = "snow", size = 2) +
      geom_line(color = "darkred") + 
      scale_y_continuous(labels = scales::dollar) + 
      geom_line(data = dat_filt1(),
                aes(y = rollmean(adjusted, input$window1, fill = NA, align = "center")),
                color = "gold") +
      geom_line(data = dat_filt2(),
                aes(y = rollmean(adjusted, input$window2, fill = NA, align = "center")),
                color = "forestgreen") +
      coord_cartesian(xlim = input$date, expand = c(0,0)) +
      theme_tq() +
      theme(axis.title = element_blank(),
            axis.text.x = element_blank())
  })
  
  plot2 <- reactive({
    dat_filt() %>%
      tq_transmute(select = volume, 
                   mutate_fun = period.apply,
                   INDEX = endpoints(.$date, 'days', 
                                     k = as.numeric(difftime(end_date(), start_date()))/50),
                   FUN = sum,
                   na.rm = TRUE,
                   col_rename = "volume") %>%
      ggplot() +
      geom_bar(aes(x = date, y = volume/1000000), stat = "identity", width = 1, color = "blue") +
      scale_y_continuous(labels = scales::comma) +
      labs(y = "Volume (millions)") +
      theme_tq() +
      theme(axis.title = element_blank())
  })
  
  output$cow <- renderPlot({
    
    plot_grid(plot1(), plot2(), align = "v", nrow = 2, rel_heights = c(3/4, 1/4))
    
  })
  
  # stockrow data #
  
  dat.income <- reactive({
    req(input$searchButton)
    get_stockrow(input$searchText, section = "income")
  })
  output$income.table <- renderDataTable({
    untidy_stockrow(dat.income())
  }, options = list(scrollX = TRUE))
  dat.balance <- reactive({
    req(input$searchButton)
    get_stockrow(input$searchText, section = "balance")
  })
  output$balance.table <- renderDT({
    untidy_stockrow(dat.balance())
  }, options = list(scrollX = TRUE))
  dat.cash <- reactive({
    req(input$searchButton)
    get_stockrow(input$searchText, section = "cash")
  })
  output$cash.table <- renderDT({
    untidy_stockrow(dat.cash())
  }, options = list(scrollX = TRUE))
  dat.metrics <- reactive({
    req(input$searchButton)
    get_stockrow(input$searchText, section = "metrics")
  })
  # datq.metrics <- reactive({
  #   req(input$searchButton)
  #   get_stockrow(input$searchText, section = "metrics", dimension = "quarter")
  # })
  output$metrics.table <- renderDT({
    untidy_stockrow(dat.metrics())
  }, options = list(scrollX = TRUE))
  dat.growth <- reactive({
    req(input$searchButton)
    get_stockrow(input$searchText, section = "growth")
  })
  output$growth.table <- renderDT({
    untidy_stockrow(dat.growth())
  }, options = list(scrollX = TRUE))

  # Plots #
  
  # Revenues
  output$revenues.plot <- renderPlot({
    dat.income() %>%
      mutate(ind = case_when(Revenue > lag(Revenue) ~ "+",
                             Revenue < lag(Revenue) ~ "-")) %>%
      ggplot(aes(x = Date, y = Revenue/1000000)) +
      geom_bar(aes(color = ind), 
               fill = "violet", size = 1, alpha = 0.8, 
               stat = "identity", show.legend = FALSE) +
      # coord_trans(y = "cart") +
      scale_x_date(date_breaks = "years", date_labels = "%Y") +
      scale_y_continuous(labels = scales::comma) +
      scale_fill_manual(values = c("tomato", "lightgreen")) +
      labs(title = "Revenues", subtitle = "(in millions $)") +
      theme_light() +
      theme(axis.title = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major.y = element_line(linetype = "dashed", color = "gray40"),
            axis.text.x = element_text(angle = 35, hjust = 1))
  })
  
  # Cash from Ops to Net Income
  output$cash_to_income.plot <- renderPlot({
    full_join(dat.income(), dat.cash(), by = "Date") %>%
      select(Date, `Net Income`, `Operating Cash Flow`) %>%
      gather(`Net Income`, `Operating Cash Flow`, key = "stat", value = "value") %>%
      ggplot(aes(x = Date, y = value/1000000, fill = stat)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_x_date(date_breaks = "years", date_labels = "%Y") +
      labs(title = "Operating Cash Flow to Net Income", subtitle = "(in millions $)", fill = "") +
      theme_light() +
      theme(axis.title = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major.y = element_line(linetype = "dashed", color = "gray40"),
            axis.text.x = element_text(angle = 35, hjust = 1),
            legend.position = "top")
  })
  
  # EPS and DPS
  output$EPS_DPS.plot <- renderPlot({
    dat.income() %>% 
      select(Date, EPS, `Dividend per Share`) %>%
      rename(DPS = `Dividend per Share`) %>%
      gather(EPS, DPS, key = "stat", value = "value") %>%
      ggplot(aes(x = Date, y = value, fill = stat)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_x_date(date_breaks = "years", date_labels = "%Y") +
      # scale_fill_manual(values = c("tomato", "lightgreen")) +
      labs(title = "EPS and DPS") +
      theme_light() +
      theme(axis.title = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major.y = element_line(linetype = "dashed", color = "gray40"),
            axis.text.x = element_text(angle = 35, hjust = 1))
  })
  
  # Working Cap
  output$workingcap.plot <- renderPlot({
    full_join(dat.income(), dat.balance(), by = "Date") %>%
      select(Date, `Total current assets`, `Total current liabilities`, Revenue) %>%
      mutate(`Working Cap` = `Total current assets` - `Total current liabilities`) %>%
      select(Date, `Working Cap`, Revenue) %>%
      ggplot(aes(x = Date, y = `Working Cap`/Revenue)) +
      geom_line(color = "red", size = 1.5, alpha = 0.5) +
      geom_point(color = "firebrick", size = 2.5) +
      scale_x_date(date_breaks = "years", date_labels = "%Y") +
      scale_y_continuous(labels = scales::percent) +
      labs(title = "Working Cap / Sales") +
      theme_light() +
      theme(axis.title = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major.y = element_line(linetype = "dashed", color = "gray40"),
            axis.text.x = element_text(angle = 35, hjust = 1),
            legend.position = "top")
  })
  
  # Net Income Margin - (income statement)
  output$profitmargin.plot <- renderPlot({
    dat.income() %>%
      mutate(ind = case_when(`Profit Margin` > lag(`Profit Margin`) ~ "+",
                             `Profit Margin` < lag(`Profit Margin`) ~ "-")) %>%
      ggplot(aes(x = Date, y = `Profit Margin`)) +
      geom_bar(aes(color = ind), size = 1, stat = "identity", fill = "skyblue",
               alpha = 0.8,
               show.legend = F) +
      #coord_trans(y = "sqrt") +
      scale_x_date(date_breaks = "years", date_labels = "%Y") +
      scale_color_manual(values = c("tomato", "green")) +
      labs(title = "Profit Margin", subtitle = "(in millions $)") +
      theme_light() +
      theme(axis.title = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major.y = element_line(linetype = "dashed", color = "gray40"),
            axis.text.x = element_text(angle = 35, hjust = 1))
  })
  
  # Return on Equity - (metrics)
  output$ROE.plot <- renderPlot({
    dat.metrics() %>%
      mutate(ind = case_when(ROE > lag(ROE) ~ "+",
                             ROE < lag(ROE) ~ "-")) %>%
      ggplot(aes(x = Date, y = ROE)) +
      geom_bar(aes(color = ind), size = 1, stat = "identity", fill = "skyblue",
               alpha = 0.8,
               show.legend = F) +
      #coord_trans(y = "sqrt") +
      scale_x_date(date_breaks = "years", date_labels = "%Y") +
      scale_y_continuous(labels = scales::percent) +
      scale_color_manual(values = c("tomato", "green")) +
      labs(title = "Return on Equity") +
      theme_light() +
      theme(axis.title = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major.y = element_line(linetype = "dashed", color = "gray40"),
            axis.text.x = element_text(angle = 35, hjust = 1))
  })
  
  # Financial Leverage (Equity Multiplier) = Total
  output$leverage.plot <- renderPlot({
    dat.balance() %>%
      ggplot(aes(x = Date, y = `Total shareholders equity` / `Total assets`)) +
      geom_line(color = "blue", size = 1.5, alpha = 0.5) +
      geom_point(color = "navyblue", size = 2.5) +
      scale_x_date(date_breaks = "years", date_labels = "%Y") +
      scale_y_continuous(labels = scales::percent) +
      labs(title = "Financial Leverage", subtitle = "Equity Multiplier (Assets / Equity)") +
      theme_light() +
      theme(axis.title = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major.y = element_line(linetype = "dashed", color = "gray40"),
            axis.text.x = element_text(angle = 35, hjust = 1),
            legend.position = "top")
  })
  
  # Debt to Equity
  output$leverage.plot <- renderPlot({
    dat.metrics() %>%
      ggplot(aes(x = Date, y = `Debt to Assets`)) +
      geom_line(color = "blue", size = 1.5, alpha = 0.5) +
      geom_point(color = "navyblue", size = 2.5) +
      scale_x_date(date_breaks = "years", date_labels = "%Y") +
      scale_y_continuous(labels = scales::percent) +
      labs(title = "Debt to Assets") +
      theme_light() +
      theme(axis.title = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major.y = element_line(linetype = "dashed", color = "gray40"),
            axis.text.x = element_text(angle = 35, hjust = 1),
            legend.position = "top")
  })
  
  # Dividend Growth
  output$dividendgrowth.plot <- renderPlot({
    dat.growth() %>%
      ggplot(aes(x = Date, y = `Dividends per Share Growth`)) +
      geom_line(color = "green", size = 1.5, alpha = 0.5) +
      geom_point(color = "forestgreen", size = 2.5) +
      scale_x_date(date_breaks = "years", date_labels = "%Y") +
      scale_y_continuous(labels = scales::percent) +
      labs(title = "Dividends per Share Growth") +
      theme_light() +
      theme(axis.title = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major.y = element_line(linetype = "dashed", color = "gray40"),
            axis.text.x = element_text(angle = 35, hjust = 1),
            legend.position = "top")
  })
  
  # Asset Growth
  output$assetgrowth.plot <- renderPlot({
    dat.growth() %>%
      ggplot(aes(x = Date, y = `Asset Growth`)) +
      geom_line(color = "purple", size = 1.5, alpha = 0.5) +
      geom_point(color = "violet", size = 2.5) +
      scale_x_date(date_breaks = "years", date_labels = "%Y") +
      scale_y_continuous(labels = scales::percent) +
      labs(title = "Asset Growth") +
      theme_light() +
      theme(axis.title = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major.y = element_line(linetype = "dashed", color = "gray40"),
            axis.text.x = element_text(angle = 35, hjust = 1),
            legend.position = "top")
  })
  
  # PE Ratio
  output$PE.plot <- renderPlot({
    dat.metrics() %>%
      ggplot(aes(x = Date, y = `PE ratio`)) +
      geom_line(color = "navyblue", size = 1.5, alpha = 0.5) +
      geom_point(color = "blue", size = 2.5) +
      scale_x_date(date_breaks = "years", date_labels = "%Y") +
      scale_y_continuous(labels = scales::percent) +
      labs(title = "PE Ratio") +
      theme_light() +
      theme(axis.title = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major.y = element_line(linetype = "dashed", color = "gray40"),
            axis.text.x = element_text(angle = 35, hjust = 1),
            legend.position = "top")
  })
  
  # Current Ratio
  output$current.plot <- renderPlot({
    dat.metrics() %>%
      ggplot(aes(x = Date, y = `Current ratio`)) +
      geom_line(color = "navyblue", size = 1.5, alpha = 0.5) +
      geom_point(color = "blue", size = 2.5) +
      scale_x_date(date_breaks = "years", date_labels = "%Y") +
      scale_y_continuous(labels = scales::percent) +
      labs(title = "PE Ratio") +
      theme_light() +
      theme(axis.title = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major.y = element_line(linetype = "dashed", color = "gray40"),
            axis.text.x = element_text(angle = 35, hjust = 1),
            legend.position = "top")
  })
  
  # PB and PS Ratios
  output$PB_PS.plot <- renderPlot({
    dat.metrics() %>% 
      select(Date, `PB ratio`, `PS ratio`) %>%
      gather(`PB ratio`, `PS ratio`, key = "stat", value = "value") %>%
      ggplot(aes(x = Date, y = value, color = stat)) +
      geom_line(size = 1.5, alpha = 0.5) +
      geom_point(size = 2.5) +
      scale_x_date(date_breaks = "years", date_labels = "%Y") +
      labs(title = "PB and PS Ratios", color = "") +
      theme_light() +
      theme(axis.title = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major.y = element_line(linetype = "dashed", color = "gray40"),
            axis.text.x = element_text(angle = 35, hjust = 1),
            legend.position = "top")
  })
  
  # POCF and PFCF
  output$POCF_PFCF.plot <- renderPlot({
    dat.metrics() %>% 
      select(Date, `POCF ratio`, `PFCF ratio`) %>%
      gather(`POCF ratio`, `PFCF ratio`, key = "stat", value = "value") %>%
      ggplot(aes(x = Date, y = value, color = stat)) +
      geom_line(size = 1.5, alpha = 0.8) +
      geom_point(size = 2.5) +
      scale_x_date(date_breaks = "years", date_labels = "%Y") +
      scale_color_manual(values = c("limegreen", "orange")) +
      labs(title = "POCF and PFCF Ratios", color = "") +
      theme_light() +
      theme(axis.title = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major.y = element_line(linetype = "dashed", color = "gray40"),
            axis.text.x = element_text(angle = 35, hjust = 1),
            legend.position = "top")
  })
}
