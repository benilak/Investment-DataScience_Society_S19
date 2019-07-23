library(shiny)
library(shinydashboard)
library(tidyquant)
library(DT)
library(cowplot)
source("../get_stockrow/get_stockrow.R")

untidy_stockrow <<- function(dat){
  dat %>% gather(key = "Key", value = "Value", -"Date") %>%
    spread(key = Date, value = Value) %>%
    .[match(colnames(dat)[-1], .$Key),] %>%
    select(Key, dim(.)[2]:2)
}

# sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    tags$head(tags$style(HTML('.content-wrapper { height: 2000px !important;}'))),
    menuItem("Stock Chart", tabName = "stockchart", icon = icon("chart-line")),
    menuItem("Customized Visualizations", tabName = "dashboard", icon = icon("chart-bar")),
    menuItem("Data", icon = icon("th"), tabName = "data"),
    sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                      label = "Search...")
  )
)


# body
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "stockchart", 
            h2(textOutput(outputId = "stock.header")),
            plotOutput(outputId = "cow"),
            
            fluidRow(
              column(12, sliderInput("date", "Dates:",
                                     min = today() - years(10),
                                     max = today(),
                                     value = c(today() - years(1), today()),
                                     timeFormat = "%Y-%m-%d",
                                     width = '95%')
              )
            ),
            
            fluidRow(
              column(6, sliderInput(inputId = "window1", label = "Rolling Average (Long-Term):",
                                    min = 1, max = 365,
                                    value = 100,
                                    width = '95%')
              ),
              column(6, sliderInput(inputId = "window2", label = "Rolling Average (Short-Term):",
                                    min = 1, max = 365,
                                    value = 28,
                                    width = '95%')
              )
            )
    ),
    
    # Graphs Tab
    tabItem(tabName = "dashboard",
            h2(textOutput(outputId = "visual.header")),
            fluidRow(
              
              tabBox(
                title = "",
                id = "tabset1", width = 12,
                tabPanel("Income Statement",
                         br(), 
                         box(
                           title = "Cash from Ops to Net Income", 
                           status = "primary", solidHeader = TRUE,
                           collapsible = TRUE,
                           plotOutput("cash_to_income.plot")
                         ),
                         box(
                           title = "Revenues", status = "primary", 
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           plotOutput("revenues.plot")
                         )
                ),
                tabPanel("Balance Sheet",
                         box(
                           title = "Working Capital to Sales", status = "primary", 
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           plotOutput("workingcap.plot")
                         ),
                         box(
                           title = "Profit Margin", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE,
                           plotOutput("profitmargin.plot")
                         )
                ),
                tabPanel("Cash Flow",
                         "There's nothing here :("
                ),
                tabPanel("Metrics",
                         box(
                           title = "EPS and DPS", status = "warning", solidHeader = TRUE,
                           collapsible = TRUE,
                           plotOutput("EPS_DPS.plot")
                           # radioButtons("revenues.buttons", "Coordinate Transformation",
                           #              c("Normal" = "norm",
                           #                "Log" = "log",
                           #                "Square Root" = "sqrt"))
                         ),
                         box(
                           title = "Return on Equity", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE,
                           plotOutput("ROE.plot")
                         ),
                         box(
                           title = "PE Ratio", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE,
                           plotOutput("PE.plot")
                         )
                        ),
                tabPanel("Growth",
                         box(
                           title = "Dividend Growth", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE,
                           plotOutput("dividendgrowth.plot")
                         ),
                         box(
                           title = "Asset Growth", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE,
                           plotOutput("assetgrowth.plot")
                         )
                ),
                tabPanel("Financial Leverage",
                         box(
                           title = "Assets to Equity", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE,
                           plotOutput("leverage.plot")
                         ),
                         box(
                           title = "Debt to Equity", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE,
                           plotOutput("debt_equity.plot")
                         )
                ),
                tabPanel("Ratios",
                         box(
                           title = "PB and PS Ratios", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE,
                           plotOutput("PB_PS.plot")
                         ),
                         box(
                           title = "Current Ratio", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE,
                           plotOutput("current.plot")
                         ),
                         box(
                           title = "POCF and PFCF Ratios", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE,
                           plotOutput("POCF_PFCF.plot")
                         )
                         
                )
                
              )
            )
    ),
    
    # Data Tables Tab
    tabItem(tabName = "data",
            h2(textOutput(outputId = "data.header")),
            # fluidRow(
            #   column(9, h2(textOutput(outputId = "data.header"), width = 10)),
            #   column(3, downloadButton("downloadData", "Download"))
            # ),
            
            tabBox(
              title = "",
              id = "tabset2", width = 12,
              tabPanel("Income Statement",
                       fluidRow(column(10, ""), 
                                column(2, downloadButton("download.income", "Download"))),
                       dataTableOutput("income.table")
              ),
              tabPanel("Balance Sheet",
                       fluidRow(column(10, ""), 
                                column(2, downloadButton("download.balance", "Download"))),
                       dataTableOutput("balance.table")
                       ),
              tabPanel("Cash Flow",
                       fluidRow(column(10, ""), 
                                column(2, downloadButton("download.cash", "Download"))),
                       dataTableOutput("cash.table")),
              tabPanel("Metrics",
                       fluidRow(column(10, ""), 
                                column(2, downloadButton("download.metrics", "Download"))),
                       dataTableOutput("metrics.table")),
              tabPanel("Growth",
                       fluidRow(column(10, ""), 
                                column(2, downloadButton("download.growth", "Download"))),
                       dataTableOutput("growth.table"))
            )
    )
  )
)


# ui
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "BYU-Idaho Investment Society)"),
  sidebar,
  body
)
