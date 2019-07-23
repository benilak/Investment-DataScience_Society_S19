library(tidyverse)
library(DT)


dat <- get_stockrow("TJX", section = "income", dimension = "annual")


Income_Statement <- dat %>% 
  select(Revenue, EBITDA, `Operating Income`, `Net Income`)


sRatios <- dat %>% 
  select(`Gross Margin`, `EBITDA Margin`)



balance <- get_stockrow("TJX", section = "balance", dimension = "annual")


Balance_Sheet <- balance %>% 
  select(`Cash and short-term investments`, `Inventories`, `Receivables`, `Total current assets`, `Property, Plant & Equipment Net`, `Goodwill and Intangible Assets`, `Total assets`, Payables, `Total debt`, `Total shareholders equity`)


flow <- get_stockrow("TJX", section = "cash flow", dimension = "annual")

CashFlow_Sheet <- flow %>% 
  select(`Net cash flow / Change in cash`, `Depreciation & Amortization`, `Operating Cash Flow`, `Capital Expenditure`, `Investing Cash flow`, `Dividend payments`, `Financing Cash Flow`, `Free Cash Flow`)
  


metrics <- get_stockrow("TJX", section = "metrics", dimension = "annual")




