library(tidyquant)
source("scripts/get_stockrow.R")

TJX_income <- get_stockrow("TJX", section = "income", dimension = "annual" )
TJX_balance <- get_stockrow("TJX", section = "balance", dimension = "annual")
TJX_cash <- get_stockrow("TJX", section = "cash", dimension = "annual")
TJX_metrics <- get_stockrow("TJX", section = "metrics", dimension = "annual")
TJX_growth <- get_stockrow("TJX", section = "growth", dimension = "annual")

TJX_income_q <- get_stockrow("TJX", section = "income", dimension = "Q")
TJX_balance_q <- get_stockrow("TJX", section = "balance", dimension = "Q")
TJX_cash_q <- get_stockrow("TJX", section = "cash", dimension = "Q")
TJX_metrics_q <- get_stockrow("TJX", section = "metrics", dimension = "Q")
TJX_growth_q <- get_stockrow("TJX", section = "growth", dimension = "Q")

TJX_income_12 <- get_stockrow("TJX", section = "income", dimension = "T")
TJX_cash_12 <- get_stockrow("TJX", section = "cash", dimension = "T")
TJX_metrics_12 <- get_stockrow("TJX", section = "metrics", dimension = "T")
TJX_growth_12 <- get_stockrow("TJX", section = "growth", dimension = "T")

income_names <- lapply(c(TJX_income, TJX_income_q, TJX_income_12), colnames) %>%
  names() %>%
  unique()

balance_names <- lapply(c(TJX_balance, TJX_balance), colnames) %>%
  names() %>%
  unique()

cash_names <- lapply(c(TJX_cash, TJX_cash_q, TJX_cash_12), colnames) %>%
  names() %>%
  unique()

metrics_names <- lapply(c(TJX_metrics, TJX_metrics_q, TJX_metrics_12), colnames) %>%
  names() %>%
  unique()

growth_names <- lapply(c(TJX_growth, TJX_growth_q, TJX_growth_12), colnames) %>%
  names() %>%
  unique()

