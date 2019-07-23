
source("get_stockrow.R")

TJX_income <- get_stockrow("TJX", section = "income", dimension = "year")
ALE_income <- get_stockrow("ALE", section = "income")

tsla_income <- get_stockrow("TSLA", section = "income", tidy = TRUE)
tsla_income_y <- get_stockrow("TSLA", section = "income", dimension = "yearly", tidy = TRUE)
tsla_income %>% 
  mutate(ind = case_when(Revenue > lag(Revenue) ~ "+",
                         Revenue < lag(Revenue) ~ "-")) %>%
  ggplot(aes(x = Date, y = Revenue/1000000)) +
  geom_bar(aes(fill = ind), size = 1, stat = "identity", show.legend = F) +
  #coord_trans(y = "sqrt") +
  scale_x_date(date_breaks = "years", date_labels = "%b-%y") +
  scale_y_continuous(labels = scales::comma, trans = "sqrt",
                     expand = c(0,2)) +
  scale_fill_manual(values = c("tomato", "lightgreen")) +
  labs(title = "Revenues", subtitle = "Quarterly, in millions $") +
  theme_light() +
  theme(axis.title = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(linetype = "dashed", color = "gray40"),
        axis.text.x = element_text(angle = 35, hjust = 1))






# Revenues - (income statement)
TJX_income %>% 
  mutate(ind = case_when(Revenue > lag(Revenue) ~ "+",
                         Revenue < lag(Revenue) ~ "-")) %>%
  ggplot(aes(x = Date, y = Revenue/1000000)) +
  geom_bar(aes(fill = ind), size = 1, stat = "identity", show.legend = F) +
  coord_trans(y = "fixed") +
  scale_x_date(date_breaks = "years", date_labels = "%b-%y") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("tomato", "lightgreen")) +
  labs(title = "Revenues", subtitle = "(in millions $)") +
  theme_light() +
  theme(axis.title = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(linetype = "dashed", color = "gray40"),
        axis.text.x = element_text(angle = 35, hjust = 1))


# Cash from Ops to Net Income - (cash flow $ income statement)
TJX_cash <- get_stockrow("TJX", section = "cash")
full_join(TJX_income, TJX_cash, by = "Date") %>%
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

# EPS and DPS - (income statement)
TJX_income %>% 
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

ALE_income %>% 
  select(Date, EPS, `Dividend per Share`) %>%
  rename(DPS = `Dividend per Share`) %>%
  gather(EPS, DPS, key = "stat", value = "value") %>%
  ggplot(aes(x = Date, y = value, fill = stat)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  labs(title = "EPS and DPS", fill = "") +
  theme_light() +
  theme(axis.title = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(linetype = "dashed", color = "gray40"),
        axis.text.x = element_text(angle = 35, hjust = 1),
        legend.position = "top")

# Working Cap / Sales
# Working Cap = Total Current Assets - Total Current Liabilities (balance sheet)
TJX_balance <- get_stockrow("TJX", section = "balance")
full_join(TJX_income, TJX_balance, by = "Date") %>%
  select(Date, `Total current assets`, `Total current liabilities`, Revenue) %>%
  mutate(`Working Cap` = `Total current assets` - `Total current liabilities`) %>%
  select(Date, `Working Cap`, Revenue) %>%
  ggplot(aes(x = Date, y = `Working Cap`/Revenue)) +
  geom_bar(stat = "identity", alpha = 0.7, fill = "red") +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Working Cap / Sales") +
  theme_light() +
  theme(axis.title = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(linetype = "dashed", color = "gray40"),
        axis.text.x = element_text(angle = 35, hjust = 1),
        legend.position = "top")
# line plot 
full_join(TJX_income, TJX_balance, by = "Date") %>%
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

# Net Income Margin - (income statement)
TJX_income %>%
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

# Return on Equity - (metrics)
TJX_metrics <- get_stockrow("TJX", section = "metrics")
TJX_metrics %>%
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
# line plot
TJX_metrics %>%
  ggplot(aes(x = Date, y = ROE)) +
  geom_line(color = "blue", size = 1.5, alpha = 0.5) +
  geom_point(color = "navyblue", size = 2.5) +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Return on Equity") +
  theme_light() +
  theme(axis.title = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(linetype = "dashed", color = "gray40"),
        axis.text.x = element_text(angle = 35, hjust = 1),
        legend.position = "top")

# Financial Leverage (Equity Multiplier) = Total
TJX_balance %>%
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





TJX_metrics %>% 
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


