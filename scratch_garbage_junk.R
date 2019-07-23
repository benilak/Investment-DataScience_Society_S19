library(tidyverse)
library(lubridate)

source("get_stockrow.R")


tsla_income <- get_stockrow("TSLA", section = "income", tidy = TRUE) %>%
  mutate(Date = parse_date(Date))

# revenues, yellow and black
tsla_income %>% 
  ggplot(aes(x = Date, y = Revenue/1000000)) +
  geom_point(color = "yellow", size = 2) +
  geom_line(color = "yellow1", size = 1) +
  geom_line(color = "lightyellow", alpha = .2, size = 3) + 
  #coord_trans(y = "sqrt") +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  scale_y_continuous(labels = scales::comma, trans = "log", breaks = c(50, 200, 500, 2000, 3500)) +
  labs(title = "Revenues", subtitle = "Quarterly, in millions $") +
  theme(axis.text.x = element_text(angle = 35, hjust = 1),
        panel.background = element_rect(fill = "gray30"),
        axis.title = element_blank(),
        panel.grid.minor = element_line(color = "gray40"))
    
# revenues, coral

mylo <- loess(Revenue ~ as.numeric(Date), data=tsla_income, degree=1)



tsla_income %>% 
  ggplot(aes(x = Date, y = Revenue/1000000)) +
  geom_bar(color = "navy", size = 1, width = 70, fill = "coral", stat = "identity") +
  scale_x_date(date_breaks = "years", date_labels = "%Y",
               limits = as.Date(c("2009-01-01", "2021-01-01"))) +
  #geom_smooth(method = "lm") +
  # geom_line(data = cbind(Date = as.Date(tsla_income$Date), Fit = mylo$fitted),
  #           aes(x = Date, y = Fit)) +
  scale_y_continuous(labels = scales::comma, trans = "sqrt",
                     breaks = c(100, 1000, 2000, 4000, 6000)) +
  labs(title = "Revenues", subtitle = "Quarterly, in millions $", 
       caption = "Source: stockrow.com") +
  theme_light() +
  theme(axis.title = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(linetype = "dashed", color = "gray40"))

# revenues, growth indicator colors
tsla_income %>% 
  mutate(ind = case_when(Revenue > lag(Revenue) ~ "+",
                         Revenue < lag(Revenue) ~ "-")) %>%
  ggplot(aes(x = Date, y = Revenue/1000000)) +
  geom_bar(aes(fill = ind), size = 1, width = 70, stat = "identity", show.legend = F) +
  #coord_trans(y = "sqrt") +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  scale_y_continuous(labels = scales::comma, trans = "sqrt",
                     expand = c(0,1)) +
  scale_fill_manual(values = c("tomato", "lightgreen")) +
  labs(title = "Revenues", subtitle = "Quarterly, in millions $") +
  theme_light() +
  theme(axis.title = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(linetype = "dashed", color = "gray40"))


tsla_income %>%
  ggplot() +
  geom_bar(aes(x = Date, y = Revenue), stat = "identity")
  









