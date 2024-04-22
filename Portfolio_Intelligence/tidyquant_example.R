
library(dplyr)
library(plotly)
library(tidyquant)

Ra <- c("AAPL", "GOOG", "NFLX") %>%
  tq_get(get  = "stock.prices",
         from = "2010-01-01",
         to   = "2015-12-31") %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Ra")

Rb <- "DJI" %>%
  tq_get(get  = "stock.prices",
         from = "2010-01-01",
         to   = "2015-12-31") %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Rb")

wts <- c(0.25, 0.25, 0.5)
Rp<- Ra %>%
  tq_portfolio(assets_col  = symbol, 
               returns_col = Ra, 
               weights     = wts, 
               col_rename  = "Ra")

RpRb <- left_join(Rp,
                  Rb,
                  by = "date")

plot_tbl = rbind(
  RpRb %>% 
  select(date, Ra) %>% 
  mutate(symbol = "Portfolio") %>% 
  rename(value = Ra)
,
  RpRb %>%
    select(date, Rb) %>%
    mutate(symbol = "Benchmark") %>% 
    rename(value = Rb)
,
  Ra %>%rename(value = Ra)
)

CAPM = RpRb %>%
  tq_performance(Ra = Ra, Rb = Rb, performance_fun = table.CAPM)

ggplotly(plot_tbl %>%
           ggplot(aes(date, value, colour = symbol)) +
           geom_line(size = 1, alpha = .9) +
           theme_minimal(base_size=16) +
           theme(axis.title = element_blank(),
                 plot.background  = element_rect(fill = "#222222"),
                 panel.background = element_rect(fill = "#222222"),
                 panel.grid       = element_blank(),
                 legend.text      = element_text(colour = "white"))
)

