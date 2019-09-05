library(tidyverse)
library(lubridate)
# DADOS ----
  # DADOS: AJUSTE EM DADOS ECONOMICOS ----
economics_long <- ggplot2::economics_long %>%
  mutate(
    rotulo = ifelse(variable == "psavert", paste(round(value, 1), "%", sep = ""),
                  ifelse(variable == "pce", paste("$", round(value / 1000, 1), "tr", sep = ""),
                         format(round(value, 1), big.mark = ".", decimal.mark = ",")))
  )

economics_long <- list(
  US = data.frame(country = "US", economics_long),
  UE = data.frame(country = "European Union", {economics_long %>% mutate(value = value * 0.8)})
) %>%
  do.call(rbind, .) %>%
  filter(month(date) == 12 & year(date) >= 2005)

# PLOT
ggplot(economics_long, aes(y = value, x = date, colour = country)) +
  geom_line(linetype = 2, size = 2) +
  geom_text(
    aes(y = value, x = date, label = rotulo, colour = country),
    colour = "black", size = 4, angle = 0, position = position_jitter()
  ) +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_x_date(labels = scales::date_format()) +
  facet_wrap(~variable, scales = "free") +
  xlab("Data") + ylab("") +
  theme_bw() +
  theme(
    axis.text = element_text(size = 15, angle = 0),
    axis.title = element_text(size = 15),
    strip.text = element_text(size = 20)
  )
