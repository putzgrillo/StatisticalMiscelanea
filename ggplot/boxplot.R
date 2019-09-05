library(tidyverse)
library(lubridate)
# DADOS ----
  # DADOS: AJUSTE EM DADOS CONSUMO COMBUSTÍVEL ----
mpg <- mpg %>%
  mutate(
    TRANSMISSAO = ifelse(grepl("auto", trans), "AUTOMÁTICA", "MANUAL"),
    CILINDROS = paste(cyl, " Cilindros", sep = "")
  )

ggplot(mpg, aes(x = TRANSMISSAO, y = cty, colour = TRANSMISSAO)) +
  geom_boxplot() +
                # LINHAS COM MÉDIA
  geom_hline(
    data = {mpg %>% group_by(CILINDROS, TRANSMISSAO) %>% summarise(Media = mean(cty))}, 
    mapping = aes(yintercept = Media, colour = TRANSMISSAO), 
    size = 1, linetype = 2, alpha = 0.7
  ) +
  ylab("Nome Eixo Y") + xlab("Nome Eixo X") +
                # TEXTO
  stat_summary(geom = "text", fun.y = quantile,
               aes(label = paste(round(..y.., 1), "", sep = ""), colour = TRANSMISSAO),
               position = position_nudge(x = 0.33), size = 3.5
  ) +
                # ESCALA LOG 
  scale_y_continuous(labels = scales::number_format()) +
                # FACETADO
  facet_wrap(~CILINDROS, scales = "free") + 
  theme_bw() +
  theme(
    # legend.position = c(0.96, 0.96), 
    # legend.justification = c(0.96, 0.96),
    legend.text = element_text(size = 15),
    axis.text.y = element_text(size = 15, angle = 0),
    axis.text.x = element_text(size = 15, angle = 90, hjust = 1),
    strip.text.x = element_text(size = 15)
  )
