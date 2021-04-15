# REFERENCIAS
  # https://stats.stackexchange.com/questions/191851/var-forecasting-methodology/192165#192165   (STEP-BY-STEP VAR)
  # https://towardsdatascience.com/vector-autoregressions-vector-error-correction-multivariate-model-a69daf6ab618
  # https://stats.stackexchange.com/questions/246886/selecting-lag-order-for-var-and-vecm
  # https://bookdown.org/ccolonescu/RPoE4/vec-and-var-models.html   (IMPLEMENTAÇÃO)

  # https://stats.stackexchange.com/questions/194006/var-vecm-ardl-optimal-lag-selection/194071#194071
  # https://www.diva-portal.org/smash/get/diva2:638279/FULLTEXT02
  # https://www.zeileis.org/teaching/AER/Ch-TimeSeries.pdf
  # https://rdrr.io/cran/vars/f/inst/doc/vars.pdf



  # https://github.com/changshun/Course15fall/blob/master/Time%20series/supplementary/Analysis%20of%20Financial%20Time%20Series%20Third%20Edition%20%20By%20Ruey%20S.Tsay.pdf
  # http://ftp.uni-bayreuth.de/math/statlib/R/CRAN/doc/vignettes/vars/vars.pdf   (DESCRIÇÃO DE USO DO PACOTE VARS)
  # https://www.reed.edu/economics/parker/s14/312/tschapters/S13_Ch_5.pdf
  # https://doi.org/10.1057/s41278-020-00156-5 (TRABALHO APLICADO)

# PASSOS:
  # VAR
        # VERIFICAR PELO TESTE ADF SE SÉRIES SÃO I(0), CASO NÃO SEJAM, ACHAR REPRESENTAÇÃO I(0)
        # DEPOIS DE I(O), RODAR VAR(P)
        # TESTAR DIFERENTES LAGS E COMPARAR POR ALGUM CRITÉRIO: INFORMAÇÃO OU ACERTO
          # CASO SEJA ACERTO, O IDEAL É AVALIAR COM OBSERVAÇÕES OUT-OF-SAMPLE
        # AVALIAÇÃO RESIDUAL (PRECISA SER RUÍDO BRANCO)
            # AUSÊNCIA DE AUTOCORRELAÇÃO (NORMALIDADE NÃO É NECESSÁRIA)

  # VEC(M)


  # CASO AS VARIÁVEIS SEJAM COINTEGRADAS, PARTIR PARA VEC
library(urca)
library(vars)
library(tseries)
library(data.table)
library(tidyverse)

# DADOS ----
df_q3 <- fread("https://dataverse.harvard.edu/api/access/datafile/4509837")
dt_inicial <- as.Date("2020-01-01")

df <- df_q3 %>%
  mutate(ref.date = as.Date(ref.date)) %>%
  filter(
    grepl("ARZZ3|CEAB3|HGTX3|LREN3|AMAR3", ticker)
    & ref.date >= (dt_inicial - 15)
  ) %>%
  arrange(ref.date) %>%
  group_by(ticker) %>%
  mutate(
    price.dif = price.close - lag(price.close, n = 1L),
    log.return = log(price.close / lag(price.close, n = 1L))
  ) %>%
  filter(ref.date >= dt_inicial)


df_wide <- df %>%
  select(ref.date, ticker, price.close, price.dif, log.return) %>%
  pivot_wider(
    names_from = ticker,
    names_glue = "{ticker}_{.value}",
    values_from = c(price.close, price.dif, log.return)
  )

# DESCRIÇÃO DAS SÉRIES ----
              # PREÇO
ggplot(df, aes(x = ref.date, y = price.close, group = ticker, colour = ticker)) + 
  geom_line() + 
  geom_smooth() +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_x_date(breaks = scales::breaks_pretty(n = 10)) +
  theme_bw()

              # LOG RETORNO
ggplot(df, aes(x = ref.date, y = log.return, group = ticker, colour = ticker)) + 
  geom_line() + 
  # geom_smooth() + 
  scale_x_date(breaks = scales::breaks_pretty(n = 10)) +
  theme_bw()

              # PREÇO DIFF(1)
ggplot(df, aes(x = ref.date, y = price.dif, group = ticker, colour = ticker)) + 
  geom_line() + 
  geom_smooth() +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_x_date(breaks = scales::breaks_pretty(n = 10)) +
  theme_bw()

# TESTES DE ESTACIONARIEDADE ----
df %>%
  split(., f = list(.$ticker)) %>%
  lapply(., function(x) {
    x %>%
      ungroup() %>%
      select(price.close, log.return, price.dif) %>%
      lapply(., function (y) {data.frame(p.valor = tseries::adf.test(y)$p.value)}) %>%
      bind_rows(.id = "variavel")
  }) %>%
  bind_rows(.id = "ticker") %>%
  pivot_wider(names_from = variavel,
              values_from = c(p.valor)) %>%
  # mutate(Estacionario = if_else(p.valor > 0.05, FALSE, TRUE)) %>%
  View()


 # 
