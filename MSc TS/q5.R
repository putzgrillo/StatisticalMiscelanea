library(tidyverse)
library(tidymodels)
library(not)

# CRIAR DADOS
y <- 3
for (w in seq(2, 500)) {
  set.seed(193827 + w)
  y[w] <- (1 + (cos(y[w-1]) / 314)) * y[w-1] + rnorm(1, sd = 0.05)
}

plot(y, type = "o")

df <- data.frame(y = y) %>%
  mutate(y_lag = lag(y, n = 1))
  
# AJUSTE BOOTSTRAP
df_bootstraps <- bootstraps(df, 
                            times = 1000, 
                            apparent = TRUE)

# FUNÇÃO MODELO
mqo_bootstrap <- function(df_sample) {
  lm(y ~ 0 + y_lag, data = analysis(df_sample))
}


modelos_bootstrap <-
  df_bootstraps %>% 
  mutate(model = map(splits, mqo_bootstrap),
         coef_info = map(model, tidy))

boot_coefs <- 
  modelos_bootstrap %>% 
  unnest(coef_info)

percentile_intervals <- int_pctl(modelos_bootstrap, coef_info)


ggplot(boot_coefs, aes(estimate)) +
  geom_histogram(bins = 30) +
  facet_wrap( ~ term, scales = "free") +
  geom_vline(aes(xintercept = .lower), data = percentile_intervals, col = "blue") +
  geom_vline(aes(xintercept = .upper), data = percentile_intervals, col = "blue") +
  theme_bw()


boot_aug <- 
  modelos_bootstrap %>% 
  mutate(augmented = map(model, augment)) %>% 
  unnest(augmented)



boot_aug %>%
  mutate(.rownames = as.numeric(gsub("\\..*", "", .rownames))) %>%
  filter(.rownames < 50) %>% 
  ggplot(., aes(x = .rownames, y = y)) +
    geom_line(aes(y = .fitted, group = id), alpha = .2, col = "blue") +
    geom_point(size = 1.5) +
    theme_bw()


quebras <- seq(0, 500, 50)

cut(seq(500), breaks = quebras)


boot_aug %>%
  mutate(.rownames = as.numeric(gsub("\\..*", "", .rownames))) %>%
  mutate(
    ind = if_else(.rownames %% 51 == 0, 1, .rownames %% 50),
    ind_fac = cut(.rownames, seq(0, 500, 50))
  ) %>%
  # filter(.rownames < 50) %>% 
  ggplot(., aes(x = ind, y = y)) +
    geom_line(aes(y = .fitted, group = id), alpha = .2, col = "blue") +
    geom_point(size = 1.5) +
    facet_wrap(~ind_fac, strip.position = "top") +
    theme_bw()
    
