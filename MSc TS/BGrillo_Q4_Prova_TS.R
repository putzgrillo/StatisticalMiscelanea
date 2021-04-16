# PROVA SÉRIES TEMPORAIS
library(data.table)
library(tidyverse)
library(lubridate)
library(tidymodels)   # framework pra machine learning da equipe RStudio (Max Kuhn)
library(finetune)     # simmulated annealing para otimização hiperparâmetros
library(vip)          # importância variável
library(timetk)       # resampling para datas



setwd('/media/bgrillob/DATABASE/aMestrado Estatística/Disciplinas/T4 - Series Temporais/PROVA/')
# STRINGS ----
cores <- parallel::detectCores() - 1
n_initial_parameters <- 25            # PARÂMETROS_INICIAIS = n_parametros * n_initial_parameters

# Q4: ----
# Q4: BAIXAR DADOS E SELECIONAR ALEATORIAMENTE 10 AÇÕES PARA ESTIMAR RETORNO RENNER ----
      # OBS: APENAS AÇÕES QUE EM AO MENOS UM DIA TENHAM TIDO MAIS DE R$ 3 MI EM NEGOCIAÇÃO
df_q4 <- fread("https://dataverse.harvard.edu/api/access/datafile/4509837")
dt_inicial <- as.Date("2019-01-01")
df_q4 <- df_q4 %>%
  filter(ref.date >= (dt_inicial - 15))

set.seed(15081991)
titulos_usar <- c("LREN3.SA", sample(unique(df_q4$ticker[df_q4$volume > 3000000]), 
                                     size = 10, replace = FALSE))

# Q4: FILTRAR E AJUSTAR DATASET ----
df <- df_q4 %>%
  filter(ticker %in% titulos_usar) %>%
  arrange(ref.date) %>%
  group_by(ticker) %>%
  mutate(log.return = log(price.close / lag(price.close, n = 1L))) %>%
  mutate(
    l1.log.return = lag(log.return, n = 1),
    l2.log.return = lag(log.return, n = 2),
    l3.log.return = lag(log.return, n = 3)
  ) %>%
  select(
    ticker, ref.date, log.return, l1.log.return, l2.log.return, l3.log.return
  ) %>%
  filter(ref.date >= dt_inicial) %>%
  pivot_wider(
    names_from = ticker,
    names_glue = "{ticker}_{.value}",
    values_from = c(log.return, l1.log.return, l2.log.return, l3.log.return)
  ) %>%
  rename(y = LREN3.SA_log.return) %>% # variável a ser prevista
  select_at(vars(-contains("_log"))) # REMOVER REALIZAÇÕES t

# Q4: MODELAGEM ----
  # Q4: MODELAGEM: SPLIT TREINO/TESTE E VALIDAÇÃO ----
df_split <- initial_split(df, prop = 9.5/10)

df_training <- training(df_split)
df_test <- testing(df_split)
df_folds <- df_training %>%
  vfold_cv(v = 10, repeats = 1)

df_split <- timetk::time_series_split(df,
                              assess = 30,  # ÚLTIMOS 30 DIAS SÃO PARA TESTE
                              cumulative = TRUE)
            # PLOT SÉRIE
df_split %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(ref.date, y, .interactive = FALSE)

  # Q4: MODELAGEM: RF: PRE PROCESSING ----
rf_recipe <- recipe(y ~ ., data = df_training) %>%
  # step_timeseries_signature(ref.date) %>%           # cria variáveis a partir de data/hora
  # step_pca(all_predictors(), num_comp = tune()) %>% 
  update_role(ref.date, new_role = "ID")

  # Q4: MODELAGEM: RF: MODELO E FLUXO DE TRABALHO ---- 
rf_model <- 
  rand_forest(
    mtry = tune(),
    min_n = tune(),
    trees = tune()
  ) %>% 
  set_engine(
    "ranger", 
    num.threads = cores,
    importance = "impurity"
  ) %>% 
  set_mode("regression") 

rf_workflow <- 
  workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(rf_recipe)

  # Q4: MODELAGEM: RF: OTIMIZAÇÃO HIPERPARÂMETROS ----
rf_parameters <- rf_workflow %>%
  parameters() %>%
  update(
    # num_comp = num_comp(range = c(0L, 20L)),                   # PCA
    mtry = mtry(range = c(3L, 15L)),
    trees = trees(range = c(1000L, 2000L)),
    min_n = min_n(range = c(10L, 200L))
  )

  # Q4: MODELAGEM: RF: OTIMIZAÇÃO HIPERPARÂMETROS: INITIAL TUNE (BAYESIAN) ----
tempo <- proc.time()
rf_initial_tune <- rf_workflow %>%
  tune_bayes(object = .,
             resamples = df_folds,
             initial = n_initial_parameters * length(rf_model$args),
             param_info = rf_parameters,
             control = control_bayes(save_pred = TRUE),
             metrics = metric_set(rmse))
proc.time() - tempo

  # Q4: MODELAGEM: RF: OTIMIZAÇÃO HIPERPARÂMETROS: FINE TUNE (SIMULATED ANNEALING) ----
tempo <- proc.time()
rf_final_tune <- rf_workflow %>%
  tune_sim_anneal(object = .,
                  resamples = df_folds,
                  iter = 10,
                  initial = {rf_initial_tune},
                  param_info = rf_parameters,
                  control = control_sim_anneal(verbose = TRUE,
                                               no_improve = 100,
                                               restart = 8,
                                               radius = c(0.05, 0.15),
                                               flip = 0.75,
                                               cooling_coef = 0.05
                  ),
                  metrics = metric_set(rmse))
proc.time() - tempo

  # Q4: MODELAGEM: RF: OTIMIZAÇÃO HIPERPARÂMETROS: MÉTRICAS RESULTADO ----
rf_df_best_hyperparameters <- rf_final_tune %>% 
  show_best(metric = "rmse", n = 15)

rf_plot_hyperparameters <- autoplot(rf_final_tune) + 
  geom_smooth() +
  theme_bw()

rf_best_hyperparameters <- 
  rf_final_tune %>% 
  select_best(metric = "rmse")

  # Q4: MODELAGEM: RF: FINALIZAR FLUXO DE TRABALHO ----
rf_best_model <-
  rf_workflow %>%
  finalize_workflow(rf_best_hyperparameters)

  # Q4: MODELAGEM: RF: IMPORTÂNCIA DE VARIÁVEIS ----
rf_var_imp <- rf_best_model %>%
  fit(data = df_training) %>%
  pull_workflow_fit() %>%
  vip() +
  theme_bw()


  # Q4: MODELAGEM: RF: FIT & CONFUSION MATRIX ----
rf_fit <- rf_best_model %>%
  last_fit(df_split)

rf_test_predictions <- 
  rf_fit %>%
  collect_predictions()

autoplot(rf_test_predictions)

# Q5 ----
