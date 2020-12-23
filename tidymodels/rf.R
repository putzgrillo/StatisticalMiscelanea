library(tidyverse)
library(tidymodels)
library(finetune)
library(vip)
library(themis)

# DATASETs ----
  # SPLIT BETWEEN 
df_split <- initial_split(dfRR, prop = 4/5, strata = Y)

df_training <- training(df_split)
df_test <- testing(df_split)

  # RESAMPLING
df_folds <- df_training %>%
  vfold_cv(v = 10,
           repeats = 1,
           strata = Y)

# PRE PROCESSING ----
rf_recipe <- 
  recipe(Y ~ ., data = dfRR) %>% 
  update_role(
    ID_CONTA, ID_FATURA, DT_VENCIMENTO, ID_STATUSCONTA, FL_EMITE_EXTRATO,
    new_role = "ID"
  ) %>%
  step_dummy(all_nominal(), -has_role("ID"), -all_outcomes()) %>%
  # step_YeoJohnson(all_numeric(), -has_role("ID")) %>%
  # step_BoxCox(all_numeric(), -has_role("ID")) %>%
  # step_normalize(all_numeric(), -has_role("ID")) %>%
  # step_pca(all_predictors(), num_comp = tune()) %>%              # PCA
  themis::step_downsample(Y)


# MODEL
cores <- floor(parallel::detectCores() /3)

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
  set_mode("classification") 

rf_workflow <- 
  workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(rf_recipe)

# TUNING HYPERPARAMETERS 
    # HYPERPARAMETERS' RANGE
rf_parameters <- rf_workflow %>%
  parameters() %>%
  update(
    # num_comp = num_comp(range = c(0L, 20L)),                   # PCA
    mtry = mtry(range = c(3L, 15L)),
    trees = trees(range = c(1000L, 2000L)),
    min_n = min_n(range = c(10L, 200L))
  )

    # INITIAL TUNE OF HYPERPARAMETERS (BAYESIAN)
tempo <- proc.time()
rf_initial_tune <- rf_workflow %>%
  tune_bayes(object = .,
                  resamples = df_folds,
                  initial = 5 ** length(rf_model$args),
                  param_info = rf_parameters,
                  control = control_bayes(save_pred = TRUE),
                  metrics = metric_set(roc_auc))
proc.time() - tempo

    # SIMULATED ANNEALING TUNE
tempo <- proc.time()
rf_final_tune <- rf_workflow %>%
  tune_sim_anneal(object = .,
                  resamples = df_folds,
                  iter = 10,
                  initial = rf_initial_tune,
                  param_info = rf_parameters,
                  control = control_sim_anneal(verbose = TRUE,
                                               no_improve = 100,
                                               restart = 8,
                                               radius = c(0.05, 0.15),
                                               flip = 0.75,
                                               cooling_coef = 0.05
                                               ),
                  metrics = metric_set(roc_auc))
proc.time() - tempo



rf_final_tune %>% 
  show_best(metric = "roc_auc")

autoplot(rf_final_tune)

rf_best <- 
  rf_final_tune %>% 
  select_best(metric = "roc_auc")

rf_final_model <-
  rf_workflow %>%
  finalize_workflow(rf_best)

# IMPORTÂNCIA DE VARIÁVEIS
rf_final_model %>%
  fit(data = df_training) %>%
  pull_workflow_fit() %>%
  vip()



# PREVISÃO E AVALIAÇÃO NO TESTE 
rf_fit <- rf_final_model %>%
  last_fit(df_split)

rf_test_predictions <- 
  rf_fit %>%
  collect_predictions()


rf_test_predictions %>%
  conf_mat(truth = Y, estimate = .pred_class)



# MODELO PARA OUTRAS PREVISÕES
final_model <- fit(rf_final_model, dfRR)

lala <- predict(final_model, dfR)
caret::confusionMatrix(table(data.frame(REAL = dfR$Y, PREVISTO = lala$.pred_class)))
