library(tidyverse)
library(tidymodels)
library(finetune)
library(vip)
library(themis)
library(rlang)
library(xgboost)
library(vctrs)

# STRINGS ----
cores <- floor(parallel::detectCores() / 6)
n_initial_parameters <- 5                           # PARÂMETROS_INICIAIS = n_parametros * n_initial_parameters

# DIRETÓRIO E DADOS ----
setwd("Z:\\Publico\\zBruno Grillo\\Meu Cartao\\Precificação Parcelamento")
setwd("C:\\Users\\00690\\Desktop\\Precificação Parcelamento")

dfR <- readRDS("db\\baseTreinoModelo.rds")
# SELECIONAR APENAS QUEM JÁ POSSUI TRÊS MESES
dfR <- dfR %>%
  ungroup() %>%
  filter(
    !is.na(VL_PGTO_MEDIA)
    & !is.na(VL_PGTO_ULTIMA)
    & !is.na(VL_PGTO_ATUAL)
  ) %>%
  mutate(
    Y = as.factor(ifelse(TIPO_PARCELAMENTO == "NAO_ADERIU", "SEM_PARCELAMENTO", "COM_PARCELAMENTO"))
    ) %>%
  replace(is.na(.), 0)


dfR <- dfR %>%
  select(
    -TIPO_PGTO, -TIPO_PARCELAMENTO, -nDefasagens, -PROP_PGTO, PROP_PGTO_ULTIMA, PROP_PGTO_MEDIA, 
    DIF_PROP_PGTO_ULTIMA_MEDIA, -VL_PGTO_ATUAL) %>%
  group_by(Y) %>%
  sample_n(30000)

set.seed(15081991)

# MARCAÇÃO TEMPO ----
t_total <- proc.time()
# DATASETs ----
  # SPLIT BETWEEN 
df_split <- initial_split(dfR, prop = 4/5, strata = Y)

df_training <- training(df_split)
df_test <- testing(df_split)

  # RESAMPLING
df_folds <- df_training %>%
  vfold_cv(v = 10,
           repeats = 1,
           strata = Y)
####
####
# RANDOM FORREST ----
  # RANDOM FORREST: PRE PROCESSING (RECIPE) ----
rf_recipe <- 
  recipe(Y ~ ., data = df_training) %>% 
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


  # RANDOM FORREST: MODEL & WORKFLOW ----
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

  # RANDOM FORREST: TUNING HYPERPARAMETERS ----
    # RANDOM FORREST: TUNING HYPERPARAMETERS: RANGE ----
rf_parameters <- rf_workflow %>%
  parameters() %>%
  update(
    # num_comp = num_comp(range = c(0L, 20L)),                   # PCA
    mtry = mtry(range = c(3L, 15L)),
    trees = trees(range = c(1000L, 2000L)),
    min_n = min_n(range = c(10L, 200L))
  )

    # RANDOM FORREST: TUNING HYPERPARAMETERS: INITIAL TUNE (BAYESIAN) ----
tempo <- proc.time()
rf_initial_tune <- rf_workflow %>%
  tune_bayes(object = .,
                  resamples = df_folds,
                  initial = n_initial_parameters * length(rf_model$args),
                  param_info = rf_parameters,
                  control = control_bayes(save_pred = TRUE),
                  metrics = metric_set(roc_auc))
proc.time() - tempo

    # RANDOM FORREST: TUNING HYPERPARAMETERS: SIMULATED ANNEALING TUNE ----
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


    # RANDOM FORREST: MÉTRICAS DE HYPERPARAMETERS ----
rf_df_best_hyperparameters <- rf_final_tune %>% 
  show_best(metric = "roc_auc", n = 15)

rf_plot_hyperparameters <- autoplot(rf_final_tune)

rf_best_hyperparameters <- 
  rf_final_tune %>% 
  select_best(metric = "roc_auc")

  # RANDOM FORREST: FINALIZAR FLUXO DE TRABALHO ----
rf_best_model <-
  rf_workflow %>%
  finalize_workflow(rf_best_hyperparameters)

  # RANDOM FORREST: IMPORTÂNCIA DE VARIÁVEIS ----
rf_var_imp <- rf_best_model %>%
  fit(data = df_training) %>%
  pull_workflow_fit() %>%
  vip()

  # RANDOM FORREST: FIT & CONFUSION MATRIX ----
rf_fit <- rf_best_model %>%
  last_fit(df_split)

rf_test_predictions <- 
  rf_fit %>%
  collect_predictions()

rf_conf_matrix <- rf_test_predictions %>%
  conf_mat(truth = Y, estimate = .pred_class)

rf_conf_matrix_metrics <- summary(rf_conf_matrix)


  # RANDOM FORREST: MODELO FINAL PARA PREVISÕES NEWDATA ----
rf_final_model <- fit(rf_best_model, dfR)

# rf_predict <- predict(rf_final_model, dfR)





#####
#####
# GRADIENT BOOSTING ----
  # GRADIENT BOOSTING: PRE PROCESSING (RECIPE) ----
xgb_recipe <- 
  recipe(Y ~ ., data = df_training) %>% 
  update_role(
    ID_CONTA, ID_FATURA, DT_VENCIMENTO, ID_STATUSCONTA, FL_EMITE_EXTRATO,
    new_role = "ID"
  ) %>%
  step_dummy(all_nominal(), -has_role("ID"), -all_outcomes()) %>%
  step_YeoJohnson(all_numeric(), -has_role("ID")) %>%
  # step_BoxCox(all_numeric(), -has_role("ID")) %>%
  step_normalize(all_numeric(), -has_role("ID")) %>%
  # step_pca(all_predictors(), num_comp = tune()) %>%              # PCA
  themis::step_downsample(Y)



  # GRADIENT BOOSTING: MODEL & WORKFLOW ----
xgb_model <- 
  boost_tree(
    mtry = tune(),
    trees = tune(),
    min_n = tune(),
    tree_depth = tune(),
    learn_rate = tune(),
    loss_reduction = tune()
  ) %>% 
  set_engine(
    "xgboost", 
    num.threads = cores, 
    importance = "impurity"
  ) %>% 
  set_mode("classification") 

xgb_workflow <- 
  workflow() %>% 
  add_model(xgb_model) %>% 
  add_recipe(xgb_recipe)


  # GRADIENT BOOSTING: TUNING HYPERPARAMETERS ----
    # GRADIENT BOOSTING: TUNING HYPERPARAMETERS: RANGE ----
xgb_parameters <- xgb_workflow %>%
  parameters() %>%
  update(
    mtry = mtry(range = c(10L, 50L)),
    trees = trees(range = c(1000L, 2000L)),
    min_n = min_n(range = c(10L, 200L)),
    tree_depth = tree_depth(range = c(3L, 25L)),
    learn_rate = learn_rate(range = c(-5,-1)),
    loss_reduction = loss_reduction(range = c(0, 2))
  )

    # GRADIENT BOOSTING: TUNING HYPERPARAMETERS: INITIAL TUNE (BAYESIAN) ----
tempo <- proc.time()
xgb_initial_tune <- xgb_workflow %>%
  tune_bayes(object = .,
             resamples = df_folds,
             initial = n_initial_parameters * length(xgb_model$args),
             param_info = xgb_parameters,
             control = control_bayes(save_pred = TRUE),
             metrics = metric_set(roc_auc))
proc.time() - tempo

    # GRADIENT BOOSTING: TUNING HYPERPARAMETERS: SIMULATED ANNEALING TUNE ----
tempo <- proc.time()
xgb_final_tune <- xgb_workflow %>%
  tune_sim_anneal(object = .,
                  resamples = df_folds,
                  iter = 10,
                  initial = xgb_initial_tune,
                  param_info = xgb_parameters,
                  control = control_sim_anneal(verbose = TRUE,
                                               no_improve = 100,
                                               restart = 8,
                                               radius = c(0.05, 0.15),
                                               flip = 0.75,
                                               cooling_coef = 0.05
                  ),
                  metrics = metric_set(roc_auc))
proc.time() - tempo


    # GRADIENT BOOSTING: MÉTRICAS DE HYPERPARAMETERS ----
xgb_df_best_hyperparameters <- xgb_final_tune %>% 
  show_best(metric = "roc_auc", n = 15)

xgb_plot_hyperparameters <- autoplot(xgb_final_tune)

xgb_best_hyperparameters <- 
  xgb_final_tune %>% 
  select_best(metric = "roc_auc")


  # GRADIENT BOOSTING: FINALIZAR FLUXO DE TRABALHO ----
xgb_best_model <-
  xgb_workflow %>%
  finalize_workflow(xgb_best_hyperparameters)

  # GRADIENT BOOSTING: IMPORTÂNCIA DE VARIÁVEIS ----
xgb_var_imp <- xgb_best_model %>%
  fit(data = df_training) %>%
  pull_workflow_fit() %>%
  vip()

  # GRADIENT BOOSTING: FIT & CONFUSION MATRIX ----
xgb_fit <- xgb_best_model %>%
  last_fit(df_split)

xgb_test_predictions <- 
  xgb_fit %>%
  collect_predictions()

xgb_conf_matrix <- xgb_test_predictions %>%
  conf_mat(truth = Y, estimate = .pred_class)

xgb_conf_matrix_metrics <- summary(xgb_conf_matrix)


  # GRADIENT BOOSTING: MODELO FINAL PARA PREVISÕES NEWDATA ----
xgb_final_model <- fit(xgb_best_model, dfR)

xgb_predict <- predict(xgb_final_model, dfR)


#####
#####
# NEURAL NETWORKS ---- 
  # NEURAL NETWORKS: PRE PROCESSING (RECIPE) ----
nn_recipe <- 
  recipe(Y ~ ., data = df_training) %>% 
  update_role(
    ID_CONTA, ID_FATURA, DT_VENCIMENTO, ID_STATUSCONTA, FL_EMITE_EXTRATO,
    new_role = "ID"
  ) %>%
  step_dummy(all_nominal(), -has_role("ID"), -all_outcomes()) %>%
  step_YeoJohnson(all_numeric(), -has_role("ID")) %>%
  # step_BoxCox(all_numeric(), -has_role("ID")) %>%
  step_normalize(all_numeric(), -has_role("ID")) %>%
  # step_pca(all_predictors(), num_comp = tune()) %>%              # PCA
  themis::step_downsample(Y)



  # NEURAL NETWORKS: MODEL & WORKFLOW ----
nn_model <- 
  mlp(
    hidden_units = tune(),
    penalty = tune(),
    epochs = tune(),
    # dropout = tune(),
    # activation = "softmax"
  ) %>% 
  set_engine(
    "nnet", 
    num.threads = cores, 
    importance = "impurity"
  ) %>% 
  set_mode("classification") 

nn_workflow <- 
  workflow() %>% 
  add_model(nn_model) %>% 
  add_recipe(nn_recipe)


  # NEURAL NETWORKS: TUNING HYPERPARAMETERS ----
    # NEURAL NETWORKS: TUNING HYPERPARAMETERS: RANGE ----
nn_parameters <- nn_workflow %>%
  parameters() %>%
  update(
    hidden_units = hidden_units(range = c(1L, 10L)),
    penalty = penalty(range = c(-10, 0)),
    epochs = epochs(range = c(10L, 1000L)),
    # dropout = dropout(range = c(0, 1))
  )

    # NEURAL NETWORKS: TUNING HYPERPARAMETERS: INITIAL TUNE (BAYESIAN) ----
tempo <- proc.time()
nn_initial_tune <- nn_workflow %>%
  tune_bayes(object = .,
             resamples = df_folds,
             initial = n_initial_parameters * length(nn_model$args),
             param_info = nn_parameters,
             control = control_bayes(save_pred = TRUE),
             metrics = metric_set(roc_auc))
proc.time() - tempo

    # NEURAL NETWORKS: TUNING HYPERPARAMETERS: SIMULATED ANNEALING TUNE ----
tempo <- proc.time()
nn_final_tune <- nn_workflow %>%
  tune_sim_anneal(object = .,
                  resamples = df_folds,
                  iter = 10,
                  initial = nn_initial_tune,
                  param_info = nn_parameters,
                  control = control_sim_anneal(verbose = TRUE,
                                               no_improve = 100,
                                               restart = 8,
                                               radius = c(0.05, 0.15),
                                               flip = 0.75,
                                               cooling_coef = 0.05
                  ),
                  metrics = metric_set(roc_auc))
proc.time() - tempo


    # NEURAL NETWORKS: MÉTRICAS DE HYPERPARAMETERS ----
nn_df_best_hyperparameters <- nn_final_tune %>% 
  show_best(metric = "roc_auc", n = 15)

nn_plot_hyperparameters <- autoplot(nn_final_tune)

nn_best_hyperparameters <- 
  nn_final_tune %>% 
  select_best(metric = "roc_auc")

  # NEURAL NETWORKS: FINALIZAR FLUXO DE TRABALHO ----
nn_best_model <-
  nn_workflow %>%
  finalize_workflow(nn_best_hyperparameters)

  # NEURAL NETWORKS: IMPORTÂNCIA DE VARIÁVEIS ----
nn_var_imp <- nn_best_model %>%
  fit(data = df_training) %>%
  pull_workflow_fit() %>%
  vip()

  # NEURAL NETWORKS: FIT & CONFUSION MATRIX ----
nn_fit <- nn_best_model %>%
  last_fit(df_split)

nn_test_predictions <- 
  nn_fit %>%
  collect_predictions()

nn_conf_matrix <- nn_test_predictions %>%
  conf_mat(truth = Y, estimate = .pred_class)

nn_conf_matrix_metrics <- summary(nn_conf_matrix)


  # NEURAL NETWORKS: MODELO FINAL PARA PREVISÕES NEWDATA ----
nn_final_model <- fit(nn_best_model, dfR)

nn_predict <- predict(nn_final_model, dfR)



####
####


# COMPARAR RESULTADO DOS DIFERENTES MODELOS ----
proc.time() - t_total

# SALVAR LS ----
save.image("ls_competicao.RData")

