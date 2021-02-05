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

  # RANDOM FORREST: SALVAR OBJETOS RELACIONADOS (EXCETO DFR) ----
arquivos_random_forest <- ls()
arquivos_random_forest <- arquivos_random_forest[grepl("rf_", arquivos_random_forest)]

save(list = arquivos_random_forest, file = "rf_objects.RData")
save(rf_final_model, file = "rf_modelo.RData")

