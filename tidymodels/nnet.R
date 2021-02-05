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

  # NEURAL NETWORKS: SALVAR OBJETOS RELACIONADOS (EXCETO DFR) ----
arquivos_neuralnet <- ls()
arquivos_neuralnet <- arquivos_neuralnet[grepl("nn_", arquivos_neuralnet)]

save(list = arquivos_neuralnet, file = "nn_objects.RData")
save(nn_final_model, file = "nn_modelo.RData")


