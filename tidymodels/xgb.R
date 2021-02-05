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

# xgb_predict <- predict(xgb_final_model, dfR)

  # GRADIENT BOOSTING: SALVAR OBJETOS RELACIONADOS (EXCETO DFR) ----
arquivos_gradient <- ls()
arquivos_gradient <- arquivos_gradient[grepl("xgb_", arquivos_gradient)]

save(list = arquivos_gradient, file = "xgb_objects.RData")
save(xgb_final_model, file = "xgb_modelo.RData")


