# PRE PROCESSING
xgb_recipe <- 
  recipe(Y ~ ., data = dfRR) %>% 
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

# RESAMPLING
df_folds <- df_training %>%
  vfold_cv(v = 10,
           repeats = 1,
           strata = Y)

# MODEL
cores <- floor(parallel::detectCores() /3)

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

# TUNING HYPERPARAMETERS 
# HYPERPARAMETERS' RANGE
xgb_parameters <- xgb_workflow %>%
  parameters() %>%
  update(
    mtry = mtry(range = c(3L, 15L)),
    trees = trees(range = c(1000L, 2000L)),
    min_n = min_n(range = c(10L, 200L)),
    tree_depth = tree_depth(range = c(3L, 25L)),
    learn_rate = learn_rate(range = c(-5,-1)),
    loss_reduction = loss_reduction(range = c(0, 2))
  )

# INITIAL TUNE OF HYPERPARAMETERS (BAYESIAN)
tempo <- proc.time()
xgb_initial_tune <- xgb_workflow %>%
  tune_bayes(object = .,
             resamples = df_folds,
             initial = 2 ** length(xgb_model$args),
             param_info = xgb_parameters,
             control = control_bayes(save_pred = TRUE),
             metrics = metric_set(roc_auc))
proc.time() - tempo

# SIMULATED ANNEALING TUNE
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



nn_final_tune %>% 
  show_best(metric = "roc_auc")

autoplot(nn_final_tune)

nn_best <- 
  nn_final_tune %>% 
  select_best(metric = "roc_auc")

nn_final_model <-
  nn_workflow %>%
  finalize_workflow(nn_best)

# IMPORTÂNCIA DE VARIÁVEIS
nn_final_model %>%
  fit(data = df_training) %>%
  pull_workflow_fit() %>%
  vip()



# PREVISÃO E AVALIAÇÃO NO TESTE 
nn_fit <- nn_final_model %>%
  last_fit(df_split)

nn_test_predictions <- 
  nn_fit %>%
  collect_predictions()


nn_test_predictions %>%
  conf_mat(truth = Y, estimate = .pred_class)



# MODELO PARA OUTRAS PREVISÕES
final_model <- fit(nn_final_model, dfRR)

lala <- predict(final_model, dfR)
caret::confusionMatrix(table(data.frame(REAL = dfR$Y, PREVISTO = lala$.pred_class)))
