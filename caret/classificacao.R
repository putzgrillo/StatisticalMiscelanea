obsTreino <- sample(x = seq(nrow(dfTreino)), size = 5000)
dfTreinar <- dfTreino[obsTreino, ]
dfTestar <- dfTreino[-obsTreino, ]

# MODELO: CONTROLES ----
controle <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 10,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)
# MODELO: MODELO ----
tempo <- proc.time()
modeloRf <- train(
  pgtoTotal ~ .,
  data = dfTreinar, 
  trControl = controle,
  method = "rf",
  metric = "ROC"#,
  #preProcess = "BoxCox"
)
proc.time() - tempo

# MODELO: IMPORTÂNCIA DAS VARIÁVEIS ----
importanciaVariaveis <- varImp(modeloRf)
plot(importanciaVariaveis)

# MODELO: MATRIZ DE CONFUSÃO ----
probsTest <- predict(modeloRf, dfTestar, type = "prob")
threshold <- 0.5
pred      <- factor( ifelse(probsTest[, "TOTAL"] > threshold, "TOTAL", "NAO_TOTAL") )
confusionMatrix(pred, as.factor(dfTestar$pgtoTotal))
