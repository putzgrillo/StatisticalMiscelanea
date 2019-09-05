# MODELAGEM ----
  # MODELAGEM: FUNÇÃO MODELO ----
funcaoModelo <- function(df, nTam = 0.8, nMax = 2500) { # RETORNA MODELO E ESTATÍSTICAS DOS RESÍDUOS
  library(tidyverse)
  library(caret)
  # FUNÇAO MODELO: AJUSTE BASE PARA MODELO ----
  baseModelo <- df %>% 
    as.data.frame %>%
    select(-c(Classificador, Chave.Atendimento)) %>%
    mutate(
      Produto = as.factor(Produto),
      Sexo.Beneficiario = as.factor(Sexo.Beneficiario),
      ClassificacaoPrestador = as.factor(ClassificacaoPrestador),
      logCusto = log(Custo)
    )
  
    # DETERMINAR VARIAVEIS A EMPREGAR ----
  variaveisUsar <- baseModelo %>%
    select(-c(Custo, logCusto)) %>%
    lapply(., nlevels) %>%
    unlist %>%
    {names(.)[. != 1]} # pegar apenas explicativas 
  
    # FUNÇAO MODELO: AMOSTRAGEM ----
  nLin <- nrow(baseModelo)
  nAmostra <- ifelse(nLin * 0.8 > nMax, nMax, nLin * 0.8)
  refTreino <- sample(seq(nLin), size = nAmostra)
  baseModeloTreino <- baseModelo[refTreino, ]
  baseModeloTeste <- baseModelo[-refTreino, ]
  
    # FUNÇAO MODELO: CONTROLES ----
  controle <- trainControl(
    method = "repeatedcv",
    number = 5,
    repeats = 2
  )
    # FUNÇAO MODELO: MODELOS (LOG E NIVEL) ----
  modeloRF <- train(as.formula(paste("Custo ~", paste(variaveisUsar, collapse = " + "))),
                    data = baseModeloTreino,
                    method = "rf",
                    metric = "RMSE"
  )
  modeloRFlog <- train(as.formula(paste("logCusto ~", paste(variaveisUsar, collapse = " + "))),
                    data = baseModeloTreino,
                    method = "rf",
                    metric = "RMSE"
  )
  # FUNÇAO MODELO: ESCOLHA MÉTODO VENCEDOR ----
    # RESIDUOS
  comparacaoResiduos <- data.frame(
    baseModeloTeste,
    yHatNivel = predict(modeloRF, baseModeloTeste),
    yHatLog = exp(predict(modeloRFlog, baseModeloTeste)) * exp(var(residuals(modeloRFlog))/2)
  ) %>%
    mutate(
      uHatNivel = yHatNivel - Custo,
      uHatLog = yHatLog - Custo
    ) 
    # COMPARAÇAO
  erroMedioAbs <- data.frame(
    Nivel = mean(abs(comparacaoResiduos$uHatNivel)), 
    Log = mean(abs(comparacaoResiduos$uHatLog))
  )
  # FUNÇAO MODELO: RESULTADO ----
  if (erroMedioAbs$Log < erroMedioAbs$Nivel) {
    resultado <- list(
      modeloFinal = modeloRF,
      modeloLog = TRUE,
      dfAnaliseResidual = {
        comparacaoResiduos %>%
          select(-c(yHatLog, uHatLog))  
      }
    )
  } else {
    resultado <- list(
      modeloFinal = modeloRFlog,
      modeloLog = FALSE,
      dfAnaliseResidual = {
        comparacaoResiduos %>%
          select(-c(yHatNivel, uHatNivel))  
      }
    )
  }
return(resultado)
}
cl <- makeCluster(detectCores())
clusterExport(cl, "funcaoModelo")
listaModelos <- parLapply(cl, baseLista, function(x) funcaoModelo(df = x))
stopCluster(cl)
