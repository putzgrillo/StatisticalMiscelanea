# https://stats.stackexchange.com/questions/195446/choosing-the-right-linkage-method-for-hierarchical-clustering
# OBS: NECESSÁRIO EVOLUIR PARA UM CRITÉRIO QUE PENALIZE QUANTIDADE DE CLUSTERS
library(data.table)
library(plotly)
library(tidyverse)
library(cluster)
library(purrr)
library(gridExtra)
library(GGally)
library(ggfortify)
library(dendextend)
library(combinat)
library(caret)
library(reshape2)
rm(list = ls())
setwd('Z:\\Publico\\zBruno Grillo\\Cluster')
# FUNÇÕES PRE-DEFINIDAS ----
source('Scripts\\calcularCluster.R')
source('Scripts\\escolherCluster.R')

# STRINGS ----
intTestar <- 3:9
seed <- 15081991
options(encoding = "UTF-8")

# BASE ----
clnts <- readRDS("cdClntCluster.rds")
baseR <- fread("Cluster Full 2018.csv")
baseR <- merge(clnts, baseR, all.x = TRUE) %>%
  arrange(Ordem) %>%
  select(-Ordem)
baseR <- baseR %>%
  replace(is.na(.), 0) %>%
  mutate(
    # TRANSFORMAÇÃO EXPONENCIAL 
    LOG_RECEITA_COMPRA_ON_COM_JUROS = log(RECEITA_COMPRA_ON_COM_JUROS + 1),
    LOG_VALOR_COMPRA_OFF = log(VALOR_COMPRA_OFF + 1),
    LOG_VALOR_COMPRA_ON = log(VALOR_COMPRA_ON + 1),
    LOG_RECEITA_COMPRA_OFF = log(RECEITA_COMPRA_OFF + 1),
    LOG_RECEITA_COMPRA_OFF_COM_JUROS = log(RECEITA_COMPRA_OFF_COM_JUROS + 1),
    LOG_RECEITA_SQRAP = log(RECEITA_SQRAP + 1),
    LOG_RECEITA_SAQUE_CARTAO = log(RECEITA_SAQUE_CARTAO + 1),
    LOG_VALOR_JUROS_OFF = log(VALOR_JUROS_OFF + 1),
    LOG_VALOR_TARIFAS_OFF = log(VALOR_TARIFAS_OFF + 1),
    LOG_VALOR_REFINANCIAMENTO_OFF = log(VALOR_REFINANCIAMENTO_OFF + 1),
    LOG_RECEITA_TOTAL = log(RECEITA_TOTAL + 1),
    # PROPORCOES
    PROP_INADIMP = (VALOR_REFINANCIAMENTO_OFF + VALOR_JUROS_OFF) / (RECEITA_TOTAL + 1)
  ) %>%
  replace(is.na(.), 0) 

baseR <- within(baseR, {
  TipoUso = ifelse(
    VALOR_COMPRA_ON > 0 & VALOR_COMPRA_OFF == 0, "Apenas On",
    ifelse(
      VALOR_COMPRA_ON == 0 & VALOR_COMPRA_OFF > 0, "Apenas Off",
      "On e Off")
  )
})

# BASE: AMOSTRA ----
# set.seed(seed)
# obsAmostra <- c(sample(seq(nrow(baseR)), size = 5000), 30001)
# baseCluster <- baseR[obsAmostra, ]
# 
# baseCluster <- readRDS("v2.rds")

baseCluster <- baseR

funcaoRangeScale <- function(x) {
  resultado <- (x-min(x))/(max(x)-min(x))
return(resultado)
}


# BASE CLUSTER ----
fixas <- c("LOG_RECEITA_COMPRA_ON_COM_JUROS", "LOG_RECEITA_COMPRA_OFF", "LOG_RECEITA_COMPRA_OFF_COM_JUROS", "LOG_RECEITA_SQRAP")
variaveis <- c("LOG_RECEITA_SAQUE_CARTAO", "LOG_VALOR_JUROS_OFF" , "LOG_VALOR_TARIFAS_OFF", "LOG_VALOR_REFINANCIAMENTO_OFF")

  # BASE CLUSTER: AJUSTAR ESCALA DAS VARIÁVEIS
variaveisTransformar <- c(fixas, variaveis)
for (w in seq_along(variaveisTransformar)) {
  refCol <- which(colnames(baseCluster) == variaveisTransformar[w])
  baseCluster[, refCol] <- funcaoRangeScale(x = baseCluster[, refCol])
}

# RESULTADOS CLUSTERS ----
tempo <- proc.time()
clusters <- funcaoEscolherClusters(df = baseCluster, percPior = 0.05, intK = 4:9,
  varFixasNm = fixas, varCombnNm = variaveis)
proc.time() - tempo

save(clusters, file = "clusterApp.Rdata")
