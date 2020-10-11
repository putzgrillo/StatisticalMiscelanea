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
setwd('')
# FUNÇÕES PRE-DEFINIDAS ----
source('Scripts\\calcularCluster.R')
source('Scripts\\escolherCluster.R')

# STRINGS ----
intTestar <- 3:9
seed <- 15081991
options(encoding = "UTF-8")

# BASE ----
baseR <- fread("df.csv")
baseR <- baseR %>%
  replace(is.na(.), 0) %>%
  mutate(
    # BOX-COX TRANSFORMATIONS
    # VARIABLE ENCODING
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
fixas <- c("VARIAVEIS_SEMPRE")
variaveis <- c("VARIAVEIS_TESTE")

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
