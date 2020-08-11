# LISTA COMPUTACIONAL ----
  # QUESTÃO 1: MÉTODO K-MEANS K = 2 POR ATUALIZAÇÃO EM BLOCOS ----
library(tidyverse)
                    # Q1: FUNÇÃO QUE CALCULA O CLUSTER A QUAL PERTENCE ----
calculadoraKmeans <- function(df, nClusters, variaveis) {
  tempo <- proc.time()
  clusterAtual <- vector("numeric", nrow(df))
  iteracoes <- 1
  quebra <- FALSE
  while (!all(clusterAtual == df$Cluster)) {
      # CONDIÇÃO QUEBRA ALGORITMO SE ALGUM CLUSTER FICAR VAZIO (CÒDIGO NÃO PREPARADO PARA LIDAR)
      if(n_distinct(df$Cluster) != nClusters) { 
        quebra <- TRUE
        break 
      }
      
      # ATUALIZAR CLUSTERS
      clusterAtual <- df$Cluster
      # CENTROIDE DOS CLUSTERS
      centroide <- df %>%
        group_by(Cluster) %>%
        summarise_all(list(mean)) 
  
      centroide <- split(centroide, f = centroide$Cluster)
      
      # DISTÂNCIA ENTRE CENTRÓIDES
      dfTemp <- df
      for (w in seq(nClusters)) {
        observacao <- dfTemp[, colnames(dfTemp) %in% variaveis]
        kEsimoCentroide <- centroide[[w]][, colnames(centroide[[w]]) %in% variaveis]
        distancia <- ( sweep(observacao, MARGIN = 2, as.numeric(kEsimoCentroide)) ) ** 2
        distancia <- apply(distancia, 1, sum)
        dfTemp[[paste("Distancia_", w, sep = "")]] <- distancia
      }
      
      # VERIFICAR MENOR DISTÂNCIA
      dfTemp <- dfTemp[, grepl("Distancia", colnames(dfTemp))]
      df$Cluster <- apply(dfTemp, 1, function(x) which.min(x))
      iteracoes <- iteracoes + 1
      distanciaTotal <- sum(apply(dfTemp, 1, function(x) min(x)))
  }
  
  difTempo <- proc.time() - tempo
  # RETORNAR RESULTADO 
  sumario <- data.frame(
    Observacoes = nrow(df), Variaveis = length(variaveis), nIteracoes = iteracoes, 
    funcaoOjetivo = distanciaTotal, tempoDecorrido = difTempo[3], Parou = quebra, stringsAsFactors = FALSE)
  
  resultado <- list(Cluster = df$Cluster, Sumario = sumario)
return(resultado)
}


                    # Q1: FUNÇÃO QUE SUMARIZA RODAGENS E DETERMINA O MELHOR CLUSTER ----
kMeansBloco <- function(X, colunas = NULL, k = 2, nSimulacoes = 500) {
  if (is.null(colunas)) {colunas <- colnames(X)}   # SE NÃO TIVER COLUNAS ATRIBUÍDAS, É TUDO

  df <- X[, colnames(X) %in% colunas]
  listaResultado <- vector("list", nSimulacoes)
  
  # APLICAR FUNÇÃO
  for (w in seq(nSimulacoes)) {
    df$Cluster <- sample(seq(k), size = nrow(df), replace = TRUE)
    listaResultado[[w]] <- calculadoraKmeans(df = df, nClusters = k, variaveis = colunas)
  }
  # ESTATÍSTICAS
  dfEstatisticas <- vector("list", length = nSimulacoes)
  for (w in seq(nSimulacoes)) {
    dfEstatisticas[[w]] <- listaResultado[[w]]$Sumario
  }
  dfEstatisticas <- bind_rows(dfEstatisticas)
  # MELHOR CLUSTER 
  X$Cluster <- listaResultado[[which.min(dfEstatisticas$funcaoOjetivo)]]$Cluster
  # 
  resultado <- list(X = X, Stats = dfEstatisticas)
return(resultado)
}

                    # Q1: EXEMPLO DE APLICAÇÃO ---- 
dfCluster <- mpg
resultadoQ1 <- kMeansBloco(X = dfCluster, colunas = c("cyl", "displ", "cty", "hwy"), k = 3)

