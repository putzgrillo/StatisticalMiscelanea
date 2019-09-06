# varFixas sempre entram no cluster (inserir vetor com nome variáveis)
# varCombn são testadas (inserir vetor com nome variáveis)
# percPior refere-se ao critério de clusters escolhidos
      # se percPior for percentual, retorna-se os cluster que estão percPior % pior em relação à silhueta do melhor
      # se percPior for inteiro, retorna-se os percPior melhores clusters
# ex: percPior 0.03, traz-se os clusters que tem silhueta 97% ou mais em relação à máxima
# intK é o intervalo do número de clusters a testar
# paralelo para determinar se será paralelizado. funciona apenas pra linux
# nucleos determina a quantidade de nucleos a usar caso empregue-se processamento paralelo
funcaoEscolherClusters <- function(df, varFixasNm, varCombnNm, percPior = 0.03, intK = 3:12, paralelo = FALSE, nucleos = 4) {
  varFixas <- which(colnames(df) %in% varFixasNm)
  varCombn <- which(colnames(df) %in% varCombnNm)
  # GERAR COMBINAÇÕES A TESTAR ----
  vetor <- varCombn
  combVariaveis <- lapply(seq_along(vetor), function (x) combinat::combn(vetor, x, simplify = FALSE)) %>%
    unlist(., recursive = FALSE) %>%
    lapply(., function (x) {c(varFixas, x)}) %>%
    append(., list(varFixas))
  
  # APLICAR COMBINAÇÕES ----
  if (paralelo) {
    library(foreach)
    library(doMC)
    clParalelo <- registerDoMC(nucleos)
    retornoClusters <- vector("list", length(combVariaveis))
    retornoClusters <- foreach(w = seq_along(combVariaveis)) %dopar% {
      funcaoCalcularClusters(x = df[, combVariaveis[[w]] ], kTest = intK, idClust = paste0(combVariaveis[[w]], collapse = "|"), seed = seed)
    }
  } else {
    retornoClusters <- lapply(combVariaveis, function(x) 
      funcaoCalcularClusters(x = df[, x], kTest = intK, idClust = paste0(x, collapse = "|"), seed = seed)
    )
  }

  # ESCOLHER MELHORES ----
  comparacaoClusters <- lapply(retornoClusters, function(x) {x$Sumario}) %>%
    do.call(rbind, .)
  # CLUSTERS QUE IRÃO PARA O RELATÓRIO (OS ESCOLHIDOS) ----
  dfClusters <- comparacaoClusters
  ePercentual <- ifelse(percPior > 0 & percPior <= 1, TRUE, FALSE)
  
  if (ePercentual) {
    clustersParaRelatorio <- comparacaoClusters %>%
      arrange(-SilhuetaMax) %>%
      filter(
        SilhuetaMax > (max(SilhuetaMax) * (1 - percPior) )                                        # CRITÉRIO POR PROXIMIDADE % DO MELHOR CLUSTER 
      ) %>%
      mutate(Escolhido = "SIM")
  } else {
    clustersParaRelatorio <- comparacaoClusters %>%
      top_n(percPior, wt = SilhuetaMax) %>%                                                                        # N PRIMEIROS CLUSTERS
      arrange(-SilhuetaMax) %>%
      mutate(Escolhido = "SIM")
  }
  

  # CATEGORIZAÇÃO DE CADA OBSERVAÇÃO PELOS CLUSTERS ESCOLHIDOS ----
  resultadosClusters <- lapply(retornoClusters, function(x) {x$Clusters}) %>%
    do.call(rbind, .) %>%
    filter(
      largSil %in% unique(clustersParaRelatorio$SilhuetaMax) &
      Iteracao %in% unique(clustersParaRelatorio$Iteracao) &
      Modelo %in% unique(clustersParaRelatorio$Cluster)
    )
  resultadosClusters <- split(resultadosClusters, 
                              f = list(resultadosClusters$Iteracao, resultadosClusters$largSil, resultadosClusters$Modelo),
                              sep = "<>")
  posUsar <- unlist(lapply(resultadosClusters, nrow)) > 0
  resultadosClusters <- resultadosClusters[posUsar]
  # SCATTER 3D CLUSTERS CANDIDATOS ----

  comparacaoClusters <- merge(comparacaoClusters, clustersParaRelatorio, all.x = TRUE) %>%
    replace(is.na(.), "NAO") %>%
    mutate(Escolhido = as.factor(Escolhido))
  

  # TRAZER RESULTADO ----
  resultado <- list(
    dfSumarioClusters = clustersParaRelatorio,
    dfTodosCandidatos = comparacaoClusters,
    dfClusterObservacoes = resultadosClusters,
    dfUtilizado = df,
    dfMetricaClusters = dfClusters
  )
  return(resultado)
}
