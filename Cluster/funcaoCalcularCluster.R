# CLUSTER: FUNÇÃO COMPARAR CLUSTERS ----
funcaoCalcularClusters <- function(x, idClust, kTest = 3:12, seed = 15081991) {
  # CLUSTERS CANDIDATOS ----
  
  # CLUSTER: K-MEANS ----
  largSilKM <- map_dbl(kTest,  function(k){
    set.seed(seed)
    modelo <- kmeans(x = x, centers = k)
    silhueta <- silhouette(modelo$cluster, dist(x))
    mean(silhueta[, 3])
  }) %>%
    round(., 4)
  set.seed(seed)
  modeloKM <- kmeans(x = x, centers = kTest[which.max(largSilKM)])
  
  # CLUSTER: HIERARCHICAL CLUSTER: LINKAGE COMPLETE ----
  largSilHCC <- map_dbl(kTest,  function(k){
    silhuetaCompl <- x %>%
      dist(., method = 'euclidean') %>% # MATRIZ DISSIMILARIDADE
      hclust(., method = 'complete') %>%
      cutree(., k = k) %>%
      silhouette(., dist(x, method = 'euclidean'))
    mean(silhuetaCompl[, 3])
  }) %>%
    round(., 4)
  
  modeloHCC <- x %>%
    dist(method = 'euclidean') %>%
    hclust(method = 'complete') %>%
    cutree(k = kTest[which.max(largSilHCC)])
  
  
  # CLUSTER: HIERARCHICAL CLUSTER: LINKAGE CENTROIDE ----
  largSilHCW <- map_dbl(kTest,  function(k){
    silhuetaCompl <- x %>%
      dist(., method = 'euclidean') %>% # MATRIZ DISSIMILARIDADE
      hclust(., method = 'ward.D2') %>%
      cutree(., k = k) %>%
      silhouette(., dist(x, method = 'euclidean'))
    mean(silhuetaCompl[, 3])
  }) %>%
    round(., 4)
  
  modeloHCW <- x %>%
    dist(method = 'euclidean') %>%
    hclust(method = 'ward.D2') %>%
    cutree(k = kTest[which.max(largSilHCW)])
  
  # CLUSTER: HIERARCHICAL CLUSTER: LINKAGE AVERAGE ----
  largSilHCA <- map_dbl(kTest,  function(k){
    silhuetaCompl <- x %>%
      dist(., method = 'euclidean') %>% # MATRIZ DISSIMILARIDADE
      hclust(., method = 'average') %>%
      cutree(., k = k) %>%
      silhouette(., dist(x, method = 'euclidean'))
    mean(silhuetaCompl[, 3])
  }) %>%
    round(., 4)
  
  modeloHCA <- x %>%
    dist(method = 'euclidean') %>%
    hclust(method = 'average') %>%
    cutree(k = kTest[which.max(largSilHCA)])
  
  # CLUSTER: HIERARCHICAL CLUSTER: LINKAGE CORRELATION-BASED (NÃO APLICÁVEL DATASET) ----
  # baseClusterT <- t(baseCluster)
  # largSilHCCorrW <- map_dbl(kTest,  function(k){
  #   silhueta <- baseClusterT %>%
  #     t %>%
  #     cor(., method = 'pearson') %>%
  #     as.dist(1 - .) %>%
  #     hclust(., method = 'ward.D2') %>%
  #     cutree(., k = k) %>%
  #     silhouette(., dist(baseCluster, method = 'euclidean')) 
  #   mean(silhueta[, 3])
  # })
  # 
  # modeloHCCorrW <- baseCluster %>%
  #   dist(method = 'euclidean') %>%
  #   hclust(method = 'complete') %>%
  #   cutree(k = kTest[which.max(largSilHCCorrW)])
  
  
  # TABELA COM SILHUETAS ----
  dfSumario <- data.frame(
    Iteracao = idClust,
    Cluster = c("k-Means", "HC-Complete", "HC-Centroide", "HC-Average"),
    SilhuetaMax = c(max(largSilKM), max(largSilHCC), max(largSilHCW), max(largSilHCA)),
    NumClusters = c(
      kTest[which.max(largSilKM)], kTest[which.max(largSilHCC)],
      kTest[which.max(largSilHCW)], kTest[which.max(largSilHCA)]
    ),
    nVariaveis = ncol(x)
  )
  
  # TABELA COM CLUSTERS ----
  dfClusters <- rbind(
    data.frame(
      Iteracao = idClust, largSil = max(largSilKM), Modelo = "k-Means", Cluster = modeloKM$cluster),
    data.frame(
      Iteracao = idClust, largSil = max(largSilHCC), Modelo = "HC-Complete", Cluster = modeloHCC),
    data.frame(
      Iteracao = idClust, largSil = max(largSilHCW), Modelo = "HC-Centroide", Cluster = modeloHCW),
    data.frame(
      Iteracao = idClust, largSil = max(largSilHCA), Modelo = "HC-Average", Cluster = modeloHCA)
  )
  
  # GRÁFICO: SILHUETA EM FUNÇÃO DE K ----
  basePlotSilhueta <- rbind(
    data.frame(k = kTest, largSil = largSilKM, Modelo = 'k-Means'),
    data.frame(k = kTest, largSil = largSilHCC, Modelo = 'HC-Complete'),
    data.frame(k = kTest, largSil = largSilHCW, Modelo = 'HC-Centroide'),
    data.frame(k = kTest, largSil = largSilHCA, Modelo = 'HC-Average')
  )
  
  graficoSilhueta <- ggplot(basePlotSilhueta, aes(x = k, y = largSil, colour = Modelo)) +
    geom_line() +
    scale_x_continuous(breaks = kTest) + 
    labs(
      x = "K", y = "Largura Média Silhueta", 
      title = 'Largura Média da Silhueta em função do número de Clusters'
    ) + 
    theme(text = element_text(size = 20))
  
  
  # DENDOGRAMAS (DEIXAR DE FORA) ----
  #  dendogramaHCA <- x %>%
  #    dist(method = 'euclidean') %>%
  #    hclust(method = 'average') %>% 
  #    as.dendrogram %>% 
  #    color_branches(., k = kTest[which.max(largSilHCA)]) %>%
  #    plot
  
  # RESULTADO ----
  resultado <- list(
    Sumario = dfSumario, Clusters = dfClusters, grafSilhueta = graficoSilhueta
  )
  return(resultado)
}
