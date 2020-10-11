# LISTA COMPUTACIONAL ----
library(tidyverse)
  # QUESTÃO 1: MÉTODO K-MEANS K = 2 POR ATUALIZAÇÃO EM BLOCOS ----
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
kMeansBloco <- function(X, colunas = NULL, k = 2, nSimulacoes = 100) {
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


  # QUESTÃO 2: REGRESSÃO NÃO-LINEAR ----
                    # Q2: A: ----
df2 <- within(data.frame(x = runif(n = 200, min = 2, max = 40)), {y = 60 * exp(-0.05 * x) + rnorm(n = 200)})

                    # Q2: B: ----
            # GAUSS-NEWTON
gaussNewtonR <- function(f, J, X, Y, chuteInicial, convergencia = 0.00001, iteracaoMaxima = 50) {
  etapa <- 0
  x0 <- chuteInicial
  valoresParciais <- vector("list", iteracaoMaxima)
  continuar <- TRUE
  while( continuar ) {
    # b_(t+1) = b_(t) - (J^T * J)^(-1) * J^T * epsilon(f(b_(t)))
    x1 <- x0 - solve(t(J(parametros = x0, X = X, Y = Y)) %*% J(parametros = x0, X = X, Y = Y)) %*% t(J(parametros = x0, X = X, Y = Y)) %*% f(parametros = x0, X = X, Y = Y)
    # x1 <- x0 - MASS::ginv(t(J(parametros =x1 <- x0 - solve(t(J(parametros = x0, X = X, Y = Y)) %*% J(parametros = x0, X = X, Y = Y)) %*% t(J(parametros = x0, X = X, Y = Y)) %*% f(parametros = x0, X = X, Y = Y) x0, X = X, Y = Y)) %*% J(parametros = x0, X = X, Y = Y)) %*% t(J(parametros = x0, X = X, Y = Y)) %*% f(parametros = x0, X = X, Y = Y)
    etapa <- etapa + 1
    difErro <- sum(f(parametros = x1, X = X, Y = Y) ** 2) - sum(f(parametros = x0, X = X, Y = Y) ** 2)
    x0 <- x1
    continuar <- abs(difErro) > convergencia
    valoresParciais[[etapa]] <- x1
    if (etapa == iteracaoMaxima) {break}
  }
  resultado <- list(nIteracoes = etapa, parametros = matrix(x1, ncol = 1))
return(resultado)
}


# AVALIAÇÃO
funcaoOtimizar <- function(parametros, X, Y) {Y - parametros[1] * exp(parametros[2] * X)}  # f = y - b0* exp(b1*x)
jacobianaFuncaoOtimizar <- function(parametros, X, Y) {  
  lista <- list(
    db0 = -exp(parametros[2] * X),                                # df/db0 = -exp(b1*x)
    db1 =  -parametros[1] * exp(parametros[2] * X) * X            # df/db1 = -b0* exp(b1*x) * x
  )
  do.call(cbind, lista)
} 



gaussNewtonR(f = funcaoOtimizar, J = jacobianaFuncaoOtimizar, X = df2$x, Y = df2$y, chuteInicial = c(58, -0.01))
gaussNewtonR(f = funcaoOtimizar, J = jacobianaFuncaoOtimizar, X = df2$x, Y = df2$y, chuteInicial = c(5, 0.1))

chutes <- expand.grid(b0 = runif(n = 300, 0, 300), b1 = runif(300, -0.15, 0.15))
dfChutes <- vector("list", nrow(chutes))
for (w in seq_along(dfChutes)) {
  dfChutes[[w]] <- gaussNewtonR(f = funcaoOtimizar, J = jacobianaFuncaoOtimizar, X = df2$x, Y = df2$y, chuteInicial = as.numeric(chutes[w,]))
}

                    # Q2: C ----
              # APROVEITANDO O CÓDIGO DA LISTA 1
newtonRaphson <- function(f, g, chuteInicial, X, Y, erro = 0.00001, iteracaoMaxima = 5000, ...) {
  etapa <- 0
  x0 <- chuteInicial
  while( sum(abs(f(x0, X = X, Y = Y)) > erro) > 0 ) {
    x1 <- x0 - solve(g(x0, X = X, Y = Y)) %*% f(x0, X = X, Y = Y)
    etapa <- etapa + 1
    x0 <- x1
    if (etapa == iteracaoMaxima) {break}
  }
  resultado <- list(nIteracoes = etapa, raizes = matrix(x1, ncol = 1))
  return(resultado)
}

            # APLICAR NEWTON-RAPHSON
fNR <- function(parametros, X, Y) {
  c(
    -2*sum((Y - parametros[1]*exp(parametros[2]*X))*exp(parametros[2]*X)), 
    -2*sum((Y - parametros[1]*exp(parametros[2]*X))*X*parametros[1]*exp(parametros[2]*X))
  )
}

gNR <- function(parametros, X, Y) {
  matrix(c(
    sum(-2*exp(parametros[2]*X)*(-exp(parametros[2]*X))),
    sum(2*(-parametros[1]*X*exp(parametros[2]*X))*(-exp(parametros[2]*X))) + sum(2*(Y-parametros[1]*exp(parametros[2]*X))*(-X*exp(parametros[2]*X))),
    sum(2*(-parametros[1]*X*exp(parametros[2]*X))*(-exp(parametros[2]*X))) + sum(2*(Y-parametros[1]*exp(parametros[2]*X))*(-X*exp(parametros[2]*X))),
    sum(2*(-parametros[1]*X*exp(parametros[2]*X))*(-parametros[1]*X*exp(parametros[2]*X))) + sum(2*(Y-parametros[1]*exp(parametros[2]*X))*(-parametros[1]*X^2*exp(parametros[2]*X)))
  ), ncol = 2, byrow = TRUE)
}
  

newtonRaphson(f = fNR, g = gNR, X = df2$x, Y = df2$y, chuteInicial = c(55, -0.04))
newtonRaphson(f = fNR, g = gNR, X = df2$x, Y = df2$y, chuteInicial = c(58, -0.01))
newtonRaphson(f = fNR, g = gNR, X = df2$x, Y = df2$y, chuteInicial = c(5, 0.1))

  # QUESTÃO 3: 
  # QUESTÃO 3: ----    
                    # Q3: A: GERAR DADOS ----
df3 <- matrix(NA, nrow = 40, ncol = 3)
mu <- matrix(c(rep(0, 3), rep(3,3)), ncol = 2)
set.seed(15081991)
              # PESOS MISTURA
n_mu1 <- round(nrow(df3) * runif(1), digits = 0)
n_mu2 <- nrow(df3) - n_mu1

for (w in seq(ncol(df3))) {
  df3[,w] <- c(
    rnorm(n = n_mu1, mean = mu[w, 1], sd = 1),
    rnorm(n = n_mu2, mean = mu[w, 2], sd = 1)
  )
}

                    # Q3: B: SIMULATED ANNEALING ----
sAnnealing <- function(df, chuteInicial, pesos, iteracoes = 500) {
  listaIteracoes <- vector("list", iteracoes)
  chuteAtual <- chuteInicial
  comp <- length(chuteAtual)
          # CONFIGURAÇÃO INICIAL
  logVerossimilhanca <- sum(log(pesos[1]*apply(dnorm(sweep(df, MARGIN = 2, chuteAtual[,1])), 1, prod) + pesos[2]*apply(dnorm(sweep(df, MARGIN = 2, chuteAtual[,2])), 1, prod)))
  listaIteracoes[[1]] <- list(Parametros = chuteAtual, Stats = data.frame(logVerossimilhanca = logVerossimilhanca, aceita = FALSE))
  
  for (w in seq(2, iteracoes)) {
    temperatura <- 1 / log(1 + w)
    distancia <- 0.2 * sqrt(temperatura)
    energia <- matrix(runif(comp, min = -distancia, max = distancia), ncol = 2)
    
    chuteAtual <- listaIteracoes[[w-1]]$Parametros + energia
    u <- runif(1)
    logVerossimilhanca <- sum(log(pesos[1]*apply(dnorm(sweep(df, MARGIN = 2, chuteAtual[,1])), 1, prod) + pesos[2]*apply(dnorm(sweep(df, MARGIN = 2, chuteAtual[,2])), 1, prod)))
    
    prob <- min(exp((logVerossimilhanca - listaIteracoes[[w-1]]$Stats$logVerossimilhanca)/temperatura), 1)

    chuteAtual <- (u <= prob) * chuteAtual + (u > prob)*listaIteracoes[[w-1]]$Parametros
    logVerossimilhanca <- sum(log(pesos[1]*apply(dnorm(sweep(df, MARGIN = 2, chuteAtual[,1])), 1, prod) + pesos[2]*apply(dnorm(sweep(df, MARGIN = 2, chuteAtual[,2])), 1, prod)))
    
    listaIteracoes[[w]] <- list(Parametros = chuteAtual,  Stats = data.frame(logVerossimilhanca = logVerossimilhanca, aceita = (u <= prob)))
  }
  
        # AVALIAÇÃO
  dfAvaliacao <- do.call(rbind, lapply(listaIteracoes, function(x) x$Stats))
  posicao <- which.max(dfAvaliacao$logVerossimilhanca)
  
  dfPlot <- do.call(rbind, lapply(listaIteracoes, function(x) matrix(c(x$Parametros), nrow = 1) ) )
  
  resultado <- list(
    dfPlot = dfPlot,
    ParâmetrosFinal = listaIteracoes[[posicao]]$Parametros,
    lokLikelihood = max(dfAvaliacao$logVerossimilhanca)
  )

return(resultado)
}

sAnnealing(df = df3, chuteInicial = matrix(c(1, 1, 1, 2, 2, 2), ncol = 2), pesos = c(n_mu1, n_mu2) / sum(c(n_mu1, n_mu2)))

                    # Q3: C: EXPECTATION MAXIMIZATION ----
funcaoEM <- function(df, chuteInicial, nIteracoes = 500, convergencia = 0.00001) {
  listaIteracoes <- vector("list", nIteracoes)
  listaIteracoes[[1]] <- chuteInicial
  continuar <- TRUE 
  parametrosAtuais <- chuteInicial
  etapa <- 1
  while (continuar) {
    # EXPECTATION
    pi <- apply(dnorm(sweep(df, MARGIN = 2, parametrosAtuais[,1])), 1, prod) / (apply(dnorm(sweep(df, MARGIN = 2, parametrosAtuais[,1])), 1, prod) + apply(dnorm(sweep(df, MARGIN = 2, parametrosAtuais[,2])), 1, prod) )
    # MAXIMIZATION
    novosParametros <- matrix(c(
      apply(pi * df, 2, sum) / sum(pi),
      apply((1 - pi) * df, 2, sum) / sum((1-pi))
    ), ncol = 2)
    
    # STRINGS
    continuar <- sum(abs(novosParametros - parametrosAtuais) > convergencia) > 0
    etapa <- etapa + 1
    listaIteracoes[[etapa]] <- novosParametros
    parametrosAtuais <- novosParametros
    
    if( etapa > nIteracoes) {break}
  }
  
  resultado <- list(ParâmetrosFinal = parametrosAtuais,   Iteracoes = etapa, piFinal = pi)
return(resultado)
}

funcaoEM(df = df3, chuteInicial = matrix(c(1, 1, 1, 2, 2, 2), ncol = 2))

            # CATEGORIZAR AS OBSERVAÇÕES
resposta3 <- within(as.data.frame(df3), {
  iEsimo <- c(rep("mu1", n_mu1), rep("mu2", n_mu2))
  Pi <- funcaoEM(df = df3, chuteInicial = matrix(c(1, 1, 1, 2, 2, 2), ncol = 2))$piFinal
  EM <- ifelse(Pi >= 0.5, "mu1", "mu2")
})

xtabs(~resposta3$EM + ~resposta3$iEsimo)



                    # Q3: D: OPTIM() R ----
fOptim <- function(parametros, df) {
  mu1 <- parametros[1:3]
  mu2 <- parametros[4:6]
  -sum(log(0.425*apply(dnorm(df-mu1),1,prod) + 0.575*apply(dnorm(df-mu2),1,prod)))
}

optim(par = c(1, 1, 1, 2, 2, 2), fn = fOptim, df = df3, method = 'Nelder-Mead')


                    # Q3: E: COMPARAR B C E D ----
        # GERAR DADOS
df3 <- matrix(NA, nrow = 40, ncol = 3)
mu <- matrix(c(rep(0, 3), rep(3,3)), ncol = 2)
set.seed(15081991)
        # PESOS MISTURA
n_mu1 <- round(nrow(df3) * runif(1), digits = 0)
n_mu2 <- nrow(df3) - n_mu1

for (w in seq(ncol(df3))) {
  df3[,w] <- c(
    rnorm(n = n_mu1, mean = mu[w, 1], sd = 1),
    rnorm(n = n_mu2, mean = mu[w, 2], sd = 1)
  )
}
        # COMPARAR
          # CHUTE PRÓXIMO
chuteProximo <- c(rep(sample(0:2, 1), 3), sample(0:2, 3, replace = TRUE))
chuteProximo
sAnnealing(df = df3, chuteInicial = matrix(chuteProximo, ncol = 2), pesos = c(n_mu1, n_mu2) / sum(c(n_mu1, n_mu2)))$ParâmetrosFinal  # $ParâmetrosFinal OU ver gráfico
funcaoEM(df = df3, chuteInicial = matrix(chuteProximo, ncol = 2))$ParâmetrosFinal   # $ParâmetrosFinal OU $Iteracoes
optim(par = chuteProximo, fn = fOptim, df = df3, method = 'Nelder-Mead')$par                                                         # $par ou $counts        
          # CHUTES VIAJADOS
chuteZoado <- runif(n = 6, 5, 10)
chuteZoado
sAnnealing(df = df3, chuteInicial = matrix(chuteZoado, ncol = 2), pesos = c(n_mu1, n_mu2) / sum(c(n_mu1, n_mu2)))$ParâmetrosFinal
funcaoEM(df = df3, chuteInicial = matrix(chuteZoado, ncol = 2))$ParâmetrosFinal
optim(par = chuteZoado, fn = fOptim, df = df3, method = 'Nelder-Mead')$par  


                    # Q3: F: REPRODUZIR (E) MAS COM OUTRO MU GERADOR -----
        # GERAR DADOS
df3 <- matrix(NA, nrow = 40, ncol = 3)
mu <- matrix(c(rep(0, 3), c(1, 1, 0)), ncol = 2)
set.seed(15081991)
        # PESOS MISTURA
n_mu1 <- round(nrow(df3) * runif(1), digits = 0)
n_mu2 <- nrow(df3) - n_mu1

for (w in seq(ncol(df3))) {
  df3[,w] <- c(
    rnorm(n = n_mu1, mean = mu[w, 1], sd = 1),
    rnorm(n = n_mu2, mean = mu[w, 2], sd = 1)
  )
}
        # COMPARAR
            # CHUTE PRÓXIMO 
chuteProximo <- c(rep(sample(0:2, 1), 3), sample(0:2, 3, replace = TRUE))
chuteProximo
sAnnealing(df = df3, chuteInicial = matrix(chuteProximo, ncol = 2), pesos = c(n_mu1, n_mu2) / sum(c(n_mu1, n_mu2)))$ParâmetrosFinal  # $ParâmetrosFinal OU ver gráfico
funcaoEM(df = df3, chuteInicial = matrix(chuteProximo, ncol = 2))$ParâmetrosFinal   # $ParâmetrosFinal OU $Iteracoes
optim(par = chuteProximo, fn = fOptim, df = df3, method = 'Nelder-Mead')$par                                                         # $par ou $counts        
            # CHUTES VIAJADOS
chuteZoado <- runif(n = 6, 5, 10)
chuteZoado
sAnnealing(df = df3, chuteInicial = matrix(chuteZoado, ncol = 2), pesos = c(n_mu1, n_mu2) / sum(c(n_mu1, n_mu2)))$ParâmetrosFinal
funcaoEM(df = df3, chuteInicial = matrix(chuteZoado, ncol = 2))$ParâmetrosFinal
optim(par = chuteZoado, fn = fOptim, df = df3, method = 'Nelder-Mead')$par  


                    # Q3: G: KMEANS E EM PARA POPULAÇÃO GENÉTICA ----
dfProfessora <- read.table("DadosGeneticos.txt", header = T)
dfProfessora <- dfProfessora[, unlist(lapply(dfProfessora, function(x) !all(x == 0)))]
    # kMeans
Q3G_kMeans <- kMeansBloco(X = dfProfessora, k = 2)

    # EM
options(scipen = 9999)
Q3G_EM <- funcaoEM(df = as.matrix(dfProfessora), chuteInicial = matrix(rnorm(n = ncol(dfProfessora) * 2), ncol = 2), convergencia = 0.0001)


  # COMPARAR AGRUPAMENTOS
kMeansFinal <- data.frame(Grupo = row.names(Q3G_kMeans$X), Cluster = Q3G_kMeans$X$Cluster)           # k-means
EMFinal <- data.frame(Grupo = names(Q3G_EM$piFinal), ClusterEM = ifelse(Q3G_EM$piFinal <= 0.5, 1, 2))

left_join(kMeansFinal, EMFinal, by = "Grupo")

# FIM ----
