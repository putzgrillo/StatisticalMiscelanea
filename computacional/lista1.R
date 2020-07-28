# Q5 ----
  # Q5: 3D SURFACE ----
library(plotly)
library(tidyverse)
df <- data.frame(expand.grid(x = seq(-5, 5, 0.05), y = seq(-5, 5, 0.05))) %>%
  # filter(x**2 + y ** 2 == 1) %>%
  mutate(z = x ** 4 + y ** 2 + 4*x*y) %>%
  pivot_wider(names_from = y, values_from = z) %>%
  as.data.frame
row.names(df) <- df[,1]
df <- df[,-1]
df <- as.matrix(df)

plot_ly(z = ~df) %>%
  add_trace( 
    z = ~df,    x = ~row.names(df),    y = ~colnames(df), 
    type = 'surface',   alpha = 0.8)  %>%
  layout(
    legend = list(orientation = 'h'),
    title = "f(x,y) = x^4 + y²2 + 4xy",
    scene = list(
      xaxis = list(title = "x"),
      yaxis = list(title = "y"),
      zaxis = list(title = "f(x,y)")
    )) 
  # Q5: NEWTON-RAPHSON ----

# f DO PROBLEMA (DERIVADAS PARCIAIS)
dXYL <- function(vetor) {  # x,y,lambda
  resultado <- matrix(c(
    (4 * vetor[1] ** 3)  + (4 * vetor[2]) - 2*vetor[3]*vetor[1],   #df/dx
    (2*vetor[2]) + (4 * vetor[1]) - 2*vetor[3]*vetor[2],           #df/dy
    (2* vetor[1]) + (2* vetor[2])                                 #df/dlambda
  ), ncol = 1)
  return(resultado)
}

# g DO PROBLEMA (HESSIANA)
dfXYL <- function(vetor) { # x,y,lambda
  df2 <- matrix(c(
              (12*vetor[1] ** 2) - 2*vetor[3],                 4,                 -2*vetor[1], 
              4,                                  2 - 2*vetor[3],                 -2*vetor[2],
              2,                                               2,                          0
    ), byrow = TRUE, ncol = length(vetor)
  )
  return(df2)
}

# COMO BUSCA-SE A RAIZ DA DERIVADA, f É A PRIMEIRA DERIVADA & g A SEGUNDA DERIVADA
newtonRaphson <- function(f, g, chuteInicial, erro = 0.00001, iteracaoMaxima = 5000) {
  etapa <- 0
  x0 <- chuteInicial
  while( sum(abs(f(x0)) > erro) > 0 ) {
    x1 <- x0 - solve(g(x0)) %*% f(x0)
    etapa <- etapa + 1
    x0 <- x1
    if (etapa == iteracaoMaxima) {break}
  }
  resultado <- list(nIteracoes = etapa, raizes = matrix(x1, ncol = 1))
  return(resultado)
}

# APLICAR NEWTON-RAPHSON
newtonRaphson(f = dXYL, g = dfXYL, chuteInicial = c(1,1,1))
newtonRaphson(f = dXYL, g = dfXYL, chuteInicial = c(-3,-2,-1))


  # Q5: AVALIACAO CONDIÇÕES INICIAIS ----
combTestar <- expand.grid(x = runif(50, min = -200, max = 200), y = runif(50, min = -200, max = 200), lambda = c(1, runif(50, min = -200, max = 200)))
listaLoop <- vector("list", nrow(combTestar))
for (w in seq(nrow(combTestar))) {
  temp <- newtonRaphson(f = dXYL, g = dfXYL, chuteInicial = c(combTestar$x[w], combTestar$y[w], combTestar$lambda[w]))
  la <- data.frame(
    combTestar[w, ], nIteracoes = temp$nIteracoes, xF = round(temp$raizes[1], 3), yF = round(temp$raizes[1], 3), lambdaF = temp$raizes[3]
  )
  listaLoop[[w]] <- la
}

dfPlot <- bind_rows(listaLoop) %>%
  mutate(Raizes = paste("(", xF, ",", yF, ")", sep = ""))

# TABELA
dfPlot %>%
  group_by(xF, yF) %>%
  summarise(
    Qtde = n(),
    MediaIteracoes = mean(nIteracoes),
    MinIteracoes = min(nIteracoes),
    MaxIteracoes = max(nIteracoes)
  ) %>%
  ungroup() %>%
  mutate(Proporcao = Qtde / sum(Qtde)) 

# PLOT
dfPlot %>%
  group_by(Raizes) %>%
  sample_frac(0.05) %>%
  ggplot(., aes(x = x, y = y, colour = Raizes)) +
    geom_point() +
    xlab("x0") + ylab("y0") +
    theme_bw() +
    theme(
      legend.text = element_text(size = 25),
      legend.title = element_text(size = 15),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 15)
    ) 


# PLOT 3D
plot_ly({dfPlot %>% group_by(Raizes) %>% sample_frac(0.001)},
        x = ~x, 
        y = ~y, 
        z = ~lambda, 
        type = "scatter3d", mode = "markers", color = ~Raizes, 
        # symbol = ~Escolhido, symbols = c('circle', 'diamond'),
        hoverinfo = 'text', 
        text = ~paste('</br> x0: ', round(x, 3),
                      '</br> y0: ', round(y, 3),
                      '</br> lambda0: ', round(lambda, 3),
                      '</br> Raiz: ', Raizes)
) %>%
  layout(
    legend = list(orientation = 'h'),
    title = "x ** 4 + y ** 2 + 4*x*y",
    scene = list(
      xaxis = list(title = "x0"),
      yaxis = list(title = "y0"),
      zaxis = list(title = "lambda0")
    )) # %>% 
  # ADICIONAR PLANO AO GRÁFICO
  add_trace( 
    z = ~planoScatter,
    x = ~eixoX,
    y = ~eixoY, 
    type = 'surface', 
    alpha = 0.2)

# Q3: POSITIVA DEFINIDA ----

# UDF: FORÇAR SIMETRIA VIA TRIANGULAR INFERIOR
forcarSimetria <- function(X) {
  tInferior <- lower.tri(X)
  X[tInferior] <- t(X)[tInferior]
return(X)
}
# FUNÇÃO COMPARAÇÃO
funcaoComparacao <- function(tamanho, iteracoes = 1000) {
        # GERAR MATRIZES
  listaMatrizes <- vector("list", iteracoes)
  for (w in seq_along(listaMatrizes)) {
    mTemp <- matrix(rnorm(n = tamanho ** 2), nrow = tamanho, ncol = tamanho, byrow = FALSE)
    # listaMatrizes[[w]] <- mTemp %*% t(mTemp)      # OBS: AO MULTIPLICAR PELA TRANSPOSTA VIRA SIMÉTRICA E POSITIVA DEFINIDA
    listaMatrizes[[w]] <- forcarSimetria(mTemp)     # UDF NO INÍCIO DO CÓDIGO
  }
        # APLICAR TESTE VIA AUTOVALORES
  resultadoAutovalores <- lapply(listaMatrizes, function(x) {
    inicio <- proc.time()
      positivaDefinida <- sum(eigen(x, only.values = TRUE)$values <= 0) == 0
    difTempo <- proc.time() - inicio
    data.frame(
                Metodo = "Autovalores", PositivaDefinida = positivaDefinida, 
                Tamanho = paste(tamanho, "x", tamanho, sep = " "), data.frame(t(difTempo[1:3]))
              )
  })
  resultadoAutovalores <- do.call(rbind, resultadoAutovalores)
      
        # APLICAR TESTE VIA MENORES PRINCIPAIS
  resultadoMenoresPrincipais <- lapply(listaMatrizes, function(x) {
    inicio <- proc.time()
      determinantes <- vector("numeric", nrow(x))
      determinantes[1] <- x[1,1]                       # POR EFICIÊNCIA, NÃO TESTA-SE CONDIÇÃO E INCLUI-SE QUANDO 1x1
      for (w in seq(2, nrow(x))) {                     # DETERMINANTE APENAS PARA MATRIZES COM NROW E NCOL >= 2
        posicoes <- seq(w)
        determinantes[w] <- det(x[posicoes, posicoes])
      }
      positivaDefinida <- sum(determinantes <= 0) == 0
    difTempo <- proc.time() - inicio
    data.frame(
                Metodo = "Menores Principais", PositivaDefinida = positivaDefinida, 
                Tamanho = paste(tamanho, "x", tamanho, sep = " "), data.frame(t(difTempo[1:3]))
              )
  })
  resultadoMenoresPrincipais <- do.call(rbind, resultadoMenoresPrincipais)
  
      # UNIR TABELAS
  resultado <- rbind(resultadoAutovalores, resultadoMenoresPrincipais)
return(resultado)
}

# APLICAR FUNÇÃO COMPARAÇÃO
teste <- proc.time()
combinacoesTestar <- data.frame(Tamanho = c(5, 10, 100, 500, 1000, 2000), Iteracoes = c(1000, 1000, 1000, 1000, 20, 5))
dfComparacao <- vector("list", nrow(combinacoesTestar))
for (w in seq_along(dfComparacao)) {
  dfComparacao[[w]] <- funcaoComparacao(tamanho = combinacoesTestar$Tamanho[w], iteracoes = combinacoesTestar$Iteracoes[w])
  print(w)
}
dfComparacao <- do.call(rbind, dfComparacao)
proc.time() - teste

# ANÁLISE DOS RESULTADOS
  # ATÉ AGORA APENAS R-BASE, PARA ANÁLISE E COMPARAÇÃO UTILIZA-SE TIDYVERSE,
  # POIS NINGUÉM É OBRIGADO A SOFRER E USAR SOLUÇÕES MENOS EFICIENTES & COM SINTAXE MENOS CLARA 

# TABELA 
library(tidyverse)
dfComparacao %>%
  group_by(Tamanho, Metodo) %>%
  summarise(
    Obs = n(),
    Proporcao_PosDef = mean(PositivaDefinida),
    TempoMedio = mean(elapsed),
    DesvioPadrao = sd(elapsed)
  ) %>%
  mutate(TempoTotal = TempoMedio * Obs) %>%
  View
 
# PLOT LINHA
options(scipen = 9999)
dfComparacao %>%
  group_by(Tamanho, Metodo) %>%
  summarise(TempoMedio = mean(elapsed)) %>%
  ggplot(., aes(x = Tamanho, y = TempoMedio, colour = Metodo, group = Metodo)) + 
    geom_line(size = 1.5, linetype = "dotted") +
    geom_point(size = 5) +
    geom_text(
      aes(
            x = Tamanho, y = TempoMedio, colour = Metodo, 
            label = paste(round(TempoMedio, 10), "s", sep = "")), 
      size = 12, position = position_nudge(y = 1), angle = 0
    ) +
    ggtitle("Comparação Tempo Médio") +
    xlab("Tamanho da Matriz") + 
    ylab("Tempo Médio por Teste (em segundos) - Escala Log10") +
    scale_y_log10(label = scales::number_format()) +
    theme_bw() + 
    theme(
      axis.title = element_text(size = 28),
      axis.text = element_text(size = 25),
      legend.title = element_text(size = 28),
      legend.text = element_text(size = 25)
    )


teste <- proc.time()
eigen(forcarSimetria(matrix(rnorm(500 **2), 500, 500)))
proc.time() - teste
