# LISTA 3: COMPUTACIONAL ----

  # Q1 ----
    # Q1: A ----
randomPareto <- function(n, beta, alpha) {
  beta / (1 - runif(n)) ^ (1 / alpha)
}

    # Q1: B ----
randomGumbel <- function(n) {
  -log(-log(runif(n)))
}

    # Q1: C ----
randomF <- function(n, gl1, gl2) {
  # GERAR NORMAIS
  z1 <- sqrt(-2 * log(runif(n * gl1))) * sin(2 * pi * runif(n * gl1))
  z2 <- sqrt(-2 * log(runif(n * gl2))) * sin(2 * pi * runif(n * gl2))
  # GERAR CHIS
  chi1 <- apply(matrix(z1 ** 2, ncol = gl1), 1, sum)
  chi2 <- apply(matrix(z2 ** 2, ncol = gl2), 1, sum)
  # GERAR F
  (chi1 / gl1) / (chi2 / gl2)
}

    # Q1: D ----
randomGeom <- function(n, p) {
  floor(log(1 - runif(n)) / log(1 - p))
}

randomNegBin <- function(n, p, k) {
  matriz <- matrix(floor(log(1 - runif(n * k)) / log(1 - p)), ncol = k)
  apply(matriz, 1, sum)
}

    # Q1: E (INCOMPLETA) ----

    # Q1: COMPARAÇÃO  ----
library(tidyverse)
list(
            # F
  # data.frame(Origem = "R", rF = rf(n = 1000, df1 = 3, df2 = 4)),
  # data.frame(Origem = "Custom", rF = randomF(n = 1000, gl1 = 3, gl2 = 4))
            # GEOMETRICA
  # data.frame(Origem = "R", rF = rgeom(n = 1000, prob = 1/2)),
  # data.frame(Origem = "Custom", rF = randomGeom(n = 1000, p = 1/2))
            # NEGATIVA BINOMIAL
  data.frame(Origem = "R", rF = rnbinom(n = 10000, p = 1/2, size = 5)),
  data.frame(Origem = "Custom", rF = randomNegBin(n = 10000, p = 1/2, k = 5))
) %>%
  bind_rows() %>%
  ggplot(., aes(x = rF, fill = Origem)) + 
    geom_density(alpha = 0.3) + 
    theme_bw()
  
  
doR <- rf(n = 10000, df1 = 3, df2 = 4)
customR <- randomF(n = 10000, gl1 = 3, gl2 = 4)

    # Q2: B ----
library(tidyverse)
gerarUniforme <- function(n, a, b) {
  a + b * runif(n)
}

a <- sample(seq(-10, 10), size = 1)
b <- sample(seq(10), size = 1)
list(
  a = data.frame(Dist = "1. U[a - 1/2, a + 1/2]", Y = gerarUniforme(n = 5000, a = a - 1/2, b = a + 1/2)),
  b = data.frame(Dist = "2. U[0, b]", Y = gerarUniforme(n = 5000, a = 0, b = b)),
  c = data.frame(Dist = "3. U[a, b]", Y = gerarUniforme(n = 5000, a = a, b = b))
) %>%
  bind_rows() %>%
  ggplot(., aes(x = Y, fill = Dist)) +
    geom_histogram(colour = "black") +
    facet_wrap(~Dist, ncol = 1) +
    theme_bw()


  # Q3 ----
    # Q3: A ----
gerarX <- function(n) {
  resultado <- vector("numeric", n)
  for (w in seq(n)) {
    iteracao <- 0
    U <- 0
    while ( U <= 1 ) {
      iteracao <- iteracao + 1
      U <- U + runif(1)
    }
    resultado[w] <- iteracao
  }
return(resultado)
}

X <- gerarX(10000000)
mean(X) # VALOR ESPERADO
sd(X) / sqrt(length(X)) # ERRO PADRAO

    # Q3: B & C ----
          # Q3: B: IMPORTANCE SAMPLING ----
importanceSampling <- function(n, k, alpha, beta) {
  matriz <- matrix(rbeta(n * k, shape1 = alpha, shape2 = beta), ncol = k)
  hx <- apply(matriz, 1, sum) <= 1
  gx <- apply(dbeta(matriz, shape1 = alpha, shape2 = beta), 1, prod)
  fx <- 1 
  Eg <- (hx * fx) / gx
  return(Eg)
}

Y <- importanceSampling(n = 5000, k = 9, alpha = 2, beta = 9)
mean(Y)  # E(h(X))
sd(Y) / sqrt(length(Y))  # ERRO PADRÃO


          # Q3: B: FORÇA BRUTA: P(X >= 10) ----
library(tidyverse)
iteracoes <- 1000
listaB <- vector("list", iteracoes)

for (w in seq(iteracoes)) {
  X <- gerarX(5876543)
  listaB[[w]] <- data.frame(xtabs(~X))
}

lapply(listaB, function(x) {
                              x %>%
                                mutate(X = as.numeric(X)) %>%
                                group_by(X) %>%
                                summarise(Freq = sum(Freq)) %>%
                                mutate(Prob = Freq / sum(Freq)) %>%
                                filter(X >= 10) %>%
                                summarise(P = sum(Prob))
}) %>%
  bind_rows() %>%
  summarise(
    EX = mean(P),
    EP = sd(P) / sqrt(iteracoes)
  )


          # Q3: C: IMPORTANCE SAMPLING ----
importanceSamplingC <- function(n, k, alpha, beta) {
  matriz <- matrix(rbeta(n * k, shape1 = alpha, shape2 = beta), ncol = k)
  # INDICADORA EM DUAS ETAPAS
  hx <- t(apply(matriz, 1, cumsum))
  hx <- (hx[, (k-1)] <= 1) * (hx[, k] > 1)
  gx <- apply(dbeta(matriz, shape1 = alpha, shape2 = beta), 1, prod)
  fx <- 1 
  Eg <- (hx * fx) / gx
  return(Eg)
}

Y <- importanceSamplingC(n = 5000, k = 10, alpha = 2, beta = 9)
mean(Y)  # E(h(X))
sd(Y) / sqrt(length(Y))  # ERRO PADRÃO


          # Q3: C: FORÇA BRUTA:  P(X = 10) -----
lapply(listaB, function(x) {
                              x %>%
                                mutate(X = as.numeric(X)) %>%
                                group_by(X) %>%
                                summarise(Freq = sum(Freq)) %>%
                                mutate(Prob = Freq / sum(Freq)) %>%
                                filter(X == 10) %>%
                                summarise(P = sum(Prob))
}) %>%
  bind_rows() %>%
  summarise(
    EX = mean(P),
    EP = sd(P) / sqrt(iteracoes)
  )

          # Q3: COMPARAÇÃO DISTRIBUIÇÃO PROBS FORÇA BRUTA VS IMPORTANCE SAMPLING ----
                    # FORÇA BRUTA
df <- lapply(listaB, function(x) {
                                      x %>%
                                        mutate(X = as.numeric(X)) %>%
                                        group_by(X) %>%
                                        summarise(Freq = sum(Freq)) %>%
                                        mutate(Prob = Freq / sum(Freq))
})  %>% bind_rows()

                  # IMPORTANCE SAMPLING
nObs <- 1000
dfIS <- vector("list", nObs)
for (w in seq(nObs)) {
  dfIS[[w]] <- data.frame(
    X = c(10, 11, 12), Freq = c(0, 0, 0),
    Prob = c(
      mean(importanceSamplingC(n = 5000, k = 10, alpha = 1, beta = 9)), 
      mean(importanceSamplingC(n = 5000, k = 11, alpha = 1, beta = 9)), 
      mean(importanceSamplingC(n = 5000, k = 12, alpha = 1, beta = 9)))
  )
}
dfIS <- bind_rows(dfIS)

                  # UNIR E COMPARAR
dfFinal <- list(
  data.frame(Metodo = "ForcaBruta", df),
  data.frame(Metodo = "Importance Sampling", dfIS)
) %>% bind_rows()

dfFinal %>% 
  filter(X >= 10) %>% 
  group_by(X, Metodo) %>% 
  summarise(
    Min = min(Prob), 
    Media = mean(Prob), 
    DP = sd(Prob),
    Max = max(Prob)
  )

        # OUTLIER
dfOutlier <- dfFinal %>% 
  filter(X >= 10) %>%
  group_by(X, Metodo) %>%
  summarise(
    OUTLIER_UPPER = mean(Prob) + 1.5 * quantile(Prob, 0.75)
  )

dfPlot <- dfFinal %>%
  filter(X >= 10) %>%
  left_join(., dfOutlier, by = c("X", "Metodo")) %>%
  filter(Prob <= OUTLIER_UPPER)

ggplot({dfPlot %>% filter(X >= 10)}, aes(x = Prob, fill = Metodo)) +
  geom_density(alpha = 0.3) + 
  facet_wrap(~as.factor(X), scales = "free", ncol = 1) + 
  theme_bw()

  # Q4: ----
df <- data.frame(
  X = c(6.2, 5.1, 7.6, 2.5, 3.5, 9.4, 4.1, 6.3, 3.0, 0.8),
  Y = c(6.9, 5.1, 7.5, 11.1, 10.9, 4.2, 10.5, 6.8, 12.3, 14.3)
)

    # Q4: A ----
              # BOOTSTRAP
nBootstrap <- 5000
nObs <- nrow(df)
resultadoBootstrap <- vector("list", nBootstrap)
for (w in seq(nBootstrap)) {
  regressao <- lm(Y ~ X, data = df[sample(seq(nObs), size = nObs, replace = TRUE), ])
  resultadoBootstrap[[w]] <- data.frame(
                                      beta0 = regressao$coefficients[1], 
                                      beta1 = regressao$coefficients[2]
                                    )
}

resultadoBootstrap <- do.call(rbind, resultadoBootstrap)

lapply(resultadoBootstrap, function(x) {
  mu <- mean(x) 
  sigma <- sd(x)
  data.frame(
    IC_Normalidade = paste("[", round(mu - 1.96 * sigma, 4), ", ", round(mu + 1.96 * sigma, 4), "]", sep = ""),
    IC_Quantis = paste("[", round(quantile(x, 0.025), 4), ", ", round(quantile(x, 0.975), 4), "]", sep = ""), 
    stringsAsFactors = FALSE
  )
})

    # Q4: B ----
nBootstrap <- 5000
nObs <- nrow(df)

regressao <- lm(Y ~ X, data = df)
sdE <- sd(regressao$residuals)

dfTemp <- df
dfEstimadores <- data.frame(beta0 = rep(NA, nBootstrap), beta1 = rep(NA, nBootstrap))
for (w in seq(nBootstrap)) {
  dfTemp$Y <- regressao$coefficients[1] + regressao$coefficients[2] * dfTemp$X + rnorm(nObs, sd = sdE)
  regressaoTemp <- lm(Y ~ X, data = dfTemp)
  dfEstimadores$beta0[w] <- regressaoTemp$coefficients[1]
  dfEstimadores$beta1[w] <- regressaoTemp$coefficients[2]
}

lapply(dfEstimadores, function(x) {
  mu <- mean(x) 
  sigma <- sd(x)
  data.frame(
    IC_Normalidade = paste("[", round(mu - 1.96 * sigma, 4), ", ", round(mu + 1.96 * sigma, 4), "]", sep = ""),
    IC_Quantis = paste("[", round(quantile(x, 0.025), 4), ", ", round(quantile(x, 0.975), 4), "]", sep = ""), 
    stringsAsFactors = FALSE
  )
})


    # Q4: C ----
nBootstrap <- 5000
regressao <- lm(Y ~ X, data = df)

dfTemp <- df
dfEstimador <- data.frame(beta1 = rep(NA, nBootstrap))
for (w in seq(nBootstrap)) {
  dfTemp$Y <- sample(dfTemp$Y)
  regressaoTemp <- lm(Y ~ X, data = dfTemp)
  dfEstimador$beta1[w] <- regressaoTemp$coefficients[2]
}

mu <- mean(dfEstimador$beta1)
sigma <- sd(dfEstimador$beta1)
# pnorm(q = (regressao$coefficients[2] - mu) / sigma, mean = mu, sd = sigma)
pnorm(q = (regressao$coefficients[2] - 0) / sigma, mean = 0, sd = sigma)

    # Q4 PLOT ----
library(tidyverse)
media <- mean(dfEstimador$beta1)
desvioPadrao <- sd(dfEstimador$beta1)
larguraBin <- 0.2
qtdObs <- nrow(dfEstimador)
dfEstimador %>%
  ggplot(., aes(x = beta1)) + 
  geom_histogram(binwidth = larguraBin, fill = "lightblue", colour = "white") +
  geom_vline(xintercept = regressao$coefficients[2], linetype = 2, size = 1.5, colour = "red") + 
  xlab("Distribuição beta1 Permutação") + ylab("Contagem") +
  ggtitle("Distribuição Beta1 + Densidade Normal ~ N(MediaBeta1, VarBeta1)") +
  theme_bw() + 
  stat_function(fun = function(x) dnorm(x, mean = media, sd = desvioPadrao) * qtdObs * larguraBin, color = "darkred", size = 1) 



Crowned with a Victoire de la musique for their albu
