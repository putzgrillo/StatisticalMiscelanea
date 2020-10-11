# LISTA 4 COMPUTACIONAL ----
  # Q2 ----
        # Q2: MCMC ----
funcao_p <- function(X) {
  exp(-(X[1] ** 2 / 2) - (X[2] ** 2 / 2) )
}

proposta <- function(Xi) {
  candidatos <- seq(-5, 5, by = 1)
  c(Xi[1] + sample(candidatos, 1),  Xi[2] + sample(candidatos, 1))
}

prob_aceita = function(x_new, x_old){
  min(c(funcao_p(X = x_new)/funcao_p(X = x_old), 1))
}


MCMC = function(x0, maxIter = 5000, burnIn = 0.1){
  MCMC_d = matrix(nrow = maxIter, ncol = length(x0))
  MCMC_d[1, ] <- x0
  
  it = 1
  while( it != maxIter ){
    x_old = MCMC_d[it, ]
    x_new = proposta(x_old)
    
    if( runif(1) < prob_aceita(x_new,x_old) ) {
      MCMC_d[it + 1, ] = x_new 
    } else {
      MCMC_d[it + 1, ] = x_old 
    }
    it = it+1
  }
  MCMC_d <- MCMC_d[-seq(floor(maxIter * burnIn)), ]
  MCMC_d <- as.data.frame(MCMC_d)
  colnames(MCMC_d) = paste("X", seq(length(x0)), sep = "") 
return(MCMC_d)
}

dfMCMC <- MCMC(x0 = c(5,5), maxIter = 5000, burnIn = 0.1)

        # Q2: MEDIA E VARIANCIA  -----
lapply(dfMCMC, function(x) {
  data.frame(Media = mean(x), Variancia = var(x))
})

        # Q2: PLOT MARGINAIS ----
library(tidyverse)
dfMCMC %>%
  pivot_longer(cols = everything(), values_to = "VALOR", names_to = "VARIAVEL") %>%
  ggplot(., aes(x = VALOR, fill = VARIAVEL)) +
    geom_histogram(colour = "black") +
    ggtitle("Marginais") + 
    facet_wrap(~VARIAVEL, ncol = 1) +
    theme_bw() +
    theme(
      strip.text = element_text(size = 12)
    )

        # Q2: PLOT CONJUNTA ----
            # RAZÃO ENTRE TAMANHOS (MENOR E MAIOR PONTO)
dfMCMC %>%
  group_by(X1, X2) %>%
  summarise(Freq = n()) %>%
  ungroup() %>%
  summarise(Min = min(Freq), Max = max(Freq)) %>%
  mutate(RazaoMaxMin = Max / Min)

          # Q2: PLOT CONJUNTA ----
library(ggExtra)
plotGG <- dfMCMC %>%
  left_join(., {dfMCMC %>%
                  group_by(X1, X2) %>%
                  summarise(Freq = n()) %>%
                  ungroup() %>%
                  mutate(FreqRelativa = Freq / sum(Freq))
      }, by = c("X1", "X2")) %>%
  ggplot(., aes(y = X1, x = X2)) +
    geom_point(aes(size = Freq), colour = "blue") +
    geom_text(aes(y = X1, x = X2, label = Freq), size = 8, colour = "black") + 
    scale_size_continuous(range = c(0.1, 65)) +
    theme_bw() +
    theme(
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 15)
    )

ggMarginal(p = plotGG, type = "histogram", size = 3)


  # Q3: ----
        # Q3: MCMC (COM PROPOSTA MAIS FLEXÍVEL) ----
library(coda)
library(mcmcse)
funcao_p <- function(X) {  sum(X) ** 5  }

proposta <- function(X, nDigitos = 3, puloAbs = 100) {                
  formato <- paste("%0", nDigitos, "d", sep = "")
  nMax <- as.numeric(paste(rep(9, nDigitos), collapse = "")) + 1
  x_old <- as.numeric(paste(X, collapse = ""))
  x_new <- (x_old + sample(seq(-puloAbs, puloAbs), 1)) %% nMax       # X_OLD +- puloAbs
  as.numeric(unlist(strsplit(sprintf(x_new, fmt = formato), split = "")))   
}

prob_aceita = function(x_new, x_old){
  min(c(funcao_p(X = x_new)/funcao_p(X = x_old), 1))
}


MCMC = function(x0, maxIter = 5000, burnIn = 0.1, digitos = 3, pulos = 100){
  MCMC_d = matrix(nrow = maxIter, ncol = length(x0))
  MCMC_d[1, ] <- x0
  
  it = 1
  while( it != maxIter ){
    x_old = MCMC_d[it, ]
    x_new = proposta(x_old, nDigitos = digitos, puloAbs = pulos)
    
    if( runif(1) < prob_aceita(x_new,x_old) ) {
      MCMC_d[it + 1, ] = x_new 
    } else {
      MCMC_d[it + 1, ] = x_old 
    }
    it = it+1
  }
  MCMC_d <- MCMC_d[-seq(floor(maxIter * burnIn)), ]
  MCMC_d <- as.data.frame(MCMC_d)
  colnames(MCMC_d) = paste("X", seq(length(x0)), sep = "") 
  return(MCMC_d)
}

        # Q3: COMPARAÇÃO MCMC AULA E EXERCÍCIO ----
            # Q3: AULA ----
                    # GERAR OBJETO MCMC 
dfMCMC_Aula <- MCMC(x0 = c(0, 0, 0), maxIter = 100000, digitos = 3, pulos = 100)    
colnames(dfMCMC_Aula) <- c("Centena", "Dezena", "Unidade")
dfMCMC_Aula$ValorX <- apply(dfMCMC_Aula, 1, function(x)  as.numeric(paste(x, collapse = "")) )
dfMCMC_Aula$fX <- apply(dfMCMC_Aula[, -ncol(dfMCMC_Aula)], 1, function(x) funcao_p(x))
MCMC_Aula <- mcmc(dfMCMC_Aula)

                    # SUMÁRIO MCMC
summary(MCMC_Aula)                 # SUMÁRIO MCMC
plot(MCMC_Aula)                    # PLOT MCMC
1 - rejectionRate(MCMC_Aula)       # TAXA DE ACEITAÇÃO: ideal próximo de 0.234 p rand walk proposal

effectiveSize(MCMC_Aula)           # TAMANHO AMOSTRAL EFETIVO
autocorr.plot(MCMC_Aula)

            # Q3: EXERCICIO ----
                  # GERAR OBJETO MCMC 
dfMCMC_Ex <- MCMC(x0 = c(0, 0, 0, 0, 0), maxIter = 100000, digitos = 5, pulos = 10000)    
colnames(dfMCMC_Ex) <- c("DezenaMilhar", "Milhar", "Centena", "Dezena", "Unidade")
dfMCMC_Ex$ValorX <- apply(dfMCMC_Ex, 1, function(x)  as.numeric(paste(x, collapse = "")) )
dfMCMC_Ex$fX <- apply(dfMCMC_Ex[, -ncol(dfMCMC_Ex)], 1, function(x) funcao_p(x))
MCMC_Ex <- mcmc(dfMCMC_Ex)

                  # SUMÁRIO MCMC
summary(MCMC_Ex)                 # SUMÁRIO MCMC
plot(MCMC_Ex)                    # PLOT MCMC
1 - rejectionRate(MCMC_Ex)       # TAXA DE ACEITAÇÃO: ideal próximo de 0.234 p rand walk proposal

effectiveSize(MCMC_Ex)           # TAMANHO AMOSTRAL EFETIVO
autocorr.plot(MCMC_Ex)


        # Q3: TAMANHO DO PULO VS TAMANHO EFETIVO ----
comb <- data.frame(expand.grid(
  Passos = seq(100, 40100, by = 10000), 
  Iteracoes = seq(5000, 20000, by = 5000)
))

listaTeste <- vector("list", nrow(combinacoes))
tempo <- proc.time()
for (w in seq_along(listaTeste)) {
  dfResultado <- vector("list", 40) 
  for (y in seq_along(dfResultado)) {
    dfMCMC_Ex <- MCMC(x0 = c(0, 0, 0, 0, 0), maxIter = comb$Iteracoes[w], digitos = 5, pulos = comb$Passos[w])    
    colnames(dfMCMC_Ex) <- c("DezenaMilhar", "Milhar", "Centena", "Dezena", "Unidade")
    dfMCMC_Ex$ValorX <- apply(dfMCMC_Ex, 1, function(x)  as.numeric(paste(x, collapse = "")) )
    dfMCMC_Ex$fX <- apply(dfMCMC_Ex[, -ncol(dfMCMC_Ex)], 1, function(x) funcao_p(x))
    MCMC_Ex <- mcmc(dfMCMC_Ex)
    dfResultado[[y]] <- effectiveSize(MCMC_Ex)
    if(y %% 10 == 0) {print(y)}
  }
  dfTemp <- do.call(rbind, dfResultado)
  dfTemp <- data.frame(dfTemp, Passo = comb$Passos[w], Iteracoes = comb$Iteracoes[w])
  listaTeste[[w]] <- dfTemp
  rm(dfTemp)
  print(w)
}
proc.time() - tempo
dfPlot <- do.call(rbind, listaTeste)
ggplot(dfPlot, aes(y = ValorX, x = as.factor(Passo), colour = as.factor(Passo))) +
  geom_boxplot() +
  ggtitle("Nº Repetições = 20000 * 90%") +
  ylab("Tamanho Amostral Efeitvo") +   xlab("Amplitude do passo a partir do ponto inicial") +
  theme_bw() +
  theme(legend.position = "none")

# CÓDIGO DE AULA -----
install.packages("mcmcse")
install.packages("coda")
library(coda)
library(mcmcse)


funcao_p=function(x){      ## função proporcional a densidade que queremos amostrar (aqui X é o vetor de dados)
  (x[1]+x[2]+x[3])^(5)
}

proposta=function(xi){    # aqui xi eh o elemento que queremos amostrar 
  
  x_old=100*xi[1]+10*xi[2]+xi[3]
  x_new=x_old+sample(c(-2,-1,1,2),1)
  
  if(x_new==-1) x_new=999     
  if(x_new==-2) x_new=998     
  if(x_new==1000) x_new=0     
  if(x_new==1001) x_new=1     
  
  res=vector()
  res[1]=floor(x_new/100)
  res[2]=floor((x_new-100*res[1])/10)
  res[3]=x_new-100*res[1]-10*res[2]
  res
}

#   p_proposta=1/4   

prob_aceita=function(x_old,x_new){
  min(funcao_p(x_new)/funcao_p(x_old), 1)
}





###################  Função MCMC

runmcmc=function(start,n_MCMC=50000){
  
  
  #n_MCMC=50000  #número de iterações do MCMC
  
  
  MCMC_d=matrix(ncol=n_MCMC, nrow=3)     #declara a matriz que conterá os valores dos dígitos ao longo do MCMC
  #cada coluna eh uma ieração do MCMC
  
  p=vector()                 #feclara vetor que vai guardar resultado da função densidade para cada observação
  
  MCMC_d[,1]=start       #escolhe valores iniciais
  
  
  
  
  
  for(it in (1:(n_MCMC-1))){    ### inicia o algoritmo
    x_old=MCMC_d[,it]
    x_new=x_old
    x_new=proposta(x_old)                ## cria uma proposta
    
    if(runif(1)<prob_aceita(x_old,x_new)){
      MCMC_d[,it+1]=x_new                         #com probabilidade prob_aceita registra o valor da proposta no MCMV 
    }else{
      MCMC_d[,it+1]=x_old                         #caso contrario repete o valor antigo 
    }
    
  }
  return(MCMC_d)
}

### Utilizando o pacote de diagnostico coda

MCMC_d=runmcmc(start=c(0,0,0),100000)

#monta o numero 
MCMC=rbind(MCMC_d,MCMC_d[1,]*100+MCMC_d[2,]*10+MCMC_d[3,],(MCMC_d[1,]+MCMC_d[2,]+MCMC_d[3,])^5)
row.names(MCMC)=c("centena","dezena","unidade","valor x","f(x)")

x=mcmc(t(MCMC))   #cria elemento coda.mcmc
summary(x)
plot(x)

### Pontos iniciais diferentes
MCMC_d=runmcmc(start=c(0,0,0),100000)
x1=MCMC_d[1,]*100+MCMC_d[2,]*10+MCMC_d[3,]
MCMC_d=runmcmc(start=c(4,4,4),100000)
x2=MCMC_d[1,]*100+MCMC_d[2,]*10+MCMC_d[3,]  #rodar novamente com outro ponto inicial
MCMC_d=runmcmc(start=c(8,8,8),100000)
x3=MCMC_d[1,]*100+MCMC_d[2,]*10+MCMC_d[3,]  #rodar novamente com outro ponto inicial


ts.plot(x1,ylim=c(0,1000))
lines(x2,col="red")
lines(x3,col="green")

### 

acceptanceRate <- 1 - rejectionRate(x)
acceptanceRate       #ideal próximo de 0.234 p rand walk proposal

effectiveSize(x)      #tamanho amostral efetivo
autocorr.plot(x)


## Compara com gráfico da versão inicial (qual convergiu?)

# Utilizar função mcse.q.mat(x, alfa) para construir intervalos de confiança para a esperança de X.
# qual a incerteza dessas estimativs?
