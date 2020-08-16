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


chuteProximo <- c(58, -0.01)
gaussNewtonR(f = funcaoOtimizar, J = jacobianaFuncaoOtimizar, X = df2$x, Y = df2$y, chuteInicial = chuteProximo)

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
  

newtonRaphson(f = fNR, g = gNR, X = df2$x, Y = df2$y, chuteInicial = c(55, -0.05))
newtonRaphson(f = fNR, g = gNR, X = df2$x, Y = df2$y, chuteInicial = c(0, -0.05))
