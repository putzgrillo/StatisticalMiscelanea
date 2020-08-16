  # QUESTÃO 2: REGRESSÃO NÃO-LINEAR ----
      # Q2: A: ----
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
    variacao <<-  abs(x1 - x0)
    x0 <- x1
    valoresParciais[[etapa]] <- x1
    continuar <- sum(variacao > convergencia) > 0
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
