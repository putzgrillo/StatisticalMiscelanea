      # Q3: A: ----
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
