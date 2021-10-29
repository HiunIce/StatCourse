library(MASS)
n <- 500
beta = c(-1, 1, 1, 1)
sigma = matrix(c(1, 0.2, 0.2, 1), nrow=2, ncol=2, byrow=TRUE)
x1 = runif(n)
mv_dat = mvrnorm(n, c(0,0), sigma)
x2 = mv_dat[, 1]
x3 = mv_dat[, 2]
x = rbind(rep(1, n), x1, x2, x3)
miu = beta %*% x

y = rep(-1, n)

for (i in 1:n){
  y[i] = rpois(1, miu[i])
}
model = glm(y ~ x1 + x2 + x3, family=poisson)  
summary(model)

