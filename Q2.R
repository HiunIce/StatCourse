library(MASS)
library(nnet)
# install.packages('ordinal')
library(ordinal)
n <- 800
b1 = c(-1, -1, 1)
b2 = c(-1, -1, 1)
b3 = c(1, -1, 1)
beta = cbind(b1, b2, b3)
x1 = runif(n)
x2 = rnorm(n, 0, 1)
x = rbind(rep(1, n), x1, x2)


p1 <- exp(b1 %*% x)/(1+exp(b1 %*% x))
p2 <- (exp(b2 %*% x)/(1+exp(b2 %*% x))) - (exp(b1 %*% x)/(1+exp(b1 %*% x)))
p3 <- (exp(b3 %*% x)/(1+exp(b3 %*% x))) - (exp(b2 %*% x)/(1+exp(b2 %*% x)))
p4 <- 1/(1+exp(b3 %*% x))

p <- rbind(p1, p2, p3, p4)
p_colsum <- apply(p, 2, sum)
p_colsum4 <- matrix( rep(p_colsum, 4), nrow=4, byrow=T )
p_normalized <- p/p_colsum4

y = rep(-1, n)
for (i in 1:n){
  y[i] = which(rmultinom(1, 1, p_normalized[,i]) > 0)
}
model <- clm(factor(y) ~ x1 + x2)
summary(model)

