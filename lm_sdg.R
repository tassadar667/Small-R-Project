x=read.csv('Bodyfat.csv',header = TRUE)
sgd.lm <- function(X, y, beta.init, alpha = 0.05, n.samples = 1, tol = 1e-03, max.iter = 100000) {
  n <- length(y)
  beta.old <- beta.init
  J <- betas <- list()
  sto.sample <- sample(1:n, n.samples, replace = TRUE)
  betas[[1]] <- beta.old
  J[[1]] <- lm.cost(X, y, beta.old)
  beta.new <- beta.old - alpha * sgd.lm.cost.grad(X[sto.sample, ], y[sto.sample], beta.old)
  betas[[2]] <- beta.new
  J[[2]] <- lm.cost(X, y, beta.new)
  iter <- 0
  n.best <- 0
  while ((abs(lm.cost(X, y, beta.new) - lm.cost(X, y, beta.old)) > tol) & (iter + 2 < max.iter)) {
    beta.old <- beta.new
    sto.sample <- sample(1:n, n.samples, replace = TRUE)
    beta.new <- beta.old - alpha * sgd.lm.cost.grad(X[sto.sample, ], y[sto.sample], beta.old)
    iter <- iter + 1
    betas[[iter + 2]] <- beta.new
    J[[iter + 2]] <- lm.cost(X, y, beta.new)
  }
  if (abs(lm.cost(X, y, beta.new) - lm.cost(X, y, beta.old)) > tol) {
    cat("Did not converge. \n")
  } else {
    cat("Converged. \n")
    cat("Iterated ", iter + 1, " times.", "\n")
    cat("Coefficients: ", beta.new, "\n")
    return(list(coef = betas, cost = J, niter = iter + 1))
  }
}
## Make the cost function
sgd.lm.cost <- function(X, y, beta) {
  n <- length(y)
  if (!is.matrix(X)) {
    X <- matrix(X, nrow = 1)
  }
  loss <- sum((X %*% beta - y)^2)/(2 * n)
  return(loss)
}
## Calculate the gradient
sgd.lm.cost.grad <- function(X, y, beta) {
  n <- length(y)
  if (!is.matrix(X)) {
    X <- matrix(X, nrow = 1)
  }
  t(X) %*% (X %*% beta - y)/n
}

lm.cost <- function(X, y, beta) {
  n <- length(y)
  loss <- sum((X %*% beta - y)^2)/(2 * n)
  return(loss)
}
## Calculate the gradient
lm.cost.grad <- function(X, y, beta) {
  n <- length(y)
  return()
}

graddesc.lm <- function(X, y, beta.init, alpha = 0.1, tol = 1e-09, max.iter = 100) {
  beta.old <- beta.init
  J <- betas <- list()
  betas[[1]] <- beta.old
  J[[1]] <- lm.cost(X, y, beta.old)
  beta.new <- beta.old - alpha * lm.cost.grad(X, y, beta.old)
  betas[[2]] <- beta.new
  J[[2]] <- lm.cost(X, y, beta.new)
  iter <- 0
  while ((abs(lm.cost(X, y, beta.new) - lm.cost(X, y, beta.old)) > tol) & (iter < max.iter)) {
    beta.old <- beta.new
    beta.new <- beta.old - alpha * lm.cost.grad(X, y, beta.old)
    iter <- iter + 1
    betas[[iter + 2]] <- beta.new
    J[[iter + 2]] <- lm.cost(X, y, beta.new)
  }
  if (abs(lm.cost(X, y, beta.new) - lm.cost(X, y, beta.old)) > tol) {
    cat("Did not converge \n")
  } else {
    cat("Converged \n")
    cat("Iterated", iter + 1, "times", "\n")
    cat("Coef: ", beta.new)
    return(list(coef = betas, cost = J))
  }
}

y=x$bodyfat
x=x[,-2]
x=as.matrix(x)
x=scale(x)
init_beta=seq(-1,1,length.out=14)
init_beta=matrix(init_beta,ncol  = 1)
sgd.lm(x,y,init_beta)
