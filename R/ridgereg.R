


ridgereg <- function(formula, data, lambda){
  
  X<- model.matrix(formula, data)
  y <- data[[all.vars(formula)[1]]]
  X<- scale(X, center = TRUE, scale = TRUE)[,-1]
  Beta <- solve(t(X) %*% X + diag(lambda, ncol = ncol(X), nrow = ncol(X))) %*% t(X) %*% y 
  # check efficency 
  prediction <- X %*% Beta
  result <- list(coeffs = Beta,
                 predicted = prediction,
                 call = match.call())
  class(result) <- "ridgereg"
  return(result)
}

data(iris)
formula <- Petal.Length ~ Species
r <- ridgereg(formula, iris, 1)
library(MASS)
r1 <- lm.ridge(formula, iris,lambda =  1)