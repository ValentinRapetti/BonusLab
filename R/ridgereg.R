#' Ridge Regression using least squares
#'
#' This funcction calculates the parameters for a linear regression for a given
#'set of data, formula and lambda, the ridge regularization parameter. Befor estimating the regression we 
#'standardise the covariates
#'The output is an object of class "linreg"
#'
#' @param formula formula
#' @param data data.frame
#' @param lambda non negative scalar
#' @return linreg class object
#' @export
ridgereg <- function(formula, data, lambda){
  stopifnot(is.data.frame(data), class(formula)=="formula", is.numeric(lambda) && length(lambda)==1 && !(lambda < 0))
  X<- model.matrix(formula, data)
  y <- data[[all.vars(formula)[1]]]
  X<- scale(X, center = TRUE, scale = TRUE)[,-1, drop = FALSE]
  Beta <- solve(t(X) %*% X + lambda * diag(ncol(X))) %*% t(X) %*% y 
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