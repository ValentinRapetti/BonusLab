#' Ridge Regression using QR factorization
#'
#' This function estimates the parameters of a ridge regression model for a given
#' dataset, formula, and regularization parameter lambda. Before fitting the model
#' using QR factorization, the predictor variables are standardized.
#'
#' The function returns an object of class `"linreg"`.
#'
#' @param formula A model formula.
#' @param data A data frame containing the variables in the model.
#' @param lambda A non-negative regularization parameter.
#'
#' @return An object of class `"linreg"`.
#' @export
ridgereg <- function(formula, data, lambda){
  stopifnot(is.data.frame(data), class(formula)=="formula", 
            is.numeric(lambda) && length(lambda)==1 && !(lambda < 0))
  
  X <- model.matrix(formula, data)
  X <- scale(X, center = TRUE, scale = TRUE)[ , drop=FALSE]

  # We fix the intercept and store the scaled values
  X[, 1] <- 1
  scale <- data.frame(scale = attr(X, "scaled:scale"),
                    center = attr(X, "scaled:center"))

  X <- X[, 1:ncol(X)]
  X[is.nan(X)] <- 0

  y <- data[[all.vars(formula)[1]]]
  y1 <- c(1:ncol(X)) *0
  y <- c(y,y1)

  X_original <- X
  X <- rbind(X, sqrt(lambda)*diag(ncol(X)))
  n <- nrow(X)
  m <- ncol(X)
  
  Q <- diag(n)
  R <- X
  
  for(i in 1:m){
    # Initialize our vector with zeros and then take the rest from our matrix
    v <- seq_len(n)*0
    v[i:n] <- R[i:n,i]
    
    # Calculate the Householder matrix
    normv <- sqrt(sum(v^2))
    
    if (normv == 0)
      next
    
    s <- ifelse(v[i] >= 0, 1, -1)
    v[i] <- v[i] + s * normv
    
    H <- diag(nrow(R)) - 2* (v %*% t(v)) / (t(v) %*% v)[1]
    
    # Update our matrixes
    R <- H %*% R
    Q <- Q %*% H
  }
  
  R <- R[1:m,]
  Q <- Q[,1:m]
  
  beta_hat <- solve(R) %*% t(Q) %*% y
  prediction <- X_original %*% beta_hat
  
  result <- list(coeffs = beta_hat,
                 predicted = prediction,
                 call = match.call(),
                 formula = formula,
                 scale = scale,
                 terms = terms(formula, data = data))

  class(result) <- "ridgereg"
  return(result)
}