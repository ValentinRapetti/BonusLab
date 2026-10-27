#' Print ridgereg class object
#'
#' Print the coefficients and the call function of the ridge regression
#'
#' @param x ridgereg object
#' @param ... other options
#' @export
print.ridgereg <- function(x, ...){
  cat("Call:\n")
  print(x$call)
  cat("\nCoefficients:\n")
  print_list <- unlist(x["coeffs"])
  names(print_list) <- rownames(x[["coeffs"]])
  
  print(print_list)
}


#' Predictions of ridgereg object
#'
#' Returns all the predictions of the ridgereg object
#'
#' @param x ridgereg object
#' @param newdata dataframe with new data to make predictions. Optional.
#' @param ... other options
#' @return float list with predicted values
#' @export
predict.ridgereg <- function(x, newdata=NULL, ...){
  if( is.null(newdata) )
    return(x[["predicted"]])

  terms <- delete.response(x$terms)

  m <- model.frame(terms, as.data.frame(newdata))
  newdata <- model.matrix(terms, m)

  newdata <- scale(newdata,
              center = x$scale$center,
              scale = x$scale$scale)

  newdata[, 1] <- 1

  return(drop(newdata %*% coef(x)))
}

#' Coefficients of ridgereg object
#'
#' Returns all the coefficients of the ridgereg object
#'
#' @param object ridgereg object
#' @param ... other options
#' @return float list
#' @export
coef.ridgereg <- function(object, ...){  
  coef_vector <- unlist(object["coeffs"])
  names(coef_vector) <- rownames(object[["coeffs"]])
  
  return(coef_vector)
}
