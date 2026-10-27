#' Print a ridgereg Object
#'
#' Prints the coefficients and the function call associated with a ridge
#' regression model.
#'
#' @param x a ridgereg object.
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


#' Predict from a ridgereg Object
#'
#' Returns the predicted values from a ridgereg object.
#'
#' @param x a ridgereg object.
#' @param ... other options.
#' @return A numeric vector of predicted values.
#' @export
predict.ridgereg <- function(x, ...){
  return(x[["predicted"]])
}

#' Extract Coefficients from a ridgereg Object
#'
#' Returns the regression coefficients from a ridgereg object.
#'
#' @param object A ridgereg object.
#' @param ... other options.
#' @return A named numeric vector containing the estimated coefficients.
#' @export
coef.ridgereg <- function(object, ...){  
  coef_vector <- unlist(object["coeffs"])
  names(coef_vector) <- rownames(object[["coeffs"]])
  
  return(coef_vector)
}
