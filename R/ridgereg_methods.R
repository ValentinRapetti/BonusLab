#' Print linreg class object
#'
#' Print the coefficients and the call function of the linear regression
#'
#' @param x linreg object
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


#' Predictions of linreg object
#'
#' Returns all the predictions of the linreg object
#'
#' @param x linreg object
#' @return float list
#' @export
predict.ridgereg <- function(x){
  return(x[["predicted"]])
}

#' Coefficients of linreg object
#'
#' Returns all the coefficients of the linreg object
#'
#' @param object linreg object
#' @param ... other options
#' @return float list
#' @export
coef.ridgereg <- function(object, ...){  
  coef_vector <- unlist(object["coeffs"])
  names(coef_vector) <- rownames(object[["coeffs"]])
  
  return(coef_vector)
}
