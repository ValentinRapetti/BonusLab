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
#' @return float list
#' @param ... other options
#' @export
predict.ridgereg <- function(x, ...){
  return(x[["predicted"]])
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
