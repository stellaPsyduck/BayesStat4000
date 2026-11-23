#' Credible Interval
#'
#' Calculates the credible interval
#'
#' @param samples vector of samples to find interval of
#' @param alpha the confidence level of interest
#'
#' @return list of the "low"er and "up"per bounds
#' @export
#'

cred_int <- function(samples, alpha=0.05) {
  credInt = quantile(samples, c(alpha*0.5,(1 - alpha*0.5)))

  return(list(low= credInt[1], up= credInt[2]))
}
