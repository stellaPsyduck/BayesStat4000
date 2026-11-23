#' Burn It
#'
#' Takes a vector of samples and removes a burn-in period.
#'
#' @param samples samples to burn
#' @param burn_len number of samples to skip (default: burn_len = 500)
#'
#' @return A vector with no more burn-in period
#' @export
#'
#' @examples

burn_it <- function(samples, burn_len=500) {
  return(samples[burn_len:length(samples)])
}
