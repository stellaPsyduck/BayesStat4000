#' Credible Interval
#'
#' Calculates the credible interval
#'
#' @param samples
#'
#' @return list of the "l"ower and "u"pper bounds
#' @export
#'

cred_int <- function(samples, level = 0.95) {
  stopifnot(is.numeric(samples), length(samples) > 0,
            all(is.finite(samples)), level > 0, level < 1)

  alpha <- 1 - level
  quantile(samples, probs = c(alpha / 2, 1 - alpha / 2))
}
