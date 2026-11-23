#' Save Constants
#'
#' Saves relevant constants for gibbs sampling in package-friendly format
#'
#' @param sampler string associated with a sampler in the package.
#'                OPTIONS: "norm_conj", "test"
#' @param mu_mu the mean for the normal conjugates sigma's distribution
#' @param tau_mu the 1/var(obs) for the normal conjugates sigma distribution
#' @param a the shape for the normal conjugates gamma distribution
#' @param b the shape for the normal conjugates gamma distribution
#'
#' @return A list with all the relevant arbitrary constants in
#'         a package-friendly format
#' @export
#'
#' @examples
#' arb_constants <- save_constants("norm_conj", mu_mu=5, tau_mu=20, a=1, b=1)
#'
#' # special normal conjugate case, since the first few values are what's used
#' arb_constants <- save_constants("norm_conj", 5, 20, 1, 1)
#'
#'
#'

save_constants <- function(sampler="norm_conj", #sampler
                           mu_mu=0, tau_mu=0.05, a=3, b=2, #normal conjugate
                           test=0) {
  if (sampler == "norm_conj") {
      return(list(mu_mu=mu_mu, tau_mu=tau_mu, a=a, b=b))
  } else if (sampler == "test") {
      print("bleh")
      return(list=test)
  }
}
