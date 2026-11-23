#' Normal Conjugate Priors
#'
#' Takes current positions of mu and tau to find another random location in the
#' parameter space.
#'
#' This structure uses:
#'    - a Normal Likelihood
#'    - Mu (mean) parameter following the Normal Distribution
#'    - Tau (1/var(obs)) parameter following the Gamma Distribution
#'
#' @param last_mu previous value of parameter mu
#' @param last_tau previous value of parameter tau
#' @param arbs_constants arbitrary constants, formatted by 'save_constants()'
#'        function
#'
#' @return A list holding the new sample under the names 'mu' and 'tau'
#' @export
#'
#' @examples
#' init_mu <- mean(obs)
#' init_tau <- 1 / var(obs)
#' arb_constants <- save_constants(5, 20, 1, 1)
#' samples <- run_gibb(init_mu, init_tau, arb_constants, obs=obs, runs=10000)

normal_conjugatePriors <- function(last_mu, last_tau,
            arb_constants, obs) {

  # set up for mu calculation
  mu_mean <-  ((sum(obs)*last_tau)+(arb_constants$mu_mu*arb_constants$tau_mu))/
              ((n_obs*last_tau)+arb_constants$tau_mu)
  mu_tau <- (n_obs*last_tau) + arb_constants$tau_mu
  mu <- rnorm(1, mean=mu_mean, sd = 1/sqrt(mu_tau))


  # set up for sigma calculation
  shape <- arb_constants$a + (n_obs)/2
  rate <- arb_constants$b + 0.5*(sum((obs - mu)**2))
  tau <- rgamma(1, shape, rate)


  # returning the new sample
  return(list(mu=mu, tau=tau))
}
