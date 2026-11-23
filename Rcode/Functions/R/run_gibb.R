#' (TEMPORARY) Run Gibb
#'
#' Runs the normal conjugate gibb sampler
#'
#' @param inital_mu previous value of parameter mu
#' @param inital_tau previous value of parameter tau
#' @param arbs_constants arbitrary constants, formatted by 'save_constants()'
#'        function
#'
#' @return A list of all mus and taus
#' @export
#'
#' @examples
#' init_mu <- mean(obs)
#' init_tau <- 1 / var(obs)
#' arb_constants <- save_constants(5, 20, 1, 1)
#' samples <- run_gibb(init_mu, init_tau, arb_constants, obs=obs, runs=10000)

run_gibb <- function (inital_mu, inital_tau, arb_constants,
                      obs=NULL, runs=5000, seed=666) {

  set.seed(seed)
  samples <- matrix(NA, nrow=runs, ncol=2,
                    dimnames = list(NULL, c("mu", "tau"))) # user friendly?

  start_mu <- inital_mu
  start_tau <- inital_tau

  for (i in 1:runs) {
    #sampling and saving
    gibber <- normal_conjugatePriors(start_mu, start_tau, arb_constants, obs=obs)
    samples[i,1] <- gibber$mu
    samples[i,2] <-gibber$tau

    # Updating the initalization values
    start_mu <- gibber$mu
    start_tau <- gibber$tau
  }

  return(list(mu=samples[,1], tau=samples[,2]))
}
