#' Trace Conjugate
#'
#' Creates trace plot for conjugate priors example
#'
#' @param samples
#' @param true_mu
#' @param true_sig
#'
#' @return a ggplot of the traced data
#' @export
#'

trace_conj <- function(samples, true_mu, true_sig) {
  trace_data <- data.frame(
    iteration = seq_along(samples$mu),
    mu = samples$mu,
    tau = samples$tau
  ) |>
    dplyr::filter(iteration %% 5 == 0) |>
    tidyr::pivot_longer(c(mu, tau),
                        names_to = "parameter",
                        values_to = "draw")

  reference <- data.frame(
    parameter = c("mu", "tau"),
    true_value = c(true_mu, 1/(true_sig**2))
  )

  strip_labels <- c(
    mu = '"Sampled" ~ mu',
    tau = '"Sampled" ~ tau'
  )

  the_plot <- (ggplot(trace_data, aes(x = iteration, y = draw)) +
                    geom_line(linewidth = 0.3, alpha = 0.7) +
                    geom_hline(
                      data = reference,
                      aes(yintercept = true_value),
                      colour = "firebrick",
                      linetype = "dashed"
                    ) +
                    facet_wrap(
                      ~ parameter,
                      ncol = 1,
                      labeller = as_labeller(strip_labels, default = label_parsed),
                      scales = "free_y"
                    ) +
                    labs(x = "Gibbs iteration", y = "Sampled value") +
                    theme_minimal())

  return(the_plot)
}
