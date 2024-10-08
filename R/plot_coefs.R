#' Posterior predictive check for walker object
#' 
#' Plots sample quantiles from posterior predictive sample. 
#' See [bayesplot::ppc_ribbon()] for details.
#' 
#' @importFrom dplyr group_by summarise
#' @importFrom rlang .data
#' @importFrom stats quantile time update.formula drop.terms
#' @import ggplot2
#' @param object An output from [walker()].
#' @param level Level for intervals. Default is 0.05, leading to 90% intervals.
#' @param alpha Transparency level for [ggplot2::geom_ribbon()].
#' @param transform Optional vectorized function for transforming the coefficients (for example `exp`).
#' @param scales Should y-axis of the panels be `"fixed"` (default) or `"free"`?
#' @param add_zero Logical, should a dashed line indicating a zero be included?
#' @export
plot_coefs <- function(object, level = 0.05, alpha = 0.33, transform = identity, scales = "fixed", add_zero = TRUE){
  
  # N x k x n array
  coef_data <- transform(extract(object$stanfit, pars = "beta_rw", permuted = TRUE)$beta)
  if (object$distribution != "gaussian") {
    coef_data <- coef_data[sample(1:nrow(coef_data), size = nrow(coef_data), replace = TRUE, 
      prob = extract(object$stanfit, pars = "weights", permuted = TRUE)$weights), , , drop = FALSE]
  }
  dimnames(coef_data) <- 
    list(iter = 1:nrow(coef_data), 
      beta = colnames(object$xreg_rw), 
      time = as.numeric(time(object$y)))
  coef_data <- as.data.frame(as.table(coef_data))  
  names(coef_data)[4] <- "value"
  coef_data$time <- as.numeric(levels(coef_data$time))[coef_data$time]
  quantiles <- summarise(group_by(coef_data, time, beta),
      lwr = quantile(.data$value, prob = level), 
      median = quantile(.data$value, prob = 0.5),
      upr = quantile(.data$value, prob = 1 - level))
  
  p <- ggplot(
    data = quantiles,
    mapping = aes(
      x = .data$time,
      y = .data$median,
      ymin = .data$lwr,
      ymax = .data$upr
    )
  )  + facet_wrap(~beta, scales = scales) + 
    geom_ribbon(aes_(color = "beta", fill = "beta"),
      alpha = alpha, linetype = 0) +
    geom_line(aes_(color = "beta")) +
    labs(y = NULL) + theme(legend.position = "none") + 
    scale_color_manual(
      name = "",
      values = c(beta = color_scheme_get()[[2]])
    ) +
    scale_fill_manual(
      name = "",
      values = c(beta = color_scheme_get()[[1]])
    ) 
  if (add_zero) p <- p + geom_hline(yintercept = 0, linetype = "dashed")
  p
}
