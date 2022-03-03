## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(walker)

## ----example------------------------------------------------------------------
set.seed(1)
n <- 100
beta1 <- cumsum(c(0.5, rnorm(n - 1, 0, sd = 0.05)))
beta2 <- cumsum(c(-1, rnorm(n - 1, 0, sd = 0.15)))
x1 <- rnorm(n, mean = 2)
x2 <- cos(1:n)
rw <- cumsum(rnorm(n, 0, 0.5))
ts.plot(cbind(rw, beta1 * x1, beta2 * x2), col = 1:3)

## ----observations-------------------------------------------------------------
signal <- rw + beta1 * x1 + beta2 * x2
y <- rnorm(n, signal, 0.5)
ts.plot(cbind(signal, y), col = 1:2)

## ----walker-------------------------------------------------------------------
set.seed(1)
fit <- walker(y ~ -1 + rw1(~ x1 + x2, beta = c(0, 10), sigma = c(2, 10)), 
  refresh = 0, chains = 1, sigma_y = c(2, 1))

## ----pars---------------------------------------------------------------------
print(fit$stanfit, pars = c("sigma_y", "sigma_rw1"))
library(bayesplot)
mcmc_areas(as.matrix(fit$stanfit), regex_pars = c("sigma_y", "sigma_rw1"))

## ----plot_with_true_betas-----------------------------------------------------
betas <- summary(fit$stanfit, "beta_rw")$summary[, "mean"]

ts.plot(cbind(rw, beta1, beta2, matrix(betas, ncol = 3)),
  col = rep(1:3, 2), lty = rep(1:2, each = 3))

## ----plot_pretty_betas--------------------------------------------------------
plot_coefs(fit, scales = "free") + ggplot2::theme_bw()

## ----ppc----------------------------------------------------------------------
pp_check(fit)

## -----------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
fitted <- fitted(fit) # estimates given our actual observed data
newdata <- data.frame(x1 = c(x1[1:59], rep(0, 41)), x2 = x2)
pred_x1_1 <- predict_counterfactual(fit, newdata, type = "mean")

cbind(as.data.frame(rbind(fitted, pred_x1_1)), 
    type = rep(c("observed", "counterfactual"), each = n), time = 1:n) %>% 
    ggplot(aes(x = time, y = mean)) + 
    geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`, fill = type), alpha = 0.2) + 
    geom_line(aes(colour = type)) + theme_bw()

## ----prediction---------------------------------------------------------------
new_data <- data.frame(x1 = rnorm(10, mean = 2), x2 = cos((n + 1):(n + 10)))
pred <- predict(fit, new_data)
plot_predict(pred)

## ----walker_rw2---------------------------------------------------------------
fit_rw2 <-walker(y ~ -1 + 
    rw2(~ x1 + x2, beta = c(0, 10), sigma = c(2, 0.001), nu = c(0, 10)), 
  refresh = 0, init = 0, chains = 1, sigma_y = c(2, 0.001))
plot_coefs(fit_rw2, scales = "free") + ggplot2::theme_bw()

## ----naive, eval = FALSE------------------------------------------------------
#  set.seed(1) # set seed to simulate same initial values for both methods
#  naive_fit <- walker_rw1(y ~ x1 + x2, refresh = 0,
#    chains = 2, cores = 2, iter = 1e4,
#    beta = cbind(0, rep(5, 3)), sigma = cbind(0, rep(2, 4)),
#    naive = TRUE,
#    control = list(adapt_delta = 0.999, max_treedepth = 12))
#  
#  set.seed(1)
#  kalman_fit <- walker_rw1(y ~ x1 + x2, refresh = 0,
#    chains = 2, cores = 2, iter = 1e4,
#    beta = cbind(0, rep(5, 3)), sigma = cbind(0, rep(2, 4)),
#    naive = FALSE)

## ----naive-run, eval = FALSE, echo = FALSE------------------------------------
#  # actual code run, remove betas in order to reduce size of the package
#  set.seed(1)
#  naive_fit <- walker_rw1(y ~ x1 + x2, refresh = 0,
#    chains = 2, cores = 2, iter = 1e4,
#    beta = cbind(0, rep(5, 3)), sigma = cbind(0, rep(2, 4)),
#    naive = TRUE, save_warmup = FALSE,
#    pars = c("sigma_y", "sigma_b"),
#    control = list(adapt_delta = 0.999, max_treedepth = 12))
#  
#  set.seed(1)
#  kalman_fit <- walker_rw1(y ~ x1 + x2, refresh = 0,
#    chains = 2, cores = 2, iter = 1e4,
#    beta = cbind(0, rep(5, 3)), sigma = cbind(0, rep(2, 4)),
#    naive = FALSE, save_warmup = FALSE,
#    pars = c("sigma_y", "sigma_b"))
#  
#  save(naive_fit, kalman_fit, file = "vignette_results.rds")

## ---- echo = FALSE------------------------------------------------------------
load("vignette_results.rds")

## ----warnings-and-time--------------------------------------------------------
check_hmc_diagnostics(naive_fit$stanfit)
check_hmc_diagnostics(kalman_fit$stanfit)

get_elapsed_time(naive_fit$stanfit)
get_elapsed_time(kalman_fit$stanfit)

## ----main-results-------------------------------------------------------------
print(naive_fit$stanfit, pars = c("sigma_y", "sigma_b"))
print(kalman_fit$stanfit, pars = c("sigma_y", "sigma_b"))

