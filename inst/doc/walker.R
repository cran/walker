## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(walker)

## ----example-------------------------------------------------------------
set.seed(1)
n <- 100
beta1 <- cumsum(c(0.5, rnorm(n - 1, 0, sd = 0.05)))
beta2 <- cumsum(c(-1, rnorm(n - 1, 0, sd = 0.15)))
x1 <- n:1 / 10
x2 <- cos(1:n)
u <- cumsum(rnorm(n, 0, 0.5))
ts.plot(cbind(u, beta1 * x1, beta2 * x2), col = 1:3)

## ----observations--------------------------------------------------------
signal <- u + beta1 * x1 + beta2 * x2
y <- rnorm(n, signal, 0.5)
ts.plot(signal)
lines(y, col = 2)

## ----walker--------------------------------------------------------------
kalman_walker <- walker(y ~ x1 + x2, refresh = 0, chains = 2,
  beta_prior = cbind(0, rep(5, 3)), sigma_prior = cbind(0, rep(2, 4)))
print(kalman_walker, pars = c("sigma_y", "sigma_b"))
library("rstan")
stan_plot(kalman_walker, pars = c("sigma_y", "sigma_b"))

## ----plot_betas----------------------------------------------------------
betas <- summary(kalman_walker, "beta")$summary

ts.plot(cbind(u, beta1, beta2, 
  matrix(betas[, c("mean", "2.5%", "97.5%")], ncol = 9)),
  col = c(1:3, rep(1:3, 3)), lty = rep(1:2, times = c(3, 9)))

## ----naive---------------------------------------------------------------
naive_walker <- walker(y ~ x1 + x2, seed = 1, refresh = 0, chains = 2,
  beta_prior = cbind(0, rep(5, 3)), sigma_prior = cbind(0, rep(2, 4)),
  naive = TRUE, control = list(adapt_delta = 0.9, max_treedepth = 15))
print(naive_walker, pars = c("sigma_y", "sigma_b"))

sum(get_elapsed_time(kalman_walker))
sum(get_elapsed_time(naive_walker))

## ----ppc-----------------------------------------------------------------
y_rep <- summary(kalman_walker, "y_rep")$summary
ts.plot(y_rep[, c("mean", "2.5%", "97.5%")], lty = c(1, 2, 2))
lines(y, col = 2)

## ----prediction----------------------------------------------------------
original_data <- data.frame(y = head(y, 95), x1 = head(x1, 95), x2 = head(x2, 95))
new_data <- data.frame(x1 = tail(x1, 5), x2 = tail(x2, 5))
walker_predict <- walker(y ~ x1 + x2, data = original_data, newdata = new_data, 
  iter = 2000, chains = 1, seed = 1, refresh = 0,
  beta_prior = cbind(0, rep(2, 3)), sigma_prior = cbind(0, rep(2, 4)))
intervals <- summary(walker_predict, pars = "y_new")$summary[, c("mean", "2.5%", "97.5%")]
ts.plot(ts(y), ts(intervals, start = 96),
  col = c(1, 2, 2, 2), lty = c(1, 1, 2, 2))

