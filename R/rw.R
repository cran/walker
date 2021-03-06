#' Construct a first-order random walk component 
#' 
#' Auxiliary function used inside of the formula of \code{walker}.
#' 
#' @export
#' @param formula Formula for RW1 part of the model. Only right-hand-side is used. 
#' @param data Optional data.frame.
#' @param beta A length vector of length two which defines the 
#' prior mean and standard deviation of the Gaussian prior for coefficients at time 1.
#' @param sigma A vector of length two, defining the Gamma prior for 
#' the coefficient level standard deviation. First element corresponds to the shape parameter and 
#' second to the rate parameter. Default is Gamma(2, 0.0001).
#' @param gamma An optional vector defining a damping of the random walk noises. More specifically, 
#' the variance of the conditional distribution of state_t+1 given state is of form gamma_t * sigma.
rw1 <- function(formula, data, beta, sigma = c(2, 0.0001), gamma = NULL) {
 
  mf <- match.call(expand.dots = FALSE)
  mf <- mf[c(1L, match(c("formula", "data"), names(mf), 0L))]
  mf$drop.unused.levels <- TRUE
  mf$na.action <- as.name("na.pass")
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  xreg <- model.matrix(attr(mf, "terms"), mf)
  
  if(length(beta) != 2) {
    stop("beta should be a vector of length two, defining the mean and standard deviation for the Gaussian prior of coefficients. ")
  }
  if(length(sigma) != 2) {
    stop("sigma should be should be a vector of length two, defining the shape and rate parameter for the Gamma prior of standard deviations. ")
  }
  n <- nrow(xreg)
  if (is.null(gamma)) {
    gamma <- matrix(1, ncol(xreg), n) 
  } else {
    if (ncol(gamma) != n) 
      stop("The number of column of gamma matrix for 'rw1' should equal to the number of observations. ")
    if (!is.numeric(gamma) | any(gamma < 0 | is.na(gamma))) 
      stop("Argument 'gamma' should be numeric matrix of nonnegative values. ")
  } 
  list(xreg = xreg, beta = beta, 
    sigma = sigma, gamma = gamma)
  
}
#' Construct a second-order random walk component 
#' 
#' Auxiliary function used inside of the formula of \code{walker}.
#' 
#' @export
#' @param formula Formula for RW2 part of the model. Only right-hand-side is used. 
#' @param data Optional data.frame.
#' @param beta A vector of length two which defines the 
#' prior mean and standard deviation of the Gaussian prior for coefficients at time 1.
#' @param sigma A vector of length two, defining the Gamma prior for 
#' the slope level standard deviation. First element corresponds to the shape parameter and 
#' second to the rate parameter. Default is Gamma(2, 0.0001).
#' @param nu A vector of length two which defines the 
#' prior mean and standard deviation of the Gaussian prior for the slopes nu at time 1.
#'@param gamma An optional vector defining a damping of the slope level noises. More specifically, 
#' the variance of the conditional distribution of state_t+1 given state is of form gamma_t * sigma.
#' @export
rw2 <- function(formula, data, beta, sigma = c(2, 0.0001), nu, gamma = NULL) {
  
  mf <- match.call(expand.dots = FALSE)
  mf <- mf[c(1L, match(c("formula", "data"), names(mf), 0L))]
  mf$drop.unused.levels <- TRUE
  mf$na.action <- as.name("na.pass")
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  xreg <- model.matrix(attr(mf, "terms"), mf)
  
  if(length(beta) != 2) {
    stop("beta should be a vector of length two, defining the mean and standard deviation for the Gaussian prior of initial coefficients. ")
  }
  if(length(sigma) != 2) {
    stop("sigma should be should be a vector of length two, defining the shape and rate for the Gamma prior of standard deviations. ")
  }
  if(length(nu) != 2) {
    stop("nu should be should be a vector of length two, defining the mean and standard deviation for the Gaussian prior of initial slope coeffients. ")
  }
  n <- nrow(xreg)
  if (is.null(gamma)) {
    gamma <- matrix(1, ncol(xreg), n)
  } else {
    if (ncol(gamma) != n) 
      stop("The number of column of gamma matrix for 'rw1' should equal to the number of observations. ")
    if (!is.numeric(gamma) | any(gamma < 0 | is.na(gamma))) 
      stop("Argument 'gamma' should be numeric matrix of nonnegative values. ")
  } 
  list(xreg = xreg, beta = beta, 
    sigma = sigma, nu = nu, gamma = gamma)
  
}