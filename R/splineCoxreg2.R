#' @title Fitting the five-parameter spline Cox model with a specified shape, selecting the best fit
#' @description
#' \code{splineCox.reg2} estimates the parameters of a five-parameter spline Cox model for multiple specified shapes
#' and selects the best fitting model based on the minimization of the log-likelihood function.
#' The function calculates the estimates for the model parameters (beta) and the baseline hazard scale parameter (lambda), using non-linear optimization.
#' @export
#' @importFrom stats nlm
#' @import joint.Cox
#' @param t.event a vector for time-to-event
#' @param event a vector for event indicator (=1 event; =0 censoring)
#' @param Z a matrix for covariates; nrow(Z)=sample size, ncol(Z)=the number of covariates
#' @param xi1 lower bound for the hazard function; the default is min(t.event)
#' @param xi3 upper bound for the hazard function; the default is max(t.event)
#' @param model A character vector specifying which model shapes to consider for the baseline hazard.
#'              Available options are:
#'              "increase", "constant", "decrease", "unimodal1", "unimodal2", "unimodal3", "bathtub1", "bathtub2", "bathtub3".
#'              Default is \code{names(shape.list)} which includes all available models.
#' @param p0 Initial values to maximize the likelihood (1 + p parameters; baseline hazard scale parameter and p regression coefficients)
#' @return A list containing the following components:
#'   \item{model}{The baseline hazard shape that resulted in the best fit}
#'   \item{parameter}{The specified shape parameter that resulted in the best fit}
#'   \item{beta}{A named vector with the estimates, standard errors, and 95% confidence intervals for the regression coefficients}
#'   \item{lambda}{A named vector with the estimate, standard error, and 95% confidence interval for the baseline hazard parameter}


splineCox.reg2 <- function (t.event, event, Z, xi1 = min(t.event), xi3 = max(t.event),
                            model =  names(shape.list), p0 = rep(0, 1 + p))
{
  shape.list = list(increase  = c(0.05, 0.1, 0.15, 0.3, 0.4),
                    constant  = c(0.125, 0.25, 0.25, 0.25, 0.125),
                    decrease  = c(0.4, 0.3, 0.15, 0.1, 0.05),
                    unimodal1 = c(0.001, 0.014, 0.97, 0.014, 0.001),
                    unimodal2 = c(0.001, 0.8, 0.124, 0.074, 0.001),
                    unimodal3 = c(0.001, 0.074, 0.124, 0.8, 0.001),
                    bathtub1  = c(0.3, 0.1995, 0.001, 0.1995, 0.3),
                    bathtub2  = c(0.3, 0.001, 0.1009, 0.299, 0.3),
                    bathtub3  = c(0.3, 0.299, 0.1009, 0.001, 0.3))
  d = event
  Z = as.matrix(Z)
  p = ncol(Z)
  l.func = function(phi,para) {
    b = phi[2:(1 + p)]
    g = exp(pmin(phi[1], 500))
    r = as.vector(M.spline(t.event, xi1 = min(t.event), xi3 = max(t.event)) %*%
                    (g * para))
    R = as.vector(I.spline(t.event, xi1 = min(t.event), xi3 = max(t.event)) %*%
                    (g * para))
    bZ = as.numeric(Z %*% b)
    l = sum(d * (log(r) + bZ))
    l = l - sum(pmin(exp(bZ) * R, exp(500)))
    -l
  }

  result <- lapply(names(shape.list[model]), function(name) {
    para <- shape.list[[name]]
    nlm(l.func, p = p0, hessian = TRUE, para = para)
  })

  min.l <- min(sapply(result, function(res) res$minimum))
  best.index <- which.min(sapply(result, function(res) res$minimum))
  best.fit <- names(shape.list[model])[best.index]
  res <- unlist(result[best.index], recursive = FALSE)

  beta.est = res$est[2:(1 + p)]
  lam.est  = exp(res$est[1])
  H        = -res$hessian
  V        = solve(-H, tol = 10^(-100))
  beta.se  = sqrt(diag(V)[2:(1 + p)])
  lam.se   = sqrt(lam.est %*% V[1, 1] %*% lam.est)
  b.lower  = beta.est - 1.96 * beta.se
  b.upper  = beta.est + 1.96 * beta.se
  l.lower  = lam.est - 1.96 * lam.se
  l.upper  = lam.est + 1.96 * lam.se
  beta.res = c(estimate = beta.est, SE = beta.se, Lower = b.lower, Upper = b.upper)
  lam.res  = c(estimate = lam.est,  SE = lam.se,  Lower = l.lower, Upper = l.upper)
  list(model = best.fit, parameter = unname(unlist(shape.list[best.fit])),
       beta  = beta.res, lambda = lam.res)
}
