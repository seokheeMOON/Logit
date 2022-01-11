#' Summary Method for S3 class \code{logit}
#' @param object object of class \code{logit}
#' @param ... further arguments to be passed to print
#' @details
#' Summary method for class \code{logit} returns not only selected components of
#' object \code{logit} that are important for regression analysis - e.g. null/residual
#' deviance, AIC, degrees of freedom null/residuals, but also a few additional
#' elements like deviance residuals and covariance matrix of the coefficient
#' estimates. Moreover, the \code{coefficients} component provides a matrix of
#' coefficient estimates, their standard errors, z-values and p-values.
#' @return
#' \code{summary.logit} returns an object of class "summary.logit", a list with
#' the following componets:
#'
#' \item{call}{The component from the \code{object}}
#' \item{deviance}{The component from the \code{object}}
#' \item{AIC}{The component from the \code{object}}
#' \item{df.residual}{The component from the \code{object}}
#' \item{null.deviance}{The component from the \code{object}}
#' \item{df.null}{The component from the \code{object}}
#' \item{deviance.resid}{The deviance residuals}
#' \item{coefficients}{The matrix of coefficients, standard errors, z-values and
#'  p-values. Aliased coefficients are omitted.}
#' \item{df}{A vector consisting of the rank of the model and the number of
#' residual degrees of freedom as well as the number of coefficients (including
#' aliased ones).}
#' \item{covariance}{The estimated covariance matrix of the estimated coefficients.}
#'
#' @references
#' R Core Team (2018). R documentation for \code{stats::summary.glm}. R: A language and environment for statistical
#' computing. R Foundation for Statistical Computing, Vienna, Austria.
#' URL https://www.R-project.org/.
#'
#' @export
summary.logit <- function(object, ...) {
  # generate deviance residuals
  deviance.resid <- sign(object$y - 0.5) *
    sqrt(-2 * (object$y * log(object$fitted.values) +
                 (1 - object$y) * log(1 - object$fitted.values)))
  # generate coefficient table with coefficient estimates, their S.E., z values and p values
  beta <- object$coefficients  # coefficients estimates
  se <- se(object$y, object$X)  # standard errors
  zv <- beta / se  # z-values
  pv <- 2 * pnorm(q = -abs(zv))  # p-values
  coefficients <- as.matrix(data.frame(beta, se, zv, pv))
  colnames(coefficients) <- c("Estimate", paste("Std.", "Err."), "z value", "Pr(>|z|)")
  rownames(coefficients) <- colnames(object$X)

  out <- list()
  out$call <- object$call
  out$deviance <- object$deviance
  out$AIC <- object$AIC
  out$df.residual <- object$df.residual
  out$null.deviance <- object$null.deviance
  out$df.null <- object$df.null
  out$deviance.resid <- deviance.resid
  out$coefficients <- coefficients
  out$df <- c(dim(object$X[2]), object$df.residual, dim(object$X_ini)[2])
  out$covariance <- aVar(object$y, object$X, beta = MLE(object$y, object$X))
      # no scaling needed, since the dispersion parameter is set as 1 for binomial family
  class(out) <- "summary.logit"
  return(out)
}

