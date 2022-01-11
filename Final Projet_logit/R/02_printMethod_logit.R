#' Print Method for S3 class \code{logit}
#' @param x object of class \code{logit}
#' @param ... further arguments to be passed to print
#' @details
#' An object of class "logit" is printed only with certain summarized
#' information. Here one can easily check the coefficient estimates of the logistic
#' regression. Moreover, null deviance and residual deviance
#' are provided for goodness of fit, as well as AIC value.
#'
#' @references
#' R Core Team (2018). R documentation for \code{stats::glm}. R: A language and environment for statistical
#' computing. R Foundation for Statistical Computing, Vienna, Austria.
#' URL https://www.R-project.org/.
#'
#' @examples
#' set.seed(1)
#' y <- rbinom(n = 100, size = 1, prob = 0.5)
#' X <- rnorm(n = 100)
#' data <- data.frame(y = y, X = X)
#' logit(y ~ X, data)

#'#' Call: logit.formula(y ~ X, data)

#'#' Coefficients:
#'#'   (Intercept)           X
#'#' -0.07798872  0.13041887

#'#' Degrees of Freedom:  99  Total (i.e. Null);   98  Residual
#'#' Null Deviance:  138.4694
#'#' Residual Deviance:  138.0995
#'#' AIC:  142.0995
#' @export
print.logit <- function(x, ...) {
  cat("\nCall: ")
  print(x$call)
  cat("\nCoefficients: \n")
  print(x$coefficients)
  cat("\nDegrees of Freedom: ", x$df.null, " Total (i.e. Null);  ",
      x$df.residual, " Residual")
  cat("\nNull Deviance: ", x$null.deviance)
  cat("\nResidual Deviance: ", x$deviance)
  cat("\nAIC: ", x$AIC)
  invisible(x)
}
