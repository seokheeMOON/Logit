#' Print Method for S3 class \code{summary.logit}
#' @param x object of class \code{summary.logit}
#' @param ... further arguments to be passed to print
#' @details
#' Print method for \code{summary.logit} object provides a neat summary of the
#' logistic regression of class \code{logit}. This includes the call of the
#' logistic regression, summary of the deviance residuals, a matrix containing
#' coefficient estimates/ S.E./ z-values/ p-values with significance stars,
#' as well as null/residual deviance and AIC.
#'
#' @references
#' R Core Team (2018). R documentation for \code{stats::summary.glm}. R: A language and environment for statistical
#' computing. R Foundation for Statistical Computing, Vienna, Austria.
#' URL https://www.R-project.org/.
#'
#' @examples
#' set.seed(1)
#' y <- rbinom(n = 100, size = 1, prob = 0.5)
#' X <- rnorm(n = 100)
#' data <- data.frame(y = y, X = X)
#' summary(logit(y ~ X, data))
#'
#'#' Call:
#'#'   logit.formula(y ~ X, data)
#'#'
#'#' Deviance Residuals:
#'#'   Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
#'#' -1.27880 -1.13757 -1.06205 -0.01309  1.20700  1.31306
#'#'
#'#' Coefficients:
#'#'               Estimate Std. Err. z value Pr(>|z|)
#'#' (Intercept) -0.077989  0.200555 -0.3889   0.6974
#'#' X            0.130419  0.214935  0.6068   0.5440
#'#'
#'#' Null Deviance:  138.4694  on   99  degrees of freedom
#'#'
#'#' Residual Deviance:  138.0995  on   98  degrees of freedom
#'#'
#'#' AIC:  142.0995
#' @export
print.summary.logit <- function(x, ...) {
  cat("\nCall: \n")
  print(x$call)
  cat("\nDeviance Residuals: \n")
  print(summary(x$deviance.resid))
  cat("\nCoefficients: \n")
  printCoefmat(x$coefficients)
  cat("\n    Null Deviance: ", x$null.deviance, " on  ", x$df.null, " degrees of freedom\n")
  cat("\nResidual Deviance: ", x$deviance, " on  ", x$df.residual, " degrees of freedom\n")
  cat("\nAIC: ", x$AIC)
}

