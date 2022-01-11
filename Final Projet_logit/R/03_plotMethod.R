#' Plot method for S3 class \code{logit}
#' @param x object of class \code{logit}
#' @param ... further arguments to be passed to plot
#' @details
#' For objects of class \code{logit}, plot method returns 4 plots in total, namely
#' Regression result, Residuals vs. Fitted, Normal Q-Q and Scale-Location plot.
#' @references
#' R Core Team (2018). R documentation for \code{stats::plot.lm}. R: A language and environment for statistical
#' computing. R Foundation for Statistical Computing, Vienna, Austria.
#' URL https://www.R-project.org/.
#' @export
plot.logit <- function(x, ...){

  # get deviance residual from summary.logit object
  deviance.resid <- summary(x)$deviance.resid
  pearson.resid <- (x$y - x$fitted.values) / sqrt(x$fitted.values * (1 - x$fitted.values))

  par(mar = c(2,4,2,1), oma = c(0, 0, 2, 0))
  par(ask = TRUE)

  # 1st plot: Box plots: Regressor ~ dep. var.
  par(mfrow = c(dim(x$X)[2] %/% 3 + 1, 3))
  Box <- list()
  for (i in 1:dim(x$X)[2]) {
    Box[[i]] <- boxplot(x$X[, i] ~ x$y, main = colnames(x$X)[i])
  }
  Box
  mtext("Box plots: Regressor ~ dep. var.", outer = TRUE)

  readline(prompt = "Pause. Press <Enter> to continue...")

  oldPar <- par(mfrow = c(2, 2))

  # 2nd plot: Regression Fit
  plot(x$linear.predictors, x$y, pch = 20,
       main = "Regression Fit",
       xlab = "linear predictors (Xb)",
       ylab = "true/fitted value")
  lines(sort(x$linear.predictors),
        x$fitted.values[order(x$linear.predictors)],
        type = "l", col = "blue")
  legend(x = min(x$linear.predictors), y = 0.8,
         legend = c("true value", "fitted value"), col = c("black", "blue"),
         pch = 20, bty = "n", cex = 0.8)

  # 3rd plot: Residuals vs Fitted
  plot(x$linear.predictors, deviance.resid,
       xlab = "Predicted values", ylab = "Residuals",
       main = "Residuals vs Fitted")
  abline(h = 0, col = "gray", lty = 3)
  lines(lowess(x$linear.predictors, deviance.resid), col = "red")

  # 4th plot: Normal Q-Q
  qqnorm(deviance.resid,
         main = "Normal Q-Q",
         xlab = "Theoretical Quantiles",
         ylab = "Std. deviance resid.")
  qqline(deviance.resid, lty = 3)

  # 5th plot: Scale-Location
  plot(x$linear.predictors, sqrt(abs(deviance.resid)),
       ylim = c(0, max(sqrt(abs(deviance.resid)))),
       main = "Scale-Location",
       xlab = "Predicted values",
       ylab = expression(sqrt(abs("Std. deviance resid"))))
  lines(lowess(x$linear.predictors, sqrt(abs(deviance.resid))), col = "red")


}



