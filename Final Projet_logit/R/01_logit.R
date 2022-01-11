#' Logistic Regression
#'
#' Running a logistic regression given binary dependent variable
#' @param ... arguments differ for each method. Either a formula with a data frame
#' or a numeric design matrix and response vector.
#' @return
#' \code{logit} returns an object of class "logit", a list with
#' the following componets:
#'
#' \item{coefficients}{a named vector of coefficients}
#' \item{linear.predictors}{linear fit}
#' \item{fitted.values}{the fitted mean values or fitted conditional probability.}
#' \item{residuals}{the working residuals.}
#' \item{AIC}{A version of Akaike's An Information Criterion, minus twice
#' the maximized log-likelihood plus twice the number of parameters.}
#' \item{call}{the matched call}
#' \item{df.null}{the residual degrees of freedom for the null model, i.e. n - 1}
#' \item{df.residual}{the residual degrees of freedom, i.e. n - p}
#' \item{null.deviance}{The deviance for the null model, minus two times the
#' log-likelihood of the null model. This is to compare with \code{deviance}.}
#' \item{deviance}{minus two times the log-likelihood of the saturated model}
#' \item{model}{the model frame}
#' \item{y}{the binary response vector}
#' \item{X}{the model matrix used for the regression, aliased variables omitted.}
#' \item{X_ini}{initial model matrix given either by formula or as matrix,
#' including aliased variables}
#' \item{formula}{The given formula in case of calling \code{logit} using an
#' argument of class \code{formula}}
#' @details
#' \code{logit} can perform with inputs of class \code{formula} and
#' \code{data.frame} like \code{logit(response ~ terms, data = data.frame)}
#' as well as with numeric response vector and design matrix like
#' \code{logit(X, y)}. Terms in the formula can be specified using '+',
#' ':'(interaction terms), and also '*'(cross terms).
#'
#' When calling \code{logit} with formula, the intercept term will be automatically
#' added, which can be taken out by adding "- 1 (or any constant)" in the formula
#' (@seealso model.matrix).
#'
#' If there exist two terms with exact same entries, one of them is going to be
#' automatically deleted to avoid multicollinearity which would cause unstable
#' estimates.
#'
#' @references
#' R Core Team (2018). R documentation for stats::glm. R: A language and environment for statistical
#' computing. R Foundation for Statistical Computing, Vienna, Austria.
#' URL https://www.R-project.org/.
#'
#' @export
logit <- function(...) UseMethod("logit")

#' @rdname logit
#' @param X a design matrix of dimension n * p
#' @param y a vector of observation of length n
#' @examples
#'#' set.seed(1)
#' y <- rbinom(n = 100, size = 1, prob = 0.5)
#' X <- cbind(rep(1, length(y)), rnorm(n = 100))
#' colnames(X) <- c("(Intercept)", "X")
#' logit(X, y)$coefficients
#'#'                    [,1]
#'#' (Intercept) -0.07798872
#'#' X            0.13041887
#' @export
logit.numeric <- function(X, y, ...){

  # Initial design matrix X_ini (including aliased variables)
  X_ini <- as.data.frame(X)
  # Final design matrix X (aliased variables omitted)
  X <- X_ini[!duplicated(as.list(X_ini))]
  X <- as.matrix(X)

  # Generate the output object: out
  out <- list()
  beta <- as.vector(MLE(y, X))  # coefficients estimates
  names(beta) <- colnames(X)

  out$coefficients <- beta

  out$linear.predictors <- X %*% beta

  out$fitted.values <- yHat(y, X, beta)

  out$residuals <- (y - out$fitted.values) / out$fitted.values / (1 - out$fitted.values)

  out$AIC <- aic(y, X, beta)

  out$call <- sys.call()

  out$df.null <- dim(X)[1] - 1

  out$df.residual <- dim(X)[1] - dim(X)[2]

  out$null.deviance <- -2 * sum(y) * log(mean(y) / (1 - mean(y))) - 2 *
    length(y) * log(1 - mean(y))

  out$deviance <- -2 * logL(y, X, beta = beta)

  out$model <- cbind(y, X)

  out$y <- y

  out$X <- X

  out$X_ini <- X_ini



  # Set the class of the output object as logit
  class(out) <- "logit"
  return(out)
}

#' @rdname logit
#' @param formula an object of class "formula" (or one that can be coerced
#' to that class): a symbolic description of the model to be fitted.
#' @param data an optional data frame, list or environment (or object
#' coercible by as.data.frame to a data frame) containing the variables in the
#' model. If not found in data, the variables are taken from environment(formula),
#' typically the environment from which lm is called.
#' @param na.action a function to handle NAs in the data. The default is set as
#' na.fail
#' @examples
#' #' set.seed(1)
#' y <- rbinom(n = 100, size = 1, prob = 0.5)
#' X <- rnorm(n = 100)
#' data <- data.frame(y = y, X = X)
#' logit(y ~ X, data)$coefficients
#'#'                     [,1]
#'#'  (Intercept) -0.07798872
#'#'  X            0.13041887
#' @export
logit.formula <- function(formula, data, na.action = na.fail, ...){

  # model frame
  model <- model.frame(formula, data = data, na.action = na.action)
  # Binary dependent variable y
  y <- model[, 1]
  # Initial design matrix X_ini (including aliased variables)
  X <- model.matrix(formula, data = model)

  out <- logit(X, y)
  out$call <- sys.call()
  out$formula <- formula
  out$model <- model

  return(out)
}



#' @rdname logit
#' @param others objects that is neither of class formula nor of numeric matrix
#' @export
logit.default <- function(others, ...){
  cat("For `", class(others), "` no logit method is implemented.\n", sep = "")
}


# Function to estimate E[y|x]: yHat
yHat <- function(y, X, beta) {
  p <- c()
  for (i in 1:length(y)) {
    p[i] <- exp(t(X[i, ]) %*% beta) / (1 + exp(t(X[i, ]) %*% beta))
  }
  return(p)
}

# Function to calculate the sample Log-Likelihood: logL
logL <- function(y, X, beta) {
  p <- yHat(y, X, beta)
  lL <- sum(y * log(p / (1 - p)) + log(1 - p))
  return(lL)
}

# - Estimated Hessian matrix: H
#   Since Hessian matrix of logit MLE is always negative definite,
#   we are using - Hessian matrix for convenience
H <- function(y, X, beta) {
  p <- yHat(y, X, beta)
  H_0 <- list()
  for (i in 1:length(y)) {
    H_0[[i]] <- p[i] * (1 - p[i]) * X[i, ] %*% t(X[i, ])
  }

  H <- matrix(0, ncol = dim(X)[2], nrow = dim(X)[2])
  for (i in 1:length(y)) {
    H <- H + H_0[[i]]
  }
  return(H)
}


# Estimation of MLE of beta
MLE <- function(y, X, tol = 1e-8) {
  beta <- rep(0, dim(X)[2]) # initial value of beta
  p <- yHat(y, X, beta)
  lL <- logL(y, X, beta)

  repeat {
    # FoC: First order derivative
    FoC_0 <- list()
    for (i in 1:length(y)) {
      FoC_0[[i]] <- (y[i] - p[i]) * X[i, ]
    }

    FoC <- rep(0, dim(X)[2])
    for (i in 1:length(y)) {
      FoC <- FoC + FoC_0[[i]]
    }
    # - SoC: - Second order derivative = - estimated Hessian matrix
    HessMatrix <- H(y, X, beta)

    # Newly-estimated beta by Gauss-Newton Method (iterative): beta
    beta <- beta + solve(HessMatrix, tol = 0) %*% FoC

    # Newly-estimated y (iterative): p = E[y|x] = P(y|x)
    p <- yHat(y, X, beta)

    # Newly-estimated log-Likelihood (iterative): lL_New
    lL_New <- logL(y, X, beta)

    # Stop the iteration if the progress in the log-Likelihood is smaller than
    #    the pre-set tolerance level
    # Continue the interation if the progress is significant
    if (abs(lL_New - lL) < tol) {
      break
    } else {
      lL <- lL_New
    }
  }
  return(beta)
}


# Estimation of the (asymptotic) variance covariance matrix of the MLE beta:
#
aVar <- function(y, X, beta = MLE(y, X)){
  p <- yHat(y, X, beta)

  # Asymptotic Variance-Covariance matrix of the MLE beta
  aVar <- solve(H(y, X, beta), tol = 0)
  return(aVar)
}



# S.E. of the MLE beta
se <- function(y, X, beta = MLE(y, X)) {
  aVar <- aVar(y, X, beta = MLE(y, X))
  # Asymptotic S.E. of MLE beta
  se <- sqrt(diag(aVar))
  return(se)
}

# AIC
aic <- function(y, X, beta) {
  lL <- logL(y, X, beta)
  aic <- -2 * lL + 2 * dim(X)[2]
  return(aic)
}

