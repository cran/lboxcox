#' Median exposure-response slope of a fitted logistic Box-Cox model
#'
#' Calculates a number that represents the overall gradient measurement
#' between the (log-scale) predictor and the log-odds of the outcome,
#' evaluated at the survey-weighted mean of \code{log(x)}, along with a
#' Wald-type 95\% confidence interval derived from the model's Hessian.
#'
#' @param formula the formula used to train the logistic Box-Cox model.
#' @param weight_column_name the name of the column in \code{data} containing
#'   the survey weights, a numeric vector of weights, or \code{NULL}/\code{1}
#'   for unweighted analysis.
#' @param data dataframe containing the dataset the model was trained on.
#' @param trained_model the already-trained model, e.g. the output of
#'   \code{\link{lbc_maxlik}} or \code{\link{lbc_train_ms}}. Must expose
#'   \code{$estimate} (with \code{Beta_1} and \code{Lambda}). For a
#'   \code{svyglm} result, the confidence interval is conditional on its
#'   selected lambda.
#' @return a named numeric vector with elements \code{median effect},
#'   \code{lower 95\% ci}, and \code{upper 95\% ci}.
#' @importFrom survey svydesign svymean
#' @importFrom MASS ginv
#' @importFrom stats as.formula terms vcov
#' @export
median_effect <- function(formula, weight_column_name, data, trained_model) {
  primary_predictor_name <- attr(terms(formula), "term.labels")[1]
  m      <- as.formula(paste0("~log(", primary_predictor_name, ")"))
  processed_data <- get_processed_data(formula, data, weight_column_name)
  complete_data <- data[processed_data$row_index, , drop = FALSE]
  mysub <- .make_design(complete_data, processed_data$iw)
  myu    <- unname(svymean(m, mysub)[1])

  beta1  <- unname(trained_model$estimate[2])
  lambda <- unname(trained_model$estimate[3])
  me     <- beta1 * exp((lambda - 1) * myu)

  if (is.matrix(trained_model$hessian) && nrow(trained_model$hessian) >= 3L) {
    covariance <- -ginv(trained_model$hessian)[1:3, 1:3] /
      length(processed_data$iyy)
    varbeta1    <- covariance[2, 2]
    covbeta1lam <- covariance[2, 3]
    varlambda   <- covariance[3, 3]
  } else if (inherits(trained_model, "glm")) {
    # Profile-grid, CV, and ensemble fits treat the selected lambda as fixed.
    coefficient_covariance <- vcov(trained_model)
    varbeta1    <- coefficient_covariance[2, 2]
    covbeta1lam <- 0
    varlambda   <- 0
  } else {
    stop("trained_model does not provide uncertainty information", call. = FALSE)
  }

  d_beta1 <- exp((lambda - 1) * myu)
  d_lambda <- me * myu
  var_me <- d_beta1^2 * varbeta1 +
    2 * d_beta1 * d_lambda * covbeta1lam +
    d_lambda^2 * varlambda
  if (!is.finite(var_me) || var_me < -sqrt(.Machine$double.eps)) {
    stop("could not compute a finite variance for the median effect", call. = FALSE)
  }
  sd_me <- sqrt(max(var_me, 0))

  c("median effect"  = me,
    "lower 95% ci"   = me - 1.96 * sd_me,
    "upper 95% ci"   = me + 1.96 * sd_me)
}
