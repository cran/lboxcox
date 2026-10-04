#' Predict from a fitted logistic Box-Cox model
#'
#' Produces predicted probabilities for \code{newdata} from a model fit by
#' \code{\link{lbc_maxlik}}, \code{\link{lbc_train_ms}}, or the first
#' (\code{$estimate}-bearing) element returned by \code{\link{lbc_train_bagging}}
#' or \code{\link{lbc_train_all}}.
#'
#' @param myMaxLikfit a fitted model with a named \code{$estimate} vector
#'   (\code{Beta_0}, \code{Beta_1}, \code{Lambda}, then covariates).
#' @param newdata data frame of new observations to predict on.
#' @param formula the same formula used to fit \code{myMaxLikfit}.
#' @return a numeric vector of predicted probabilities.
#' @importFrom stats delete.response model.frame model.matrix na.pass plogis predict terms
#' @export
lboxcox_maxLik.predict <- function(myMaxLikfit, newdata, formula) {
  .predict_lboxcox_fit(myMaxLikfit, newdata, formula)
}


#' Predict from one component fit without requiring the response in newdata
#' @noRd
.predict_lboxcox_fit <- function(fit, newdata, formula) {
  estimate <- fit$estimate
  if (is.null(estimate) || !all(c("Beta_0", "Beta_1", "Lambda") %in% names(estimate))) {
    stop("fit does not contain a valid named estimate vector", call. = FALSE)
  }

  ixx_col <- attr(terms(formula), "term.labels")[1]
  if (!ixx_col %in% names(newdata)) {
    stop("primary predictor not found in newdata: ", ixx_col, call. = FALSE)
  }
  newdata.trans <- box_cox_new(formula, newdata, newdata[[ixx_col]], estimate["Lambda"])

  if (inherits(fit, "glm")) {
    return(as.numeric(predict(fit, newdata = newdata.trans, type = "response")))
  }

  rhs_terms <- delete.response(terms(formula))
  model_frame <- model.frame(
    rhs_terms, data = newdata.trans, na.action = na.pass,
    xlev = fit$lbc_xlevels
  )
  if (length(fit$lbc_contrasts)) {
    model_matrix <- model.matrix(
      rhs_terms, data = model_frame, contrasts.arg = fit$lbc_contrasts
    )
  } else {
    model_matrix <- model.matrix(rhs_terms, data = model_frame)
  }
  coefficients <- estimate[-match("Lambda", names(estimate))]
  if (ncol(model_matrix) != length(coefficients)) {
    stop("newdata model matrix does not match the fitted coefficients", call. = FALSE)
  }
  as.numeric(plogis(model_matrix %*% unname(coefficients)))
}


#' Predict from a bootstrap-ensemble logistic Box-Cox model
#'
#' Averages predicted probabilities across the (up to 100) per-resample fits
#' returned as the second element of \code{\link{lbc_train_bagging}} or
#' \code{\link{lbc_train_all}}, skipping resamples that failed to converge.
#'
#' @param myMaxLik_elfit the length-3 list returned by
#'   \code{\link{lbc_train_bagging}} or \code{\link{lbc_train_all}}.
#' @param newdata data frame of new observations to predict on.
#' @param formula the same formula used to fit \code{myMaxLik_elfit}.
#' @return a numeric vector of predicted probabilities, averaged over the
#'   converged bootstrap resamples.
#' @importFrom stats plogis terms
#' @export
lboxcox_maxLik_el.predict <- function(myMaxLik_elfit, newdata, formula) {
  fits <- myMaxLik_elfit[[2]]
  p_mat <- matrix(NA_real_, nrow(newdata), length(fits))

  for (ii in seq_along(fits)) {
    fit_ii <- fits[[ii]]
    if (!is.list(fit_ii)) next
    pred <- try(.predict_lboxcox_fit(fit_ii, newdata, formula), silent = TRUE)
    if (!inherits(pred, "try-error") && length(pred) == nrow(newdata)) {
      p_mat[, ii] <- pred
    }
  }

  valid_cols <- apply(p_mat, 2, function(col) any(!is.na(col)))
  if (!any(valid_cols)) {
    return(rep(NA_real_, nrow(newdata)))
  }
  apply(p_mat[, valid_cols, drop = FALSE], 1, mean, na.rm = TRUE)
}
