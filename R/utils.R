#' Deviance residual prediction error
#'
#' Computes the sum of absolute signed deviance residuals between binary
#' outcomes and predicted probabilities. Used as the cross-validation
#' criterion in \code{\link{lboxcox_cv.fit}} and, more generally, as a
#' prediction-error summary for comparing fitted models.
#'
#' @param y.bin binary (0/1) outcome vector.
#' @param y.pred predicted probabilities, same length as \code{y.bin}.
#' @return a single numeric value: the sum of absolute deviance residuals.
#' @export
devr <- function(y.bin, y.pred) {
  y.pred <- pmin(pmax(y.pred, 1e-10), 1 - 1e-10)  # clip log(0)
  deviance_r <- ifelse(y.bin == 1,
                       sqrt(-2 * log(y.pred)),
                       -sqrt(-2 * log(1 - y.pred)))

  infidx  <- which(!is.finite(deviance_r))
  maxn    <- max(abs(deviance_r[is.finite(deviance_r)]), na.rm = TRUE)
  deviance_r[infidx[deviance_r[infidx] >  0]] <-  maxn * 1.1
  deviance_r[infidx[deviance_r[infidx] <= 0]] <- -maxn * 1.1

  sum(abs(deviance_r))
}


#' Check convergence of the maxBFGS/constrOptim2 path
#'
#' Unlike maxNR, the BFGS path follows optim-style return codes: only zero
#' denotes successful convergence; code one is the iteration limit.
#' @param result an object returned by code{maxLik(..., method = "BFGS")}.
#' @return a single logical value.
#' @noRd
.is_bfgs_success <- function(result) {
  !inherits(result, "try-error") && isTRUE(result$code == 0L)
}


#' Box-Cox transform the primary predictor of a formula, in place
#'
#' Replaces the primary predictor column (the first term on the right-hand
#' side of \code{formula}) in \code{mydata} with its Box-Cox transform.
#'
#' @param formula model formula; only the first term label is used.
#' @param mydata data frame containing the column to transform.
#' @param ixx numeric vector to transform (usually \code{mydata[[<primary predictor>]]}).
#' @param lambda Box-Cox shape parameter.
#' @return \code{mydata} with the primary predictor column replaced by its transform.
#' @importFrom stats complete.cases model.frame model.matrix na.pass terms
#' @noRd
box_cox_new <- function(formula, mydata, ixx, lambda) {
  col_name <- attr(terms(formula), "term.labels")[1]
  iv <- if (lambda != 0) expm1(lambda * log(ixx)) / lambda else log(ixx)
  mydata[[col_name]] <- iv
  mydata
}


#' One-hot encode a factor covariate
#'
#' Appends dummy columns for all but the reference level of \code{variable}
#' to \code{df}, named \code{"<var_name>_<level index>"}.
#'
#' @param df data frame to append columns to.
#' @param variable factor vector to encode.
#' @param var_name base name for the new dummy columns.
#' @return \code{df} with the dummy columns appended.
#' @noRd
add1hot_encoding <- function(df, variable, var_name) {
  if (nlevels(variable) <= 1L) return(df)

  for (i in seq_len(nlevels(variable) - 1L)) {
    new_name <- paste(var_name, i, sep = "_")
    df[new_name] <- as.numeric(variable == levels(variable)[i + 1])
  }
  df
}


#' Resolve the public weight specification to a numeric vector
#' @noRd
.resolve_weights <- function(data, weight_column_name) {
  n <- nrow(data)

  if (is.null(weight_column_name) ||
      (is.numeric(weight_column_name) && length(weight_column_name) == 1L &&
       isTRUE(weight_column_name == 1))) {
    weights <- rep(1, n)
  } else if (is.character(weight_column_name) && length(weight_column_name) == 1L) {
    if (!weight_column_name %in% names(data)) {
      stop("weight column not found in data: ", weight_column_name, call. = FALSE)
    }
    weights <- data[[weight_column_name]]
  } else if (is.numeric(weight_column_name) && length(weight_column_name) == n) {
    weights <- as.numeric(weight_column_name)
  } else {
    stop("weight_column_name must be NULL, 1, a column name, or a numeric vector with one value per row", call. = FALSE)
  }

  if (!is.numeric(weights)) {
    stop("survey weights must be numeric", call. = FALSE)
  }
  weights
}


#' Extract model matrices from a formula
#'
#' Evaluates \code{formula} against \code{data} and splits the result into
#' the outcome, primary predictor, one-hot-encoded covariates, and survey
#' weights, dropping rows with missing covariates.
#'
#' @param formula a formula of the form \code{y ~ x + z1 + z2}.
#' @param data data frame containing the variables in \code{formula}.
#' @param weight_column_name name of the column in \code{data} containing
#'   survey weights, a numeric vector of weights, or \code{NULL}/\code{1} for
#'   unweighted analysis.
#' @return a list with components \code{ixx}, \code{iyy}, \code{iZZ}, \code{iw}.
#' @importFrom stats terms
#' @noRd
get_processed_data <- function(formula, data, weight_column_name) {
  variables    <- eval(attr(terms(formula), "variables"),   envir = data)
  var_name_list <- eval(attr(terms(formula), "term.labels"), envir = data)

  iyy <- variables[[1]]
  ixx <- variables[[2]]
  iZZ <- data.frame(matrix(NA, nrow = length(iyy), ncol = 0))

  if (length(variables) >= 3) {
    for (idx in 3:length(variables)) {
      var_name <- var_name_list[idx - 1]
      if (is.factor(variables[[idx]])) {
        iZZ <- add1hot_encoding(iZZ, variables[[idx]], var_name)
      } else {
        iZZ[[var_name]] <- variables[[idx]]
      }
    }
  } else {
    iZZ <- data.frame(matrix(1, nrow = length(iyy), ncol = 1))
  }

  iw <- .resolve_weights(data, weight_column_name)
  mask <- complete.cases(iyy, ixx, iZZ, iw)

  iyy <- iyy[mask]
  ixx <- ixx[mask]
  iZZ <- iZZ[mask, , drop = FALSE]
  iw  <- iw[mask]

  if (!length(iyy)) stop("no complete observations are available", call. = FALSE)
  if (!is.numeric(ixx) || any(!is.finite(ixx)) || any(ixx <= 0)) {
    stop("the primary predictor must contain only finite, strictly positive values", call. = FALSE)
  }
  if (any(!iyy %in% c(0, 1))) {
    stop("the response must contain only 0 and 1", call. = FALSE)
  }
  if (any(!is.finite(iw)) || any(iw < 0) || sum(iw) <= 0) {
    stop("survey weights must be finite, non-negative, and have a positive sum", call. = FALSE)
  }

  model_frame <- model.frame(formula, data = data, na.action = na.pass)
  model_frame <- model_frame[mask, , drop = FALSE]
  model_matrix <- model.matrix(formula, data = model_frame)
  factor_columns <- vapply(model_frame, is.factor, logical(1))
  xlevels <- lapply(model_frame[factor_columns], levels)

  list(ixx = ixx, iyy = iyy, iZZ = iZZ, iw = iw,
       xlevels = xlevels, contrasts = as.list(attr(model_matrix, "contrasts")),
       row_index = which(mask))
}


#' Build a maxLik-compatible starting vector from a fitted svyglm model
#'
#' @param model an \code{svyglm} model fit with a \code{lambda} element attached.
#' @return a numeric vector \code{c(beta0, beta1, lambda, other covariate coefficients...)}.
#' @importFrom stats coef
#' @noRd
get_inits_from_model <- function(model) {
  inits      <- rep(NA, length(coef(model)) + 1)
  inits[1:2] <- coef(model)[1:2]
  inits[3]   <- model$lambda
  if (length(coef(model)) >= 3)
    inits[4:(length(coef(model)) + 1)] <- coef(model)[-(1:2)]
  inits
}
