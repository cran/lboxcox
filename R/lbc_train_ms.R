#' Fit a logistic Box-Cox model from multiple starting points
#'
#' Trains the given formula using a logistic Box-Cox model by running
#' \code{maxLik::maxLik} from a grid of \code{svyglm}-based starting vectors
#' (one per entry of \code{svy_lambda_vector}) and keeping the fit with the
#' highest achieved log-likelihood. More expensive than
#' \code{\link{lbc_maxlik}} but more robust to a poor single starting point.
#'
#' @inheritParams lbc_maxlik
#' @param svy_lambda_vector values of lambda used to build the grid of
#'   starting vectors that \code{maxLik} is run from.
#' @return the refit \code{svyglm} model at the selected lambda, with
#'   \code{$estimate} (named \code{Beta_0}, \code{Beta_1}, \code{Lambda}, then
#'   covariates) and a convergence flag in \code{$error}; \code{error = 1}
#'   means no continuous BFGS run converged and the exact profile grid was
#'   used as a fallback.
#' @note This is reliant on the following work:
#'
#' Henningsen, A., Toomet, O. (2011). maxLik: A package for maximum likelihood
#' estimation in R. Computational Statistics, 26(3), 443-458.
#' @importFrom maxLik maxLik
#' @export
lbc_train_ms <- function(formula, weight_column_name, data, init = NULL,
                         svy_lambda_vector = seq(0, 2, length = 100),
                         num_cores = 1) {

  processed_data <- get_processed_data(formula, data, weight_column_name)
  init_list      <- svyglm_ms(formula, data,
                              lambda_vector      = svy_lambda_vector,
                              weight_column_name = weight_column_name,
                              num_cores          = num_cores)
  n              <- length(svy_lambda_vector)
  myresult       <- matrix(NA, n, 4)
  mymax          <- matrix(NA, n, 4)

  # Constrain each BFGS run to the open interval (0, 2); constrained
  # optimization in maxLik requires a strictly interior starting value.
  p     <- length(init_list[[1]])
  eps   <- 1e-6
  ineqA <- matrix(0, nrow = 2, ncol = p)
  ineqA[1, 3] <-  1   # lambda > 0
  ineqA[2, 3] <- -1   # 2 - lambda > 0
  lambda_constraints <- list(ineqA = ineqA, ineqB = c(0, 2))

  for (ii in seq_len(n)) {
    init_ii <- init_list[[ii]]
    result  <- NULL

    # exact-boundary grid points (lambda = 0 or 2) still need to be nudged
    # inward just for the constrained BFGS starting value
    start_ii    <- init_ii
    start_ii[3] <- min(max(start_ii[3], eps), 2 - eps)

    result <- try(
      maxLik(
        logLik      = LogLikeFun_new,
        grad        = ScoreFun_new,
        start       = start_ii,
        method      = "BFGS",
        constraints = lambda_constraints,
        ixx  = processed_data$ixx,
        iyy  = processed_data$iyy,
        iw   = processed_data$iw,
        iZZ  = as.matrix(processed_data$iZZ),
        control = list(iterlim = 200, tol = 1e-6, gradtol = 1e-4)
      ),
      silent = TRUE
    )
    fit_ok <- .is_bfgs_success(result) &&
      length(result$estimate) >= 3L &&
      is.finite(result$maximum) &&
      is.finite(result$estimate[3]) &&
      result$estimate[3] > 0 && result$estimate[3] < 2

    if (!fit_ok) {
      myresult[ii, 1:3] <- init_ii[1:3]
      myresult[ii, 4]   <- -Inf
    } else {
      myresult[ii, 1:3] <- result$estimate[1:3]
      myresult[ii, 4]   <- result$maximum
    }

    val <- try(LogLikeFun_new(init_ii,
                              ixx = processed_data$ixx,
                              iyy = processed_data$iyy,
                              iw  = processed_data$iw,
                              iZZ = as.matrix(processed_data$iZZ)),
               silent = TRUE)
    mymax[ii, 1:3] <- init_ii[1:3]
    mymax[ii, 4]   <- if (inherits(val, "try-error") || !is.finite(val)) -Inf else val
  }

  # A valid grid point can legitimately beat every interior optimum, notably
  # when the profile maximum is at the exact boundary lambda = 0 or 2.
  best_bfgs <- if (any(is.finite(myresult[, 4]))) {
    which.max(myresult[, 4])
  } else {
    NA_integer_
  }
  best_grid <- if (any(is.finite(mymax[, 4]))) {
    which.max(mymax[, 4])
  } else {
    NA_integer_
  }

  bfgs_ll <- if (is.na(best_bfgs)) -Inf else myresult[best_bfgs, 4]
  grid_ll <- if (is.na(best_grid)) -Inf else mymax[best_grid, 4]
  lambda <- if (grid_ll > bfgs_ll) {
    mymax[best_grid, 3]
  } else if (is.finite(bfgs_ll)) {
    myresult[best_bfgs, 3]
  } else {
    NA_real_
  }

  # error = 1 means that no continuous BFGS run converged and the returned
  # model therefore comes from the exact profile grid fallback.
  er <- as.integer(!any(is.finite(myresult[, 4])))
  if (!is.finite(lambda)) lambda <- 1.0

  .build_final_model(formula, data, weight_column_name, lambda, error = er)
}
