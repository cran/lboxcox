#' Bootstrap-ensemble logistic Box-Cox model around \code{lbc_train_ms}
#'
#' Fits \code{\link{lbc_train_ms}} on each of 100 bootstrap resamples of
#' \code{data} in parallel, takes the median of the per-resample lambda
#' estimates, and refits the final model at that lambda on the full data.
#' More expensive than \code{\link{lbc_train_bagging}} (each resample itself
#' searches a lambda grid via \code{\link{lbc_train_ms}}) but more robust to
#' poor starting values within each resample.
#' Bootstrap samples use the caller's current random-number state. Call
#' \code{set.seed()} before this function when reproducible resamples are
#' required.
#'
#' @inheritParams lbc_train_ms
#' @param cores number of cores to parallelize the bootstrap resamples over.
#' @return a list of length 3: the final refit model (as returned by
#'   \code{\link{lbc_train_ms}}), the list of per-resample fits (\code{NA}
#'   for any resample where \code{\link{lbc_train_ms}} errored), and the
#'   count of resamples that errored.
#' @note This is reliant on the following work:
#'
#' Microsoft Corporation, Weston, S. (2020). foreach: Provides Foreach Looping
#' Construct. R package version 1.5.1.
#'
#' Microsoft Corporation, Weston, S. (2020). doParallel: Foreach Parallel
#' Adaptor for the 'parallel' Package. R package version 1.0.16.
#' @importFrom doParallel registerDoParallel stopImplicitCluster
#' @importFrom foreach foreach %dopar% registerDoSEQ
#' @importFrom stats median
#' @export
lbc_train_all <- function(formula, weight_column_name, data, init = NULL,
                          svy_lambda_vector = seq(0, 2, length = 100),
                          num_cores = 1, cores = 5) {
  nbagging <- 100
  bagid <- replicate(
    nbagging,
    sample(seq_len(nrow(data)), size = nrow(data), replace = TRUE),
    simplify = FALSE
  )
  weights <- .resolve_weights(data, weight_column_name)

  registerDoParallel(cores)
  on.exit({
    stopImplicitCluster()
    foreach::registerDoSEQ()
  }, add = TRUE)

  fit_list <- foreach(
    ii       = 1:nbagging,
    .packages = "lboxcox"
  ) %dopar% {
    sub_data <- data[bagid[[ii]], , drop = FALSE]
    temp <- try(lbc_train_ms(
      formula            = formula,
      weight_column_name = weights[bagid[[ii]]],
      data               = sub_data,
      svy_lambda_vector  = svy_lambda_vector,
      num_cores          = 1
    ), silent = TRUE)
    if (inherits(temp, "try-error")) NA else temp
  }

  results <- t(sapply(fit_list, function(x) {
    if (is.list(x)) x$estimate[c("Beta_0", "Beta_1", "Lambda")] else rep(NA, 3)
  }))

  error_all <- sum(is.na(results[, 1]))
  lambda_em <- median(results[, 3], na.rm = TRUE)
  if (!is.finite(lambda_em)) lambda_em <- 1.0

  glm.all <- .build_final_model(formula, data, weights, lambda_em, error = error_all)
  list(glm.all, fit_list, error_all)
}
