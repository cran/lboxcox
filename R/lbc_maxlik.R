#' Fit a logistic Box-Cox model by direct likelihood maximization
#'
#' Trains the given formula using a logistic Box-Cox model whose parameters
#' (intercept, slope, Box-Cox shape, and covariate coefficients) are found by
#' maximizing the log-likelihood directly with \code{maxLik::maxLik}. If the
#' resulting lambda estimate falls outside \eqn{[0, 2]}, falls back to the
#' best of a grid of \code{svyglm} fits at different lambda values (see
#' \code{\link{lbc_train_ms}} for a version that always searches the grid).
#'
#' @param formula a formula of the form \code{y ~ x + z1 + z2} where \code{y}
#'   is a binary response variable, \code{x} is a continuous predictor
#'   variable, and \code{z1, z2, ...} are covariates.
#' @param weight_column_name the name of the column in \code{data} containing
#'   the survey weights, a numeric vector of weights, or \code{NULL}/\code{1}
#'   for unweighted analysis.
#' @param data dataframe containing the dataset to train on.
#' @param init initial estimates for the coefficients. If \code{NULL}, a grid
#'   of \code{svyglm} models is used to build a starting vector.
#' @param svy_lambda_vector values of lambda used in training the \code{svyglm}
#'   model that supplies initial coefficient estimates. Ignored if \code{init}
#'   is not \code{NULL}.
#' @param init_lambda_vector values of lambda used, as a fallback, to find the
#'   best-log-likelihood starting point if the direct maximization does not
#'   converge to a lambda in \eqn{[0, 2]}.
#' @param num_cores the number of cores used when searching \code{svy_lambda_vector}.
#'   Ignored if \code{init} is not \code{NULL}.
#' @param seed deprecated compatibility argument; ignored because the BFGS
#'   optimizer is deterministic. Control bootstrap reproducibility by calling
#'   \code{set.seed()} before \code{lbc_train_bagging()} or \code{lbc_train_all()}.
#' @return object of class \code{maxLik} from the \pkg{maxLik} package (or,
#'   on fallback, of class \code{svyglm}). Contains the coefficient estimates
#'   in \code{$estimate} (named \code{Beta_0}, \code{Beta_1}, \code{Lambda},
#'   then covariates) and a convergence flag in \code{$error}.
#' @note This is reliant on the following work:
#'
#' Henningsen, A., Toomet, O. (2011). maxLik: A package for maximum likelihood
#' estimation in R. Computational Statistics, 26(3), 443-458.
#' @importFrom maxLik maxLik
#' @importFrom stats terms
#' @export
lbc_maxlik <- function(formula, weight_column_name, data, init = NULL,
                       svy_lambda_vector  = seq(0, 2, length = 4),
                       init_lambda_vector = seq(0, 2, length = 100),
                       num_cores = 1, seed = NULL) {

  processed_data <- get_processed_data(formula, data, weight_column_name)

  if (is.null(init)) {
    model <- svyglm_train(formula, data,
                          lambda_vector       = svy_lambda_vector,
                          weight_column_name  = weight_column_name,
                          num_cores           = num_cores)
    init <- get_inits_from_model(model)
  }

  if (is.na(init[3]) || !is.finite(init[3]) || init[3] < 0 || init[3] > 2)
    init[3] <- 1.0

  # Constrain the BFGS search to the open interval (0, 2); constrained
  # optimization in maxLik requires a strictly interior starting value.
  p     <- length(init)
  eps   <- 1e-6
  init[3] <- min(max(init[3], eps), 2 - eps)

  ineqA <- matrix(0, nrow = 2, ncol = p)
  ineqA[1, 3] <-  1   # lambda > 0
  ineqA[2, 3] <- -1   # 2 - lambda > 0
  lambda_constraints <- list(ineqA = ineqA, ineqB = c(0, 2))

  result <- NULL

  result <- try(
    maxLik(
      logLik      = LogLikeFun_new,
      grad        = ScoreFun_new,
      start       = init,
      method      = "BFGS",
      constraints = lambda_constraints,
      ixx = processed_data$ixx,
      iyy = processed_data$iyy,
      iw  = processed_data$iw,
      iZZ = as.matrix(processed_data$iZZ),
      control = list(iterlim = 200, tol = 1e-6, gradtol = 1e-4)
    ),
    silent = TRUE
  )

  fit_ok <- .is_bfgs_success(result) &&
    is.finite(result$maximum) &&
    is.finite(result$estimate[3]) &&
    result$estimate[3] > 0 &&
    result$estimate[3] < 2

  eval_ll <- function(bb) {
    val <- try(LogLikeFun_new(bb,
                              ixx = processed_data$ixx,
                              iyy = processed_data$iyy,
                              iw  = processed_data$iw,
                              iZZ = as.matrix(processed_data$iZZ)),
               silent = TRUE)
    if (inherits(val, "try-error") || !is.finite(val)) -Inf else val
  }

  if (fit_ok) {
    # BFGS converged strictly inside (0, 2); the constrained optimizer can
    # never return the boundary values exactly, so check them separately.
    model0 <- try(.build_final_model(formula, data, weight_column_name, lambda = 0, error = 0), silent = TRUE)
    model2 <- try(.build_final_model(formula, data, weight_column_name, lambda = 2, error = 0), silent = TRUE)

    ll_bfgs <- result$maximum
    ll0     <- if (inherits(model0, "try-error")) -Inf else eval_ll(model0$estimate)
    ll2     <- if (inherits(model2, "try-error")) -Inf else eval_ll(model2$estimate)

    best <- which.max(c(ll_bfgs, ll0, ll2))

    if (best == 1L) {
      glm.all <- result
      variable_names  <- attr(terms(formula), "term.labels")
      covariate_names <- if (length(glm.all$estimate) > 3L) {
        colnames(processed_data$iZZ)[seq_len(length(glm.all$estimate) - 3L)]
      } else {
        character(0)
      }
      names(glm.all$estimate) <- c("Beta_0", "Beta_1", "Lambda", covariate_names)
      glm.all$lbc_xlevels <- processed_data$xlevels
      glm.all$lbc_contrasts <- processed_data$contrasts
      glm.all$error <- 0L
      return(glm.all)
    }
    if (best == 2L) return(model0)
    return(model2)
  }

  # fallback: BFGS did not converge -> 100-point profile grid
  init_list <- svyglm_ms(formula, data,
                         lambda_vector      = init_lambda_vector,
                         weight_column_name = weight_column_name,
                         num_cores          = num_cores)
  n     <- length(init_lambda_vector)
  mymax <- matrix(NA, n, length(init_list[[1]]) + 1)

  for (ii in seq_len(n)) {
    init_ii <- init_list[[ii]]
    mymax[ii, seq_along(init_ii)] <- init_ii
    mymax[ii, ncol(mymax)] <- eval_ll(init_ii)
  }

  lambda_fb <- mymax[which.max(mymax[, ncol(mymax)]), 3]
  if (!is.finite(lambda_fb)) lambda_fb <- 1.0
  .build_final_model(formula, data, weight_column_name, lambda_fb, error = 1)
}
