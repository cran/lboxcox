#' Fit a single svyglm model at a fixed Box-Cox lambda
#'
#' @param formula model formula.
#' @param design an existing \code{survey::svydesign} object.
#' @param lambda Box-Cox shape parameter applied to the primary predictor.
#' @return the fitted \code{svyglm} object.
#' @importFrom stats terms binomial
#' @importFrom survey svyglm
#' @noRd
train_single_svyglm_model <- function(formula, design, lambda) {
  col_name <- attr(terms(formula), "term.labels")[1]
  myXX     <- design$variables[[col_name]]
  myV0     <- if (lambda == 0) log(myXX) else expm1(lambda * log(myXX)) / lambda
  design$variables[[col_name]] <- myV0
  survey::svyglm(formula, design = design, family = binomial(link = "logit"))
}


#' @rdname train_single_svyglm_model
#' @noRd
train_single_svyglm_model_ms <- function(formula, design, lambda) {
  col_name <- attr(terms(formula), "term.labels")[1]
  myXX     <- design$variables[[col_name]]
  myV0     <- if (lambda == 0) log(myXX) else expm1(lambda * log(myXX)) / lambda
  design$variables[[col_name]] <- myV0
  survey::svyglm(formula, design = design, family = binomial(link = "logit"))
}


#' Select an initial Box-Cox lambda by AIC over a grid of svyglm fits
#'
#' Fits \code{svyglm} at each value in \code{lambda_vector} and returns the
#' model with lowest AIC, with \code{lambda} attached. Used by
#' \code{\link{lbc_maxlik}} to build a single starting vector for
#' \code{maxLik} when \code{init} is not supplied.
#'
#' @param formula model formula.
#' @param data data frame.
#' @param lambda_vector grid of Box-Cox lambda values to search over.
#' @param weight_column_name name of the survey-weight column in \code{data},
#'   a numeric vector of weights, or \code{NULL}/\code{1} for unweighted
#'   analysis.
#' @param num_cores number of cores to parallelize the lambda search over;
#'   \code{1} disables parallelism.
#' @return the best-AIC \code{svyglm} model, with a \code{lambda} element attached.
#' @importFrom stats AIC as.formula
#' @importFrom survey svydesign
#' @importFrom foreach foreach %dopar% registerDoSEQ
#' @importFrom doParallel registerDoParallel stopImplicitCluster
#' @noRd
svyglm_train <- function(formula, data, lambda_vector = seq(0, 2, length = 25),
                         weight_column_name = NULL, num_cores = 1) {

  design <- .make_design(data, weight_column_name, formula)

  train_func <- function(lambda) {
    m <- try(train_single_svyglm_model(formula, design, lambda), silent = TRUE)
    if (inherits(m, "try-error")) return(Inf)
    AIC(m, k = 2)[2]
  }

  myAIC <- if (num_cores == 1) {
    sapply(lambda_vector, train_func)
  } else {
    registerDoParallel(num_cores)
    on.exit({
      stopImplicitCluster()
      foreach::registerDoSEQ()
    }, add = TRUE)
    foreach(lamb = lambda_vector, .combine = c) %dopar% { train_func(lamb) }
  }

  best_lambda <- lambda_vector[which.min(myAIC)]
  best_model  <- train_single_svyglm_model(formula, design, best_lambda)
  best_model$lambda <- best_lambda
  best_model
}


#' Build a grid of svyglm-based starting vectors over Box-Cox lambda
#'
#' Fits \code{svyglm} at every value in \code{lambda_vector} and returns the
#' corresponding \code{\link{get_inits_from_model}} starting vectors. Used by
#' \code{\link{lbc_maxlik}} and \code{\link{lbc_train_ms}} to seed
#' \code{maxLik} from multiple starting points.
#'
#' @inheritParams svyglm_train
#' @return a list of numeric starting vectors, one per entry of \code{lambda_vector}.
#' @importFrom survey svydesign
#' @noRd
svyglm_ms <- function(formula, data, lambda_vector = seq(0, 2, length = 100),
                      weight_column_name = NULL, num_cores = 1) {
  design <- .make_design(data, weight_column_name, formula)
  train_func <- function(lam) {
    model        <- train_single_svyglm_model_ms(formula, design, lam)
    model$lambda <- lam
    get_inits_from_model(model)
  }

  if (num_cores == 1) return(lapply(lambda_vector, train_func))

  registerDoParallel(num_cores)
  on.exit({
    stopImplicitCluster()
    foreach::registerDoSEQ()
  }, add = TRUE)
  foreach(lam = lambda_vector) %dopar% { train_func(lam) }
}


#' Build a survey design object from a weight column or numeric weight vector
#' @noRd
.make_design <- function(data, weight_column_name, formula = NULL) {
  weights <- .resolve_weights(data, weight_column_name)
  keep <- !is.na(weights)
  if (!is.null(formula)) {
    model_frame <- model.frame(formula, data = data, na.action = na.pass)
    keep <- keep & complete.cases(model_frame)
  }
  data <- data[keep, , drop = FALSE]
  weights <- weights[keep]

  if (!nrow(data)) stop("no complete observations are available", call. = FALSE)
  if (any(!is.finite(weights)) || any(weights < 0) || sum(weights) <= 0) {
    stop("survey weights must be finite, non-negative, and have a positive sum", call. = FALSE)
  }
  svydesign(ids = ~1, weights = weights, data = data)
}


#' Refit the final svyglm model at a fixed lambda and attach the estimate summary
#'
#' Shared by \code{\link{lbc_train_ms}}, \code{\link{lbc_train_bagging}}, and
#' \code{\link{lbc_train_all}} to produce the final returned model once a
#' lambda has been selected.
#'
#' @param formula model formula.
#' @param data data frame.
#' @param weight_column_name name of the survey-weight column in \code{data},
#'   a numeric vector of weights, or \code{NULL}/\code{1} for unweighted
#'   analysis.
#' @param lambda the selected Box-Cox shape parameter.
#' @param error integer error/convergence flag to attach as \code{$error}.
#' @return the fitted \code{svyglm} object with \code{$estimate} and \code{$error} attached.
#' @importFrom stats terms binomial
#' @importFrom survey svyglm
#' @noRd
.build_final_model <- function(formula, data, weight_column_name, lambda, error = 0) {
  mydata.trans <- box_cox_new(formula, data, data[[attr(terms(formula), "term.labels")[1]]], lambda)
  mydesign <- .make_design(mydata.trans, weight_column_name, formula)
  glm.all        <- svyglm(formula, design = mydesign, family = binomial(link = "logit"))

  coef_all <- glm.all$coefficients
  beta_0 <- coef_all[1]
  beta_1 <- coef_all[2]
  covariate_names <- names(coef_all)[-(1:2)]
  covariates <- coef_all[-(1:2)]
  estimate <- c(beta_0, beta_1, lambda, covariates)

  names(estimate) <- c("Beta_0", "Beta_1", "Lambda", covariate_names)
  glm.all$estimate <- estimate
  glm.all$error <- error
  glm.all
}
