#' Select the Box-Cox lambda by cross-validated deviance residual
#'
#' Trains the given formula using a logistic Box-Cox model whose lambda is
#' chosen, instead of by likelihood maximization, as the value in
#' \code{lambda_vector} with lowest mean k-fold cross-validated
#' \code{\link{devr}} (deviance residual prediction error). The model is then
#' refit on the full data at the selected lambda.
#'
#' @param mydata data frame containing the dataset to train on.
#' @param ixx the (untransformed) primary predictor vector, e.g. \code{mydata$xx}.
#' @param iyy the binary outcome vector, e.g. \code{mydata$yy}.
#' @param formula a formula of the form \code{y ~ x + z1 + z2} where \code{y}
#'   is a binary response variable, \code{x} is a continuous predictor
#'   variable, and \code{z1, z2, ...} are covariates.
#' @param weight_column_name the name of the column in \code{mydata} containing
#'   the survey weights, a numeric vector of weights, or \code{NULL}/\code{1}
#'   for unweighted analysis.
#' @param lambda_vector grid of Box-Cox lambda values to select from.
#' @param k number of cross-validation folds.
#' @return the refit \code{svyglm} model at the selected lambda, with
#'   \code{$estimate} (named \code{Beta_0}, \code{Beta_1}, \code{Lambda}, then
#'   covariates) attached.
#' @importFrom caret createFolds
#' @importFrom survey svyglm
#' @importFrom stats predict
#' @export
lboxcox_cv.fit <- function(mydata, ixx, iyy, formula, weight_column_name = NULL,
                           lambda_vector = seq(0, 2, length = 100), k) {
  if (length(ixx) != nrow(mydata) || length(iyy) != nrow(mydata)) {
    stop("ixx and iyy must have one value per row of mydata", call. = FALSE)
  }
  weights <- .resolve_weights(mydata, weight_column_name)
  model_frame <- model.frame(formula, data = mydata, na.action = na.pass)
  keep <- complete.cases(model_frame, ixx, iyy, weights)
  mydata <- mydata[keep, , drop = FALSE]
  ixx <- ixx[keep]
  iyy <- iyy[keep]
  weight_column_name <- weights[keep]
  get_processed_data(formula, mydata, weight_column_name)

  myfold  <- createFolds(factor(iyy, levels = c(0, 1)), k = k)
  err.all <- numeric(length(lambda_vector))

  for (ilam in seq_along(lambda_vector)) {
    lambda        <- lambda_vector[ilam]
    mydata.trans  <- box_cox_new(formula, mydata, ixx, lambda)
    mydesign      <- .make_design(mydata.trans, weight_column_name, formula)
    err.fold      <- numeric(k)

    for (ifold in seq_len(k)) {
      mytestid <- myfold[[ifold]]
      myglm    <- svyglm(formula, design = mydesign,
                         family = binomial(link = "logit"),
                         subset = -mytestid)
      mypred   <- predict(myglm, newdata = mydata.trans[mytestid, ], type = "response")
      err.fold[ifold] <- devr(iyy[mytestid], mypred)
    }
    err.all[ilam] <- mean(err.fold)
  }

  lambda_mincv <- lambda_vector[which.min(err.all)]
  fit <- .build_final_model(formula, mydata, weight_column_name,
                            lambda_mincv, error = 999)
  fit$lbc_cv_grid <- lambda_vector
  fit$lbc_cv_sadr <- err.all
  fit
}


#' Predict from a \code{lboxcox_cv.fit} model
#'
#' @param myCVfit a model fit by \code{\link{lboxcox_cv.fit}}.
#' @param newdata data frame of new observations to predict on.
#' @param formula the same formula used to fit \code{myCVfit}.
#' @return a numeric vector of predicted probabilities.
#' @importFrom stats predict terms
#' @export
lboxcox_cv.predict <- function(myCVfit, newdata, formula) {
  lambda       <- as.numeric(myCVfit$estimate["Lambda"])
  ixx_col      <- attr(terms(formula), "term.labels")[1]
  newdata.trans <- box_cox_new(formula, newdata, newdata[[ixx_col]], lambda)
  predict(myCVfit, newdata = newdata.trans, type = "response")
}
