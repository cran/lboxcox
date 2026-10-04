#' Safely compute the Box-Cox transform and its base power
#'
#' Guards \code{ixx^lamda} against overflow so that \code{\link{LogLikeFun_new}}
#' and \code{\link{ScoreFun_new}} stay finite while \code{maxLik} explores.
#'
#' @param ixx continuous predictor, strictly positive.
#' @param lamda Box-Cox shape parameter.
#' @return a list with components \code{iv} (the transform) and
#'   \code{ixx_lam} (\code{ixx^lamda}, clipped at \code{1e10}).
#' @noRd
.safe_iv <- function(ixx, lamda) {
  if (lamda != 0) {
    log_power <- pmin(lamda * log(ixx), log(1e10))
    ixx_lam <- exp(log_power)
    iv      <- expm1(log_power) / lamda
    list(iv = iv, ixx_lam = ixx_lam)
  } else {
    iv <- log(ixx)
    list(iv = iv, ixx_lam = rep(1, length(ixx)))
  }
}


#' Numerically stable softplus
#'
#' Computes code{log(1 + exp(x))} without overflow or cancellation.
#'
#' @param x numeric vector.
#' @return numeric vector of softplus values.
#' @noRd
.log1pexp <- function(x) {
  pmax(x, 0) + log1p(exp(-abs(x)))
}


#' Log-likelihood of the logistic Box-Cox model
#'
#' Computes the (weighted, normalized) log-likelihood used as the objective
#' passed to \code{maxLik::maxLik} in \code{\link{lbc_maxlik}} and
#' \code{\link{lbc_train_ms}}. Returns \code{-1e9} instead of \code{NA}/\code{Inf}
#' for parameter values that make the transform or linear predictor blow up,
#' so that \code{maxLik} can keep searching instead of erroring out.
#'
#' @param bb parameter vector \code{c(beta0, beta1, lambda, covariate coefficients...)}.
#' @param ixx continuous predictor.
#' @param iyy binary outcome.
#' @param iw sample weight.
#' @param iZZ covariate matrix.
#' @return the log-likelihood value for \code{bb}, or \code{-1e9} if not finite.
#' @noRd
LogLikeFun_new <- function(bb, ixx, iyy, iw, iZZ) {
  lamda <- bb[3]
  if (is.na(lamda) || !is.finite(lamda) || lamda < 0 || lamda > 2)
    return(-1e9)

  res <- .safe_iv(ixx, lamda)
  iv  <- res$iv
  if (any(!is.finite(iv))) return(-1e9)

  myp <- length(bb)
  iS  <- if (myp > 3) {
    mycovbeta <- matrix(bb[4:myp], nrow = myp - 3, ncol = 1)
    bb[1] + bb[2] * iv + iZZ %*% mycovbeta
  } else {
    bb[1] + bb[2] * iv
  }

  if (any(!is.finite(iS))) return(-1e9)

  # Use stable log-probability branches.  The algebraically equivalent
  # y * eta + log(1 - plogis(eta)) fails when plogis(eta) rounds to one.
  log_prob <- ifelse(
    iyy == 1,
    -.log1pexp(-iS),
    -.log1pexp(iS)
  )
  # Use the weighted mean log-likelihood.  Dividing by sum(iw) keeps the
  # objective invariant to an arbitrary rescaling of the weights and matches
  # the normalization used by ScoreFun_new below.
  val  <- sum(iw * log_prob) / sum(iw)
  if (!is.finite(val)) return(-1e9)
  val
}


#' Gradient of the logistic Box-Cox log-likelihood
#'
#' Companion gradient function for \code{\link{LogLikeFun_new}}, passed as
#' the \code{grad} argument to \code{maxLik::maxLik}. Returns a zero vector
#' instead of \code{NA}/\code{Inf} for parameter values that make the
#' transform or linear predictor blow up.
#'
#' @param init parameter vector \code{c(beta0, beta1, lambda, covariate coefficients...)}.
#' @param ixx continuous predictor.
#' @param iyy binary outcome.
#' @param iw sample weight.
#' @param iZZ covariate matrix.
#' @return the gradient of the log-likelihood at \code{init}.
#' @noRd
ScoreFun_new <- function(init, ixx, iyy, iw, iZZ) {
  lamda <- init[3]

  if (is.na(lamda) || !is.finite(lamda) || lamda < 0 || lamda > 2)
    return(rep(0, length(init)))

  res     <- .safe_iv(ixx, lamda)
  iv      <- res$iv
  ixx_lam <- res$ixx_lam

  if (any(!is.finite(iv))) return(rep(0, length(init)))

  de.lamda <- if (lamda != 0) {
    (ixx_lam * log(ixx) - iv) / lamda
  } else {
    log(ixx)^2 / 2
  }

  if (any(!is.finite(de.lamda))) return(rep(0, length(init)))

  myp <- length(init)
  iS  <- if (myp > 3) {
    mycovbeta <- matrix(init[4:myp], nrow = myp - 3, ncol = 1)
    init[1] + init[2] * iv + iZZ %*% mycovbeta
  } else {
    init[1] + init[2] * iv
  }

  if (any(!is.finite(iS))) return(rep(0, length(init)))
  iP <- as.numeric(plogis(iS))

  grad <- if (myp > 3) {
    c(sum(iw * (iyy - iP)),
      sum(iw * ((iyy - iP) * iv)),
      sum(iw * ((iyy - iP) * init[2] * de.lamda)),
      (iw * (iyy - iP)) %*% iZZ) / sum(iw)
  } else {
    c(sum(iw * (iyy - iP)),
      sum(iw * ((iyy - iP) * iv)),
      sum(iw * ((iyy - iP) * init[2] * de.lamda))) / sum(iw)
  }

  grad[!is.finite(grad)] <- 0
  grad
}
