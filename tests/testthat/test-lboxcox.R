test_that("get_processed_data gives correct result", {
  df <- data.frame(a = rep(0:1, 5), b = 11:20, c = 21:30,
                    d = c(rep(0, 3), rep(1, 3), rep(2, 4)), w = rep(1, 10))
  preprocess <- lboxcox:::get_processed_data(a ~ b + c + factor(d), df, "w")
  expect_equal(preprocess$ixx, 11:20)
  expect_equal(preprocess$iyy, rep(0:1, 5))
  expect_equal(preprocess$iZZ$c, 21:30)
  expect_equal(preprocess$iZZ[["factor(d)_1"]], c(rep(0, 3), rep(1, 3), rep(0, 4)))
  expect_equal(preprocess$iZZ[["factor(d)_2"]], c(rep(0, 6), rep(1, 4)))
  expect_equal(preprocess$iw, rep(1, 10))

  unweighted <- lboxcox:::get_processed_data(a ~ b + c, df, NULL)
  expect_equal(unweighted$iw, rep(1, 10))
  expect_equal(lboxcox:::get_processed_data(a ~ b + c, df, 1)$iw, rep(1, 10))
})

test_that("weight specifications and incomplete rows are handled consistently", {
  df <- data.frame(
    y = c(0, 1, 0, 1), x = c(1, 2, NA, 4),
    z = c(1, NA, 3, 4), w = c(1, 2, 3, 4)
  )
  processed <- lboxcox:::get_processed_data(y ~ x + z, df, "w")
  expect_equal(processed$row_index, c(1L, 4L))
  expect_equal(processed$iw, c(1, 4))

  numeric_weights <- c(1, 3, 5, 7)
  design <- lboxcox:::.make_design(df, numeric_weights, y ~ x + z)
  expect_equal(as.numeric(weights(design)), c(1, 7))
  expect_error(
    lboxcox:::get_processed_data(y ~ x, transform(df, x = c(0, 2, 3, 4)), NULL),
    "strictly positive"
  )
})

test_that("svyglm_train init is the same with and without parallel", {
  survey1 <- lboxcox:::svyglm_train(
    depression ~ mercury + age,
    data = depress,
    weight_column_name = "weight",
    lambda_vector = seq(0, 2, length = 5),
    num_cores = 1
  )
  init1 <- lboxcox:::get_inits_from_model(survey1)
  survey2 <- lboxcox:::svyglm_train(
    depression ~ mercury + age,
    data = depress,
    weight_column_name = "weight",
    lambda_vector = seq(0, 2, length = 5),
    num_cores = 2
  )
  init2 <- lboxcox:::get_inits_from_model(survey2)
  expect_equal(init1, init2)
  expect_equal(foreach::getDoParName(), "doSEQ")
})

test_that("devr penalizes wrong-direction predictions more than correct ones", {
  y <- c(0, 1, 0, 1)
  good_pred <- c(0.1, 0.9, 0.1, 0.9)
  bad_pred  <- c(0.9, 0.1, 0.9, 0.1)
  expect_lt(devr(y, good_pred), devr(y, bad_pred))
})

test_that("weighted log-likelihood and analytic gradient use the same normalization", {
  x <- c(0.25, 0.5, 0.8, 1.2, 1.8, 2.4)
  y <- c(0, 0, 1, 0, 1, 1)
  w <- c(0.5, 1, 2, 1.5, 3, 2.5)
  z <- matrix(1, nrow = length(y), ncol = 1)
  par <- c(-0.4, 0.7, 0.8)

  objective <- function(theta, weights = w) {
    lboxcox:::LogLikeFun_new(theta, x, y, weights, z)
  }
  h <- 1e-6
  numerical_gradient <- vapply(seq_along(par), function(j) {
    step <- rep(0, length(par))
    step[j] <- h
    (objective(par + step) - objective(par - step)) / (2 * h)
  }, numeric(1))
  analytic_gradient <- lboxcox:::ScoreFun_new(par, x, y, w, z)

  expect_equal(analytic_gradient, numerical_gradient, tolerance = 1e-5)
  expect_equal(objective(par, 10 * w), objective(par, w), tolerance = 1e-12)
  expect_equal(
    lboxcox:::ScoreFun_new(par, x, y, 10 * w, z),
    analytic_gradient,
    tolerance = 1e-12
  )
})

test_that("log-likelihood remains finite for extreme positive linear predictors", {
  # At lambda = 2 the heavy-tailed exposure produces eta > 500.  The former
  # log(1 - plogis(eta)) implementation rounded p to one and returned -1e9.
  x <- c(0.04, 0.2, 1, 32)
  y <- c(0, 0, 1, 1)
  w <- rep(1, length(y))
  z <- matrix(1, nrow = length(y), ncol = 1)
  par <- c(-3.5, 1.2, 2)

  value <- lboxcox:::LogLikeFun_new(par, x, y, w, z)
  expected_eta <- par[1] + par[2] * (x^2 - 1) / 2
  expected <- mean(ifelse(
    y == 1,
    -log1p(exp(-expected_eta)),
    -(pmax(expected_eta, 0) + log1p(exp(-abs(expected_eta))))
  ))

  expect_true(is.finite(value))
  expect_gt(value, -1e9)
  expect_equal(value, expected, tolerance = 1e-12)
})

test_that("likelihood safety guard matches the constrained lambda range", {
  x <- c(0.5, 1, 2)
  y <- c(0, 1, 1)
  w <- rep(1, length(y))
  z <- matrix(1, nrow = length(y), ncol = 1)

  at_zero <- c(-0.5, 0.8, 0)
  at_two <- c(-0.5, 0.8, 2)
  below <- c(-0.5, 0.8, -1e-6)
  above <- c(-0.5, 0.8, 2 + 1e-6)

  expect_true(is.finite(lboxcox:::LogLikeFun_new(at_zero, x, y, w, z)))
  expect_true(is.finite(lboxcox:::LogLikeFun_new(at_two, x, y, w, z)))
  expect_equal(lboxcox:::LogLikeFun_new(below, x, y, w, z), -1e9)
  expect_equal(lboxcox:::LogLikeFun_new(above, x, y, w, z), -1e9)
  expect_equal(lboxcox:::ScoreFun_new(below, x, y, w, z), rep(0, 3))
  expect_equal(lboxcox:::ScoreFun_new(above, x, y, w, z), rep(0, 3))
})

test_that("lbc_maxlik fits and predicts on the depress dataset", {
  fit <- lbc_maxlik(
    depression ~ mercury + age + factor(gender),
    weight_column_name = "weight",
    data = depress,
    svy_lambda_vector = seq(0, 2, length = 4),
    num_cores = 1
  )
  expect_true(all(c("Beta_0", "Beta_1", "Lambda") %in% names(fit$estimate)))
  expect_true(is.finite(fit$estimate["Lambda"]))

  pred <- lboxcox_maxLik.predict(fit, depress, depression ~ mercury + age + factor(gender))
  expect_length(pred, nrow(depress))
  expect_true(all(pred >= 0 & pred <= 1))
})

test_that("prediction remains finite for extreme linear predictors", {
  extreme_fit <- list(
    estimate = c(Beta_0 = 1000, Beta_1 = 1000, Lambda = 1)
  )
  newdata <- data.frame(y = c(0, 1), x = c(1, 2))

  pred <- lboxcox_maxLik.predict(extreme_fit, newdata, y ~ x)
  expect_true(all(is.finite(pred)))
  expect_true(all(pred >= 0 & pred <= 1))

  ensemble_fits <- rep(list(NA), 100)
  ensemble_fits[[1]] <- extreme_fit
  ensemble <- list(extreme_fit, ensemble_fits, 0)
  ensemble_pred <- lboxcox_maxLik_el.predict(ensemble, newdata, y ~ x)
  expect_true(all(is.finite(ensemble_pred)))
  expect_true(all(ensemble_pred >= 0 & ensemble_pred <= 1))
})

test_that("prediction does not require a response and preserves training factor levels", {
  fit <- list(
    estimate = c(Beta_0 = -1, Beta_1 = 0.2, Lambda = 1, "factor(g)_1" = 0.5),
    lbc_xlevels = list("factor(g)" = c("0", "1")),
    lbc_contrasts = list("factor(g)" = "contr.treatment")
  )
  newdata <- data.frame(x = c(1, 2), g = c(0, 0))

  pred <- lboxcox_maxLik.predict(fit, newdata, y ~ x + factor(g))
  expect_length(pred, 2)
  expect_true(all(is.finite(pred)))
})

test_that("constrained BFGS accepts only optim success code zero", {
  objective <- function(theta) {
    -(theta[1] - 1)^2 - (theta[2] - 2)^2
  }
  gradient <- function(theta) {
    c(-2 * (theta[1] - 1), -2 * (theta[2] - 2))
  }
  constraints <- list(
    ineqA = rbind(c(1, 0), c(0, 1)),
    ineqB = c(10, 10)
  )

  iteration_limited <- maxLik::maxLik(
    objective, grad = gradient, start = c(0, 0), method = "BFGS",
    constraints = constraints, control = list(iterlim = 1)
  )
  converged <- maxLik::maxLik(
    objective, grad = gradient, start = c(0, 0), method = "BFGS",
    constraints = constraints, control = list(iterlim = 200)
  )

  expect_equal(iteration_limited$code, 1)
  expect_match(iteration_limited$message, "iteration limit")
  expect_false(lboxcox:::.is_bfgs_success(iteration_limited))
  expect_equal(converged$code, 0)
  expect_true(lboxcox:::.is_bfgs_success(converged))
})

test_that("deterministic fitting does not reset the caller RNG state", {
  set.seed(1234)
  rng_before <- .Random.seed
  dat <- data.frame(
    y = rep(c(0, 1), 20),
    x = exp(seq(-1, 1, length.out = 40))
  )

  suppressWarnings(lbc_maxlik(
    y ~ x, NULL, dat,
    svy_lambda_vector = c(0, 1),
    init_lambda_vector = c(0, 1),
    num_cores = 1,
    seed = 999
  ))
  expect_identical(.Random.seed, rng_before)

  suppressWarnings(lbc_train_ms(
    y ~ x, NULL, dat,
    svy_lambda_vector = c(0, 1),
    num_cores = 1
  ))
  expect_identical(.Random.seed, rng_before)
})

test_that("lbc_train_ms returns a valid constrained lambda", {
  set.seed(42)
  x <- rlnorm(160)
  y <- rbinom(160, 1, plogis(-1 + 0.8 * log(x)))
  dat <- data.frame(y = y, x = x)

  fit <- lbc_train_ms(
    y ~ x,
    weight_column_name = NULL,
    data = dat,
    svy_lambda_vector = seq(0, 2, length = 5),
    num_cores = 1
  )
  expect_true(is.finite(fit$estimate["Lambda"]))
  expect_gte(as.numeric(fit$estimate["Lambda"]), 0)
  expect_lte(as.numeric(fit$estimate["Lambda"]), 2)
  expect_true(fit$error %in% c(0, 1))
})

test_that("lboxcox_cv.fit selects a lambda and predicts in [0, 1]", {
  set.seed(1)
  sub <- depress[sample(nrow(depress), 300), ]
  cv_fit <- lboxcox_cv.fit(
    sub, sub$mercury, sub$depression,
    depression ~ mercury + age,
    weight_column_name = "weight",
    lambda_vector = seq(0, 2, length = 4),
    k = 3
  )
  expect_true(is.finite(as.numeric(cv_fit$estimate["Lambda"])))
  expect_equal(cv_fit$lbc_cv_grid, seq(0, 2, length = 4))
  expect_length(cv_fit$lbc_cv_sadr, 4)
  expect_true(all(is.finite(cv_fit$lbc_cv_sadr)))

  pred <- lboxcox_cv.predict(cv_fit, sub, depression ~ mercury + age)
  expect_true(all(pred >= 0 & pred <= 1))
})

test_that("median_effect returns a finite point estimate and CI", {
  fit <- lbc_maxlik(
    depression ~ mercury + age,
    weight_column_name = "weight",
    data = depress,
    svy_lambda_vector = seq(0, 2, length = 4),
    num_cores = 1
  )
  me <- median_effect(depression ~ mercury + age, "weight", depress, fit)
  expect_named(me, c("median effect", "lower 95% ci", "upper 95% ci"))
  expect_true(all(is.finite(me)))
})

test_that("median_effect supports profile-grid svyglm fits", {
  set.seed(7)
  dat <- data.frame(x = exp(rnorm(100)))
  dat$y <- rbinom(100, 1, plogis(-0.5 + 0.7 * log(dat$x)))
  fit <- lboxcox:::.build_final_model(y ~ x, dat, NULL, lambda = 0.5)

  me <- median_effect(y ~ x, NULL, dat, fit)
  expect_named(me, c("median effect", "lower 95% ci", "upper 95% ci"))
  expect_true(all(is.finite(me)))
})
