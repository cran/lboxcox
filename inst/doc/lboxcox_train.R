knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE
)

# fit <- lbc_maxlik(
#   y ~ x + z1 + z2,
#   weight_column_name = NULL,
#   data = mydata
# )

library(lboxcox)
data(depress)

formula_lbc <- depression ~ mercury + age + factor(gender)

fit_ml <- lbc_maxlik(
  formula_lbc,
  weight_column_name = "weight",
  data = depress,
  seed = 1
)

fit_ml$estimate

fit_ms <- lbc_train_ms(
  formula_lbc,
  weight_column_name = "weight",
  data = depress,
  svy_lambda_vector = seq(0, 2, length.out = 10)
)

fit_ms$estimate

# set.seed(1)
# fit_el <- lbc_train_bagging(
#   formula_lbc,
#   weight_column_name = "weight",
#   data = depress,
#   cores = 2
# )
# 
# fit_cm <- lbc_train_all(
#   formula_lbc,
#   weight_column_name = "weight",
#   data = depress,
#   cores = 2
# )

p_ml <- lboxcox_maxLik.predict(fit_ml, depress, formula_lbc)
p_ms <- lboxcox_maxLik.predict(fit_ms, depress, formula_lbc)

head(p_ml)

# p_el <- lboxcox_maxLik_el.predict(fit_el, depress, formula_lbc)

devr(depress$depression, p_ml)
devr(depress$depression, p_ms)

# fit_cv <- lboxcox_cv.fit(
#   mydata = depress,
#   ixx = depress$mercury,
#   iyy = depress$depression,
#   formula = formula_lbc,
#   weight_column_name = "weight",
#   lambda_vector = seq(0, 2, length.out = 10),
#   k = 5
# )
# 
# p_cv <- lboxcox_cv.predict(fit_cv, depress, formula_lbc)

median_effect(
  formula_lbc,
  weight_column_name = "weight",
  data = depress,
  trained_model = fit_ml
)

summary(depress)
