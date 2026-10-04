# lboxcox: Logistic Box-Cox Regression with Multi-Start Fitting and Bootstrap Aggregation

## Introduction

`lboxcox` fits logistic Box-Cox (LBC) regression models for a binary outcome and a strictly positive continuous predictor. It is intended for settings in which the predictor-outcome relationship may be nonlinear but a compact, interpretable parametric model is preferred to a fully nonparametric fit.

Ordinary logistic regression assumes that a continuous predictor has a linear effect on the log-odds scale. The LBC model relaxes this assumption by applying a Box-Cox transformation to the primary predictor and estimating its shape parameter from the data. The resulting model retains the familiar logistic-regression structure while allowing log-like, concave, linear, or convex exposure-response relationships. The original model and its median-effect interpretation are described by Xing et al. (2021) [[2]](#2).

Numerical maximization of the LBC likelihood can be sensitive to starting values because the shape parameter enters the model nonlinearly. This package therefore provides multi-start fitting and bootstrap aggregation in addition to single-fit maximum likelihood. These extensions are developed by Xu, Wang, and Xing in *Ensemble Logistic Box-Cox Model for Improved Prediction and Estimation* [[3]](#3). Multi-start fitting searches from multiple initial shape values, whereas bootstrap aggregation combines predictions from models fitted to resampled datasets.

Version 2.0.1 builds on the original 2022 CRAN release series (`lboxcox` 1.0--1.1). It revises the original likelihood, score, and data-preprocessing routines for greater numerical stability and efficiency. It also adds multi-start fitting and bootstrap-aggregated estimation and prediction while retaining the package's cross-validation and prediction utilities.

## Installation

Install the package with:

```r
install.packages("lboxcox")
```

## Model

For a strictly positive predictor (x), the Box-Cox transformation is

\[
x^{(\lambda)} =
\begin{cases}
(x^\lambda - 1)/\lambda, & \lambda \ne 0, \\
\log(x), & \lambda = 0.
\end{cases}
\]

For a binary outcome (Y_i), a positive primary predictor (X_i), and a vector of adjustment covariates \(\mathbf Z_i\), the logistic Box-Cox model is

\[
\operatorname{logit}\{\Pr(Y_i=1)\}
= \beta_0 + \beta_1 X_i^{(\lambda)}
+ \boldsymbol{\gamma}^{\mathsf T}\mathbf Z_i.
\]

In the package interface, the first term on the right-hand side of the model formula is treated as the positive predictor that receives the Box-Cox transformation. All remaining terms are adjustment covariates.

### Interpreting the parameters

The shape parameter \(\lambda\) describes the form of the predictor-outcome relationship. Values near 0 correspond to a logarithmic transformation, \(\lambda=1\) gives a linear term, and larger values allow increasingly convex relationships. The coefficient \(\beta_1\) describes the direction and strength of association on the transformed scale, so its magnitude should be interpreted together with the fitted shape parameter.

### Median effect

For an approximately log-normal predictor with log-scale location \(\mu\), the median effect on the original predictor scale is

\[
\Delta^* = \beta_1 \exp\{(\lambda-1)\mu\}.
\]

`median_effect()` evaluates this summary at the survey-weighted mean of the log predictor and returns a Wald-type 95% confidence interval. Direct maximum-likelihood fits use the joint likelihood Hessian. Profile-grid, cross-validation, and ensemble refits return an interval conditional on the selected lambda.

## Fitting methods

The main fitting functions correspond to the methods described in the accompanying papers as follows:

| Method | Function | Description |
|:--|:--|:--|
| LBC-ML | `lbc_maxlik()` | Fits one LBC model by maximum likelihood |
| LBC-MS | `lbc_train_ms()` | Fits from multiple starting values and selects the lambda associated with the highest achieved log-likelihood |
| LBC-EL | `lbc_train_bagging()` | Applies LBC-ML to bootstrap samples and aggregates their predictions |
| LBC-CM | `lbc_train_all()` | Applies multi-start fitting within each bootstrap sample |

### LBC-ML

`lbc_maxlik()` maximizes the LBC likelihood using `maxLik::maxLik`. By default, survey-weighted logistic fits over a small lambda grid are used to construct a starting vector. If `init` is supplied, it is used as the starting vector for direct maximization. A fallback lambda grid is still evaluated in case the direct estimate falls outside the admissible range.

### LBC-MS

`lbc_train_ms()` evaluates the likelihood from a grid of starting lambda values. It selects the lambda from the run with the highest achieved log-likelihood and returns a full-data survey-weighted logistic refit conditional on that lambda.

### LBC-EL and LBC-CM

`lbc_train_bagging()` fits LBC-ML models to 100 bootstrap samples. `lbc_train_all()` instead performs multi-start fitting within each bootstrap sample and is therefore substantially more computationally intensive.

Each function returns a list containing:

1. a full-data refit at the median of the bootstrap-specific lambda estimates;
2. the individual bootstrap fits; and
3. the number of bootstrap calls that did not return a usable fit.

The median bootstrap-specific lambda is a descriptive summary rather than the parameter estimate of a single ensemble model. Bootstrap-aggregated predictions are obtained by averaging predicted probabilities across the available bootstrap fits.

## Data requirements

The model formula should have the form `y ~ x + z1 + z2`, where:

- `y` is a binary outcome;
- `x`, the first term on the right-hand side, is the primary continuous predictor and must be strictly positive; and
- `z1`, `z2`, and any additional terms are adjustment covariates.

`weight_column_name` may identify a column containing non-negative observation weights or be a numeric vector with one value per row. Use `NULL` or `1` for an unweighted analysis. Rows containing missing responses, predictors, covariates, or weights are excluded consistently from fitting.

The current implementation incorporates observation weights but does not accept survey strata or primary sampling-unit identifiers. Results should therefore be interpreted as sampling-weighted model estimates rather than as complete design-based survey estimates.

## Quick start

```r
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
```

For multi-start fitting:

```r
fit_ms <- lbc_train_ms(
  formula_lbc,
  weight_column_name = "weight",
  data = depress,
  svy_lambda_vector = seq(0, 2, length.out = 10)
)

fit_ms$estimate
```

For bootstrap aggregation:

```r
set.seed(1)
fit_el <- lbc_train_bagging(
  formula_lbc,
  weight_column_name = "weight",
  data = depress,
  cores = 2
)

fit_el[[1]]$estimate
fit_el[[3]]
```

`lbc_train_all()` has the same overall return structure as `lbc_train_bagging()`, but uses multi-start fitting within every bootstrap sample.

## Prediction and model evaluation

### Prediction

Use `lboxcox_maxLik.predict()` with an LBC-ML or LBC-MS fit. Use `lboxcox_maxLik_el.predict()` with an LBC-EL or LBC-CM result to average predicted probabilities across the available bootstrap fits.

```r
p_ml <- lboxcox_maxLik.predict(fit_ml, depress, formula_lbc)
p_ms <- lboxcox_maxLik.predict(fit_ms, depress, formula_lbc)
p_el <- lboxcox_maxLik_el.predict(fit_el, depress, formula_lbc)
```

### Cross-validated lambda selection

As an alternative to likelihood-based lambda estimation, `lboxcox_cv.fit()` selects lambda from a user-supplied grid by minimizing the mean fold-specific sum of absolute deviance residuals (SADR).

```r
fit_cv <- lboxcox_cv.fit(
  mydata = depress,
  ixx = depress$mercury,
  iyy = depress$depression,
  formula = formula_lbc,
  weight_column_name = "weight",
  lambda_vector = seq(0, 2, length.out = 10),
  k = 5
)

p_cv <- lboxcox_cv.predict(fit_cv, depress, formula_lbc)
```

### SADR

`devr()` computes the sum of absolute deviance residuals directly from binary outcomes and predicted probabilities. Lower values indicate better predictive performance when models are evaluated on the same observations.

```r
devr(depress$depression, p_ml)
devr(depress$depression, p_el)
```

### Median effect

```r
median_effect(
  formula_lbc,
  weight_column_name = "weight",
  data = depress,
  trained_model = fit_ml
)
```

## Dataset

The bundled `depress` data frame contains the 8,893 adults aged 20 years or older used in the NHANES application. The analytic sample combines the 2005-2006 and 2007-2008 survey cycles and contains five variables:

- `depression`: indicator equal to 1 for a Patient Health Questionnaire-9 (PHQ-9) score of at least 10;
- `mercury`: total blood mercury concentration in micrograms per litre;
- `age`: age in years;
- `gender`: 1 for male and 0 for female; and
- `weight`: four-year Day 1 dietary sampling weight, formed as `WTDRD1 / 2` for the two combined cycles.

```r
data(depress, package = "lboxcox")
summary(depress)
```

See `?depress` for the variable definitions and data-source details. See `vignette("lboxcox_train", package = "lboxcox")` for a more detailed package walkthrough.

## Main functions

| Function | Purpose |
|:--|:--|
| `lbc_maxlik()` | Fit one LBC model by maximum likelihood |
| `lbc_train_ms()` | Fit an LBC model from multiple starting values |
| `lbc_train_bagging()` | Fit the LBC-EL bootstrap procedure |
| `lbc_train_all()` | Fit the LBC-CM combined bootstrap and multi-start procedure |
| `lboxcox_maxLik.predict()` | Predict from an LBC-ML or LBC-MS fit |
| `lboxcox_maxLik_el.predict()` | Average predictions across bootstrap fits |
| `lboxcox_cv.fit()` | Select lambda by cross-validated SADR |
| `lboxcox_cv.predict()` | Predict from a cross-validated fit |
| `devr()` | Calculate SADR |
| `median_effect()` | Calculate the median-effect summary and confidence interval |

Use `help(package = "lboxcox")` for the complete function reference.

## References

<a id="1">[1]</a>
Box, G. E. P., & Cox, D. R. (1964). An analysis of transformations. *Journal of the Royal Statistical Society: Series B (Methodological)*, 26(2), 211-243.

<a id="2">[2]</a>
Xing, L., Zhang, X., Burstyn, I., & Gustafson, P. (2021). On logistic Box-Cox regression for flexibly estimating the shape and strength of exposure-disease relationships. *Canadian Journal of Statistics*, 49(3), 808-825. <https://doi.org/10.1002/cjs.11587>

<a id="3">[3]</a>
Xu, S., Wang, J., & Xing, L. *Ensemble Logistic Box-Cox Model for Improved Prediction and Estimation*. Manuscript in preparation.

<a id="4">[4]</a>
Lumley, T. (2011). *Complex Surveys: A Guide to Analysis Using R*. John Wiley & Sons.

## Authors

- Li Xing
- Shiyu Xu
- Jing Wang
- Kohlton Booth
- Xuekui Zhang
- Igor Burstyn
- Paul Gustafson
