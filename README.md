[![CRAN status](https://www.r-pkg.org/badges/version/lboxcox)](https://cran.r-project.org/package=lboxcox)

# lboxcox: An Implementation of Logistic Box-Cox Regression

## Installation
```
install.packages("lboxcox")
```

## Introduction
The purpose of this repository is to give an implementation of a Logistic Box-Cox regression described in [[2]](#2).
This type of regression allows us to estimate the relationship between a response variable and a predictor variable after we take into account some number of covariates.

## Dataset
The `depress` data frame that comes with this package has 8,893 rows and 5 columns and comes from the National Health and Nutrition Examination Survey (NHANES) 2009–2010. The data frame describes whether someone has depression or not along with mercury levels in the participants blood, age, gender.

---
## Directory Layout

The root directory of the repository is `lboxcox`. The folder structure of `lboxcox` is as follows

<details><summary>lboxcox</summary>
	
	lboxcox/
	├── data
	│   └── depress.rda
	├── DESCRIPTION
	├── lboxcox.Rproj
	├── man  # folder containing auto generated documentation
	├── NAMESPACE
	├── R
	│   ├── depress.R  # contains documentaiton for depress.rda
	│   ├── lboxcox.R  # main file containing training algorithm
	│   ├── LogLikeFun.R  # function for calculating log-likelihood of box-cox model
	│   ├── ScoreFun.R  # calculates jacobian of log-likelihood
	│   ├── SvyglmTrain.R  # calculates svyglm model. Used for calculating initial values
	│   └── Utilities.R  # various unused yet interesting functions
	└── vignettes
	    └── lboxcox_train.Rmd
</details>

## References
<a id="1">[1]</a>
Lumley, T. (2011). Complex surveys: a guide to analysis using R (Vol. 565). John Wiley & Sons.

<a id="2">[2]</a>
Xing, L., Zhang, X., Burstyn, I., & Gustafson, P. (2021). On logistic Box–Cox regression for flexibly estimating the shape and strength of exposure‐disease relationships. Canadian Journal of Statistics, 49(3), 808-825.
