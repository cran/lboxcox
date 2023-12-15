test_that("init is same w/ and w/out parallel", {
  survey1 = svyglm_train(
    depression ~ mercury + age,
    data=depress,
    weight_column_name=depress$weight,
    lambda_vector=seq(0, 2, length = 25),
    num_cores=1
  )
  init1 = get_inits_from_model(survey1)
  survey2 = svyglm_train(
    depression ~ mercury + age,
    data=depress,
    weight_column_name=depress$weight,
    lambda_vector=seq(0, 2, length = 25),
    num_cores=2
  )
  init2 = get_inits_from_model(survey1)
  expect_equal(init1, init2)
})


test_that("preprocess gives correct result", {
  df = data.frame(a=1:10, b=11:20, c=21:30, d=c(rep(0, 3), rep(1, 3), rep(2, 4)), w=rep(1, 10))
  preprocess = get_processed_data(a~b+c+factor(d), df, df$w)
  expect_equal(preprocess$ixx, 11:20)
  expect_equal(preprocess$iyy, 1:10)
  expect_equal(preprocess$iZZ$c, 21:30)
  expect_equal(preprocess$iZZ[["factor(d)_1"]], c(rep(0, 3), rep(1, 3), rep(0, 4)))
  expect_equal(preprocess$iZZ[["factor(d)_2"]], c(rep(0, 6), rep(1, 4)))
  expect_equal(preprocess$iw, rep(1, 10))
})
