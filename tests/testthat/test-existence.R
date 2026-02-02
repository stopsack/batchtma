test_that("new variables are generated", {
  df <- data.frame(
    tma = rep(1:2, times = 10),
    biomarker = rep(1:2, times = 10) +
      runif(max = 5, n = 20),
    confounder = rep(0:1, times = 10) +
      runif(max = 10, n = 20)
  )

  df_adj5 <- adjust_batch(
    data = df,
    markers = biomarker,
    batch = tma,
    method = quantreg,
    suffix = "adj"
  )

  expect_equal(
    object = sum(!is.na(df_adj5$biomarkeradj)),
    expected = sum(!is.na(df_adj5$biomarker))
  )

  df_adj6 <- adjust_batch(
    data = df,
    markers = biomarker,
    batch = tma,
    method = quantnorm,
    suffix = "adj"
  )

  expect_equal(
    object = sum(!is.na(df_adj6$biomarkeradj)),
    expected = sum(!is.na(df_adj6$biomarker))
  )
})
