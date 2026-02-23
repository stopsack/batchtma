test_that("input errors are detected", {
  df <- data.frame(
    tma = rep(1:2, times = 10),
    biomarker = rep(1:2, times = 10) +
      runif(max = 5, n = 20),
    confounder = rep(0:1, times = 10) +
      runif(max = 10, n = 20)
  )
  expect_error(
    object = adjust_batch(
      data = df,
      markers = biomarker,
      batch = tma
    ),
    regexp = "No valid argument \'method"
  )
  expect_error(
    object = adjust_batch(
      data = df,
      markers = biomarker,
      batch = tma,
      method = inexistant
    ),
    regexp = "is not implemented"
  )
  expect_message(
    object = adjust_batch(
      data = df,
      markers = biomarker,
      batch = tma,
      method = simple,
      confounders = confounder
    ),
    regexp = "does not support adjustment"
  )
  expect_message(
    object = adjust_batch(
      data = df,
      markers = biomarker,
      batch = tma,
      method = standardize
    ),
    regexp = "no valid confounders"
  )
})
