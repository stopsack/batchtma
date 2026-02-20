test_that("ggplot runs", {
  df <- data.frame(
    tma = rep(1:2, times = 10),
    biomarker = rep(1:2, times = 10) +
      runif(max = 5, n = 20),
    confounder = rep(0:1, times = 10) +
      runif(max = 10, n = 20)
  )
  expect_no_error(
    object = plot_batch(
      data = df,
      marker = biomarker,
      batch = tma,
      color = confounder
    )
  )
})
