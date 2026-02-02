test_that("equivalent approaches give same result", {
  df <- data.frame(
    tma = rep(1:2, times = 10),
    biomarker = rep(1:2, times = 10) +
      runif(max = 5, n = 20),
    confounder = rep(0:1, times = 10) +
      runif(max = 10, n = 20),
    unity = 1
  )

  df_adj2 <- adjust_batch(
    data = df,
    markers = biomarker,
    batch = tma,
    method = simple,
    suffix = "adj"
  ) |> # drop all attributes
    data.frame()

  df_adj3 <- adjust_batch(
    data = df,
    markers = biomarker,
    batch = tma,
    method = standardize,
    confounders = unity,
    suffix = "adj"
  ) |>
    data.frame()

  df_adj4 <- adjust_batch(
    data = df,
    markers = biomarker,
    batch = tma,
    method = ipw,
    confounders = unity,
    suffix = "adj"
  ) |>
    data.frame()

  expect_equal(df_adj2, df_adj3)
  expect_equal(df_adj2, df_adj4)
})
