#' Batch means for approach 3: Marginal standardization
#'
#' @param data Data set
#' @param markers Vector of variables to batch-adjust
#' @param confounders Confounders: features that differ
#'   between batches that should be retained
#'
#' @return Tibble with conditional means per marker and batch
#' @noRd
batchmean_standardize <- function(data, markers, confounders) {
  res <- data |>
    tidyr::pivot_longer(
      cols = {{ markers }},
      names_to = "marker",
      values_to = "value"
    ) |>
    dplyr::filter(!is.na(.data$value)) |>
    dplyr::group_by(.data$marker) |>
    tidyr::nest(data = c(-.data$marker)) |>
    dplyr::mutate(
      data = purrr::map(
        .x = .data$data,
        .f = ~ .x |>
          dplyr::mutate(.batchvar = factor_drop(.data$.batchvar))
      ),
      model = purrr::map(
        .x = .data$data,
        .f = ~ stats::lm(
          formula = stats::as.formula(paste0(
            "value ~ .batchvar +",
            paste(
              confounders,
              collapse = " + ",
              sep = " + "
            )
          )),
          data = .x
        )
      ),
      .batchvar = purrr::map(
        .x = .data$data,
        .f = ~ .x |>
          dplyr::pull(.data$.batchvar) |>
          levels()
      )
    )

  values <- res |>
    tidyr::unnest(cols = .data$.batchvar) |>
    dplyr::mutate(
      data = purrr::map2(
        .x = .data$data,
        .y = .data$.batchvar,
        .f = ~ .x |> dplyr::mutate(.batchvar = .y)
      ),
      pred = purrr::map2(.x = .data$model, .y = .data$data, .f = stats::predict)
    ) |>
    dplyr::select(.data$marker, .data$.batchvar, .data$pred) |>
    tidyr::unnest(cols = .data$pred) |>
    dplyr::group_by(.data$marker, .data$.batchvar) |>
    dplyr::summarize(batchmean = mean(.data$pred, na.rm = TRUE)) |>
    dplyr::group_by(.data$marker) |>
    dplyr::mutate(markermean = mean(.data$batchmean)) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      marker = .data$marker,
      .batchvar = .data$.batchvar,
      batchmean = .data$batchmean - .data$markermean
    )
  return(list(list(
    models = res |> dplyr::ungroup() |> dplyr::pull("model"),
    values = values
  )))
}
