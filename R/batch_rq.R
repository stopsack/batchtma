#' Quantiles for approach 5: Quantile regression
#'
#' @param data Data set
#' @param variable Single variable to batch-adjust
#' @param confounders Confounders: features that differ
#' @param tau Quantiles to use for scaling
#' @param rq_method Algorithmic method to fit quantile regression.
#'
#' @return Tibble of quantiles per batch
#' @noRd
batchrq <- function(data, variable, confounders, tau, rq_method) {
  res <- data |>
    dplyr::rename(variable = {{ variable }}) |>
    dplyr::filter(!is.na(.data$variable)) |>
    dplyr::mutate(.batchvar = factor_drop(.data$.batchvar)) |>
    tidyr::nest(data = dplyr::everything()) |>
    dplyr::mutate(
      un = purrr::map(
        .x = .data$data,
        .f = \(x) {
          quantreg::rq(
            formula = variable ~ .batchvar,
            data = x,
            tau = tau,
            method = rq_method
          )
        }
      ),
      ad = purrr::map(
        .x = .data$data,
        .f = \(x) {
          quantreg::rq(
            formula = stats::reformulate(
              response = "variable",
              termlabels = c(".batchvar", confounders)
            ),
            data = x,
            tau = tau,
            method = rq_method
          )
        }
      ),
      .batchvar = purrr::map(
        .x = .data$data,
        .f = \(x) {
          x |>
            dplyr::pull(.data$.batchvar) |>
            levels()
        }
      )
    )

  values <- res |>
    tidyr::unnest(cols = ".batchvar") |>
    dplyr::mutate(
      data = purrr::map2(
        .x = .data$data,
        .y = .data$.batchvar,
        .f = \(x, y) {
          x |>
            dplyr::mutate(.batchvar = y)
        }
      ),
      un = purrr::map2(
        .x = .data$un,
        .y = .data$data,
        .f = stats::predict
      ),
      ad = purrr::map2(
        .x = .data$ad,
        .y = .data$data,
        .f = stats::predict
      ),
      un = purrr::map(
        .x = .data$un,
        .f = \(x) {
          tibble::as_tibble(
            x,
            .name_repair = ~ c("un_lo", "un_hi")
          )
        }
      ),
      ad = purrr::map(
        .x = .data$ad,
        .f = \(x) {
          tibble::as_tibble(
            x,
            .name_repair = ~ c("ad_lo", "ad_hi")
          )
        }
      ),
      all_lo = purrr::map_dbl(
        .x = .data$data,
        .f = \(x) stats::quantile(x$variable, probs = 0.25)
      ),
      all_hi = purrr::map_dbl(
        .x = .data$data,
        .f = \(x) stats::quantile(x$variable, probs = 0.75)
      ),
      all_iq = .data$all_hi - .data$all_lo
    ) |>
    dplyr::select(
      ".batchvar",
      "un",
      "ad",
      "all_lo",
      "all_hi",
      "all_iq"
    ) |>
    tidyr::unnest(cols = c("un", "ad")) |>
    dplyr::group_by(.data$.batchvar) |>
    dplyr::summarize(
      un_lo = stats::quantile(.data$un_lo, probs = 0.25),
      ad_lo = stats::quantile(.data$ad_lo, probs = 0.25),
      un_hi = stats::quantile(.data$un_hi, probs = 0.75),
      ad_hi = stats::quantile(.data$ad_hi, probs = 0.75),
      all_lo = stats::median(.data$all_lo),
      all_iq = stats::median(.data$all_iq)
    ) |>
    dplyr::mutate(
      un_iq = .data$un_hi - .data$un_lo,
      ad_iq = .data$ad_hi - .data$ad_lo,
      marker = {{ variable }}
    )

  models <- res |> dplyr::pull(.data$ad)
  return(tibble::lst(values, models))
}
