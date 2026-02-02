#' Batch means for approach 2: Unadjusted means
#'
#' @param data Data set
#' @param markers Biomarkers to adjust
#'
#' @importFrom rlang .data
#' @return Tibble of means per marker and batch
#' @noRd
batchmean_simple <- function(data, markers) {
  values <- data |>
    dplyr::select(.data$.id, .data$.batchvar, {{ markers }}) |>
    dplyr::group_by(.data$.batchvar) |>
    dplyr::summarize_at(
      .vars = dplyr::vars(-.data$.id),
      .funs = mean,
      na.rm = TRUE
    ) |>
    dplyr::mutate_at(
      .vars = dplyr::vars(-.data$.batchvar),
      .funs = \(x) x - mean(x, na.rm = TRUE)
    ) |>
    tidyr::pivot_longer(
      col = c(-.data$.batchvar),
      names_to = "marker",
      values_to = "batchmean"
    )
  return(list(list(values = values, models = NULL)))
}
