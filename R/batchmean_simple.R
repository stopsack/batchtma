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
    dplyr::select(".id", ".batchvar", {{ markers }}) |>
    dplyr::group_by(.data$.batchvar) |>
    dplyr::summarize(
      dplyr::across(
        .cols = !".id",
        .fns = \(x) mean(
          x,
          na.rm = TRUE
        )
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        .cols = !".batchvar",
        .fns = \(x) x - mean(x, na.rm = TRUE)
      )
    ) |>
    tidyr::pivot_longer(
      cols = !".batchvar",
      names_to = "marker",
      values_to = "batchmean"
    )
  return(list(list(values = values, models = NULL)))
}
