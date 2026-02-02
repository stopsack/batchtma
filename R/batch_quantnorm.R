#' Helper function for approach 6: Quantile normalize
#'
#' @param var Single variable to quantile-normalize
#' @param batch Variable indicating batch
#'
#' @return Tibble of means per batch for one variable
#' @noRd
batch_quantnorm <- function(var, batch) {
  tibble::tibble(var, batch) %>%
    tibble::rowid_to_column() %>%
    tidyr::pivot_wider(names_from = batch, values_from = var) %>%
    dplyr::select(-.data$rowid) %>%
    dplyr::select_if(~ !all(is.na(.))) %>%
    as.matrix() %>%
    limma::normalizeQuantiles() %>%
    tibble::as_tibble() %>%
    dplyr::transmute(
      result = purrr::pmap_dbl(
        .l = .,
        .f = function(...) {
          mean(c(...), na.rm = TRUE)
        }
      ),
      result = dplyr::if_else(
        is.nan(.data$result),
        true = NA_real_,
        false = .data$result
      )
    ) %>%
    dplyr::pull(.data$result)
}
