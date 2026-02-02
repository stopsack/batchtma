#' Drop empty factor levels
#'
#' @description
#' Avoids predict() issues. This is \code{forcats::fct_drop()}
#' without bells and whistles.
#'
#' @param f factor
#'
#' @return Factor
#' @noRd
factor_drop <- function(f) {
  factor_levels <- levels(f)
  factor(f, levels = setdiff(factor_levels, factor_levels[table(f) == 0]))
}
