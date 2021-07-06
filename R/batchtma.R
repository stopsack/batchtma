#' batchtma: Methods to address batch effects
#'
#' The goal of the batchtma is to provide functions for
#' batch effect-adjusting biomarker data. It implements different
#' methods that address batch effects while retaining differences
#' between batches that may be due to “true” underlying differences
#' in factors that drive biomarker values (confounders).
#'
#' @section Functions:
#'
#' \code{\link[batchtma]{adjust_batch}}: Adjust for batch effects
#'
#' \code{\link[batchtma]{diagnose_models}}: Model diagnostics after batch adjustment
#'
#' \code{\link[batchtma]{plot_batch}}: Plot biomarkers by batch
#'
#' @docType package
#' @name batchtma
#' @seealso \url{https://stopsack.github.io/batchtma}
#' @references
#' Stopsack KH, Tyekucheva S, Wang M, Gerke TA, Vaselkiv JB, Penney KL,
#' Kantoff PW, Finn SP, Fiorentino M, Loda M, Lotan TL, Parmigiani G\*,
#' Mucci LA\* (\* equal contribution). Extent, impact, and mitigation of
#' batch effects in tumor biomarker studies using tissue microarrays.
#' bioRxiv 2021.06.29.450369; doi: https://doi.org/10.1101/2021.06.29.450369
NULL
# > NULL
