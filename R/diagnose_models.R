#' Model diagnostics after batch adjustment
#'
#' @description
#' After \code{\link[batchtma]{adjust_batch}} has performed
#' adjustment for batch effects, \code{diagnose_models}
#' provides an overview of parameters and adjustment models.
#' Information is only available about the most recent
#' run of \code{\link[batchtma]{adjust_batch}} on a dataset.
#'
#' @param data Batch-adjusted dataset (in which
#' \code{\link[batchtma]{adjust_batch}} has stored information on batch
#' adjustment in the attribute \code{.batchtma})
#'
#' @return List:
#' * \code{adjust_method} Method used for batch adjustment
#'   (see \code{\link[batchtma]{adjust_batch}}).
#' * \code{markers} Variables of biomarkers for adjustment
#' * \code{suffix} Suffix appended to variable names
#' * \code{batchvar} Variable indicating batch
#' * \code{confounders} Confounders, i.e. determinants of
#'   biomarker levels that differ between batches.
#'   Returned only if used by the model.
#' * \code{adjust_parameters} Tibble of parameters used to
#'   obtain adjust biomarker levels. Parameters differ between
#'   methods:
#'
#'     + \code{simple}, \code{standardize}, and \code{ipw}: Estimated adjustment
#'       parameters are a tibble with one \code{batchmean} per \code{marker}
#'       and \code{.batchvar}.
#'     + \code{quantreg} returns a tibble with numerous values per
#'       \code{marker} and \code{.batchvar}: unadjusted (\code{un_...}) and
#'       adjusted (\code{ad_...}) estimates of the lower (\code{..._lo}) and
#'       upper quantile (\code{..._hi}) and interquantile range (\code{..._iq}),
#'       plus the lower (\code{all_lo}) and upper quantiles (\code{all_hi})
#'       across all batches.
#'     + \code{quantnorm} does not explicitly estimate parameters.
#'
#' * \code{model_fits} List of model fit objects, one
#'   per biomarker. Models differ between methods:
#'
#'     + \code{standardize}: Linear regression model for the biomarker with
#'       \code{.batchvar} and \code{confounders} as predictors, from which
#'       marginal predictions of batch means for each batch are obtained.
#'     + \code{ipw}: Logistic (2 batches) or multinomial models for assignment
#'       to a specific batch with \code{.batchvar} as the response and
#'       \code{confounders} as the predictors, used to generate stabilized
#'       inverse-probability weights that are then used in a linear regression
#'       model to estimate marginally standardized batch means.
#'     + \code{quantreg}: Quantile regression with the marker as the response
#'       variable and \code{.batchvar} and \code{confounders} as predictors.
#'     + \code{simple} and \code{quantnorm} do not fit any regression models.
#'
#' @export
#' @examples
#' # Data frame with two batches
#' # Batch 2 has higher values of biomarker and confounder
#' df <- data.frame(
#'   tma = rep(1:2, times = 10),
#'   biomarker = rep(1:2, times = 10) +
#'     runif(max = 5, n = 20),
#'   confounder = rep(0:1, times = 10) +
#'     runif(max = 10, n = 20)
#' )
#'
#' # Adjust for batch effects
#' df2 <- adjust_batch(
#'   data = df,
#'   markers = biomarker,
#'   batch = tma,
#'   method = quantreg,
#'   confounders = confounder
#' )
#'
#' # Show overview of model diagnostics:
#' diagnose_models(data = df2)
#'
#' # Obtain first fitted regression model:
#' fit <- diagnose_models(data = df2)$model_fits[[1]][[1]]
#'
#' # Obtain residuals for this model:
#' residuals(fit)
diagnose_models <- function(data) {
  if (is.null(attr(data, which = ".batchtma"))) {
    message("No information from adjust_batch() available in this dataset.")
  } else {
    return(attr(data, which = ".batchtma"))
  }
}

#' Print method for diagnose_models
#'
#' Print method for \code{\link[batchtma]{diagnose_models}}
#'
#' @param x List output from
#'   \code{\link[batchtma]{diagnose_models}}
#' @param ... not passed on
#'
#' @seealso \code{\link[batchtma]{diagnose_models}}
#'
#' @return None; printout only.
#' @export
#' @noRd
print.batchtma <- function(x, ...) {
  cat(paste0(
    "Dataset after batch effect adjustment using 'method = ",
    x$adjust_method,
    "'\n"
  ))
  adj_markers <- paste0(x$markers, x$suffix)
  cat(paste0("Variable defining batches: ", x$batchvar, "\n"))
  cat(paste0(
    "Adjusted variables: ",
    paste(adj_markers, sep = " ", collapse = " "),
    "\n"
  ))
  cat(paste0(
    "Confounders: ",
    paste(x$confounders, sep = " ", collapse = " "),
    "\n"
  ))
  cat("\nEstimated adjustment parameters:\n")
  if (x$adjust_method == "quantnorm") {
    cat("'method = quantnorm' does not explicitly estimate parameters.\n")
  } else {
    print(x$adjust_parameters)
  }
  cat("\nModels for adjustment:\n")
  print(x$model_fits)
}
