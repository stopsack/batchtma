#' Adjust for batch effects
#'
#' @description
#' \code{adjust_batch} generates biomarker levels for the variable(s)
#' \code{markers} in the dataset \code{data} that are corrected
#' (adjusted) for batch effects, i.e. differential measurement
#' error between levels of \code{batch}.
#'
#' @param data Data set
#' @param markers Variable name(s) to batch-adjust. Select
#'   multiple variables with tidy evaluation, e.g.,
#'   \code{markers = starts_with("biomarker")}.
#' @param batch Categorical variable indicating batch.
#' @param method Method for batch effect correction:
#'   * \code{simple}  Simple means per batch will be subtracted.
#'     No adjustment for confounders.
#'   * \code{standardize}  Means per batch after standardization
#'     for confounders in linear models will be subtracted.
#'     If no \code{confounders} are supplied, \code{method = simple}
#'     is equivalent and will be used.
#'   * \code{ipw}  Means per batch after inverse-probability
#'     weighting for assignment to a specific batch in multinomial
#'     models, conditional on confounders, will be subtracted.
#'     Stabilized weights are used, truncated at quantiles
#'     defined by the \code{ipw_truncate} parameters. If no
#'     \code{confounders} are supplied, \code{method = simple}
#'     is equivalent and will be used.
#'   * \code{quantreg}  Lower quantiles (default: 25th percentile)
#'     and ranges between a lower and an upper quantile (default: 75th
#'     percentile) will be unified between batches, allowing for
#'     differences in both parameters due to confounders. Set the two
#'     quantiles using the \code{quantreg_tau} parameters.
#'   * \code{quantnorm}  Quantile normalization between batches. No
#'     adjustment for confounders.
#' @param confounders Optional: Confounders, i.e. determinants of
#'   biomarker levels that differ between batches. Only used if
#'   \code{method = standardize}, \code{method = ipw}, or
#'   \code{method = quantreg}, i.e. methods that attempt to retain
#'   some of these "true" between-batch differences. Select multiple
#'   confounders with tidy evaluation, e.g.,
#'   \code{confounders = c(age, age_squared, sex)}.
#' @param suffix Optional: What string to append to variable names
#'   after batch adjustment. Defaults to \code{"_adjX"}, with
#'   \code{X} depending on \code{method}:
#'   * \code{_adj2} from \code{method = simple}
#'   * \code{_adj3} from \code{method = standardize}
#'   * \code{_adj4} from \code{method = ipw}
#'   * \code{_adj5} from \code{method = quantreg}
#'   * \code{_adj6} from \code{method = quantnorm}
#' @param ipw_truncate Optional and used for \code{method = ipw} only:
#'   Lower and upper quantiles for truncation of stabilized
#'   weights. Defaults to \code{c(0.025, 0.975)}.
#' @param quantreg_tau Optional and used for \code{method = quantreg} only:
#'   Quantiles to scale. Defaults to \code{c(0.25, 0.75)}.
#' @param quantreg_method Optional and used for \code{method = quantreg} only:
#'   Algorithmic method to fit quantile regression. Defaults to
#'   \code{"fn"}. See parameter \code{method} of \code{\link[quantreg]{rq}}.
#'
#' @details
#' If no true differences between batches are expected, because
#' samples have been randomized to batches, then a \code{method}
#' that returns adjusted values with equal means
#' (\code{method = simple}) or with equal rank values
#' (\code{method = quantnorm}) for all batches is appropriate.
#'
#' If the distribution of determinants of biomarker values
#' (\code{confounders}) differs between batches, then a
#' \code{method} that retains these "true" differences
#' between batches while adjusting for batch effects
#' may be appropriate: \code{method = standardize} and
#' \code{method = ipw} address means; \code{method = quantreg}
#' addresses lower values and dynamic range separately.
#'
#' Which \code{method} to choose depends on the properties of
#' batch effects (affecting means or also variance?) and
#' the presence and strength of confounding. For the two
#' mean-only confounder-adjusted methods, the choice may depend
#' on  whether the confounder--batch association (\code{method = ipw})
#' or the confounder--biomarker association
#' (\code{method = standardize}) can be modeled better.
#' Generally, if batch effects are present, any adjustment
#' method tends to perform better than no adjustment in
#' reducing bias and increasing between-study reproducibility.
#' See references.
#'
#' All adjustment approaches except \code{method = quantnorm}
#' are based on linear models. It is recommended that variables
#' for \code{markers} and \code{confounders} first be transformed
#' as necessary (e.g., \code{\link[base]{log}} transformations or
#' \code{\link{splines}}). Scaling or mean centering are not necessary,
#' and adjusted values are returned on the original scale.
#' Parameters \code{markers}, \code{batch}, and \code{confounders}
#' support tidy evaluation.
#'
#' Observations with missing values for the \code{markers} and
#' \code{confounders} will be ignored in the estimation of adjustment
#' parameters, as are empty batches. Batch effect-adjusted values
#' for observations with existing marker values but missing
#' confounders are based on adjustment parameters derived from the
#' other observations in a batch with non-missing confounders.
#'
#' @return The \code{data} dataset with batch effect-adjusted
#'   variable(s) added at the end. Model diagnostics, using
#'   the attribute \code{.batchtma} of this dataset, are available
#'   via the \code{\link[batchtma]{diagnose_models}} function.
#' @importFrom rlang :=
#' @export
#'
#' @references
#' Stopsack KH, Tyekucheva S, Wang M, Gerke TA, Vaselkiv JB, Penney KL,
#' Kantoff PW, Finn SP, Fiorentino M, Loda M, Lotan TL, Parmigiani G+,
#' Mucci LA+ (+ equal contribution). Extent, impact, and mitigation of
#' batch effects in tumor biomarker studies using tissue microarrays.
#' eLife 2021;10:e71265. doi: https://doi.org/10.7554/elife.71265
#' (This R package, all methods descriptions, and further recommendations.)
#'
#' Rosner B, Cook N, Portman R, Daniels S, Falkner B.
#' Determination of blood pressure percentiles in
#' normal-weight children: some methodological issues.
#' Am J Epidemiol 2008;167(6):653-66. (Basis for
#' \code{method = standardize})
#'
#' Bolstad BM, Irizarry RA, Åstrand M, Speed TP.
#' A comparison of normalization methods for high density
#' oligonucleotide array data based on variance and bias.
#' Bioinformatics 2003;19:185–193. (\code{method = quantnorm})
#'
#' @author Konrad H. Stopsack
#' @seealso \url{https://stopsack.github.io/batchtma/}
#'
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
#' # Using simple means, ignoring the confounder:
#' adjust_batch(
#'   data = df,
#'   markers = biomarker,
#'   batch = tma,
#'   method = simple
#' )
#' # Returns data set with new variable "biomarker_adj2"
#'
#' # Use quantile regression, include the confounder,
#' # change suffix of returned variable:
#' adjust_batch(
#'   data = df,
#'   markers = biomarker,
#'   batch = tma,
#'   method = quantreg,
#'   confounders = confounder,
#'   suffix = "_batchadjusted"
#' )
#' # Returns data set with new variable "biomarker_batchadjusted"
adjust_batch <- function(
  data,
  markers,
  batch,
  method = c(
    "simple",
    "standardize",
    "ipw",
    "quantreg",
    "quantnorm"
  ),
  confounders = NULL,
  suffix = "_adjX",
  ipw_truncate = c(0.025, 0.975),
  quantreg_tau = c(0.25, 0.75),
  quantreg_method = "fn"
) {
  method <- as.character(dplyr::enexpr(method))
  allmethods <- c("simple", "standardize", "ipw", "quantreg", "quantnorm")
  data_orig <- data |>
    dplyr::mutate(.id = dplyr::row_number())
  data <- data_orig |>
    dplyr::rename(.batchvar = {{ batch }}) |>
    dplyr::mutate(
      .batchvar = factor(.data$.batchvar),
      .batchvar = factor_drop(.data$.batchvar)
    ) |>
    dplyr::select(.data$.id, .data$.batchvar, {{ markers }}, {{ confounders }})
  confounders <- data |>
    dplyr::select({{ confounders }}) |>
    names()

  # Check inputs: method and confounders
  if (!(method[1] %in% allmethods)) {
    stop(paste0(
      "Method '",
      method[1],
      "' is not implemented.\nAvailable methods: ",
      paste(allmethods, collapse = ", "),
      ".\n",
      "See: help(\"adjust_batch\")."
    ))
  }

  if (
    method %in% c("simple", "quantnorm") &
      data |>
        dplyr::select({{ confounders }}) |>
        ncol() >
        0
  ) {
    message(paste0(
      "Batch effect correction via 'method = ",
      method,
      "' was requested.\n This method does not support ",
      "adjustment for confounders (",
      paste(dplyr::enexpr(confounders), sep = ", ", collapse = ", "),
      "). They will be ignored."
    ))
    data <- data |> dplyr::select(-dplyr::any_of({{ confounders }}))
    confounders <- NULL
  }

  # Mean-based methods
  if (method %in% c("simple", "standardize", "ipw")) {
    if (
      method %in%
        c("standardize", "ipw") &
        data |>
          dplyr::select({{ confounders }}) |>
          ncol() ==
          0
    ) {
      message(paste0(
        "Batch effect correction via 'method = ",
        method,
        "' was requested,\nbut no valid confounders were provided. ",
        "'method = simple' is used instead."
      ))
      method <- "simple"
    }

    res <- switch(
      method,
      "simple" = batchmean_simple(data = data, markers = {{ markers }}),
      "standardize" = batchmean_standardize(
        data = data,
        markers = {{ markers }},
        confounders = {{ confounders }}
      ),
      "ipw" = batchmean_ipw(
        data = data,
        markers = {{ markers }},
        confounders = {{ confounders }},
        truncate = ipw_truncate
      )
    )
    adjust_parameters <- purrr::map_dfr(
      .x = res,
      .f = \(x) purrr::pluck(x, "values")
    )
    method_indices <- c("simple" = 2, "standardize" = 3, "ipw" = 4)
    if (suffix == "_adjX") {
      suffix <- paste0("_adj", method_indices[method[1]])
    }

    values <- data |>
      dplyr::select(-dplyr::any_of({{ confounders }})) |>
      tidyr::pivot_longer(
        cols = c(-.data$.id, -.data$.batchvar),
        names_to = "marker",
        values_to = "value"
      ) |>
      dplyr::left_join(adjust_parameters, by = c("marker", ".batchvar")) |>
      dplyr::mutate(
        value_adjusted = .data$value - .data$batchmean,
        marker = paste0(.data$marker, suffix)
      ) |>
      dplyr::select(-.data$batchmean, -.data$value)
  }

  # Quantile regression
  if (method == "quantreg") {
    res <- purrr::map(
      .x = data |> dplyr::select({{ markers }}) |> names(),
      .f = batchrq,
      data = data |>
        dplyr::filter(
          dplyr::if_all(
            dplyr::all_of({{ confounders }}),
            \(x) !is.na(x)
          )
        ),
      confounders = dplyr::if_else(
        dplyr::enexpr(confounders) != "",
        true = paste0(
          "+ ",
          paste(
            dplyr::enexpr(confounders),
            sep = " + ",
            collapse = " + "
          )
        ),
        false = ""
      ),
      tau = quantreg_tau,
      rq_method = quantreg_method
    )
    adjust_parameters <- purrr::map_dfr(
      .x = res,
      .f = \(x) purrr::pluck(x, "values")
    )
    if (suffix == "_adjX") {
      suffix <- "_adj5"
    }

    values <- data |>
      tidyr::pivot_longer(
        cols = c(
          -.data$.id,
          -.data$.batchvar,
          -dplyr::any_of({{ confounders }})
        ),
        names_to = "marker",
        values_to = "value"
      ) |>
      dplyr::left_join(adjust_parameters, by = c("marker", ".batchvar")) |>
      dplyr::group_by(.data$marker) |>
      dplyr::mutate(
        value_adjusted = (.data$value - .data$un_lo) /
          .data$un_iq *
          .data$all_iq *
          (.data$un_iq / .data$ad_iq) +
          .data$all_lo -
          .data$ad_lo +
          .data$un_lo,
        marker = paste0(.data$marker, suffix)
      ) |>
      dplyr::select(
        -dplyr::any_of({{ confounders }}),
        -.data$value,
        -.data$un_lo,
        -.data$un_hi,
        -.data$ad_lo,
        -.data$ad_hi,
        -.data$un_iq,
        -.data$ad_iq,
        -.data$all_iq,
        -.data$all_lo
      )
  }

  # Quantile normalization
  if (method == "quantnorm") {
    if (suffix == "_adjX") {
      suffix <- "_adj6"
    }
    values <- data |>
      dplyr::select(-dplyr::any_of({{ confounders }})) |>
      tidyr::pivot_longer(
        cols = c(-.data$.id, -.data$.batchvar),
        names_to = "marker",
        values_to = "value"
      ) |>
      dplyr::mutate(marker = paste0(.data$marker, suffix)) |>
      dplyr::group_by(.data$marker) |>
      dplyr::mutate(
        value_adjusted = batch_quantnorm(
          var = .data$value,
          batch = .data$.batchvar
        )
      ) |>
      dplyr::ungroup() |>
      dplyr::select(-.data$value)
    res <- list(list(res = NULL, models = NULL))
    adjust_parameters <- tibble::tibble(
      marker = data |>
        dplyr::select({{ markers }}) |>
        names()
    )
  }

  # Dataset to return
  values <- values |>
    tidyr::pivot_wider(
      names_from = .data$marker,
      values_from = .data$value_adjusted
    ) |>
    dplyr::select(-.data$.batchvar) |>
    dplyr::left_join(x = data_orig, by = ".id") |>
    dplyr::select(-.data$.id)

  # Meta-data to return as attribute
  attr_list <- list(
    adjust_method = method,
    markers = data_orig |>
      dplyr::select({{ markers }}) |>
      names(),
    suffix = suffix,
    batchvar = data_orig |>
      dplyr::select({{ batch }}) |>
      names(),
    confounders = dplyr::enexpr(confounders),
    adjust_parameters = adjust_parameters,
    model_fits = purrr::map(
      .x = res,
      .f = \(x) purrr::pluck(x, "models")
    )
  )
  class(attr_list) <- c("batchtma", class(res))
  attr(values, which = ".batchtma") <- attr_list
  return(values)
}
