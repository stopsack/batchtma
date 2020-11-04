# as per https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R:
## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

#' Batch means for approach 2: Unadjusted means
#'
#' @param data Data set
#' @param markers Biomarkers to adjust
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @return Tibble of means per marker and batch
#' @noRd
batchmean_simple <- function(data, markers) {
  markers <- dplyr::enquo(markers)
  values <- data %>%
    dplyr::select(.data$.id, .data$.batchvar, !!markers) %>%
    dplyr::group_by(.data$.batchvar) %>%
    dplyr::summarize_at(.vars = dplyr::vars(-.data$.id),
                        .funs = mean, na.rm = TRUE) %>%
    dplyr::mutate_at(.vars = dplyr::vars(-.data$.batchvar),
                     .funs = ~. - mean(., na.rm = TRUE)) %>%
    tidyr::pivot_longer(col = c(-.data$.batchvar),
                        names_to = "marker",
                        values_to = "batchmean")
  return(list(list(values = values, models = NULL)))
}

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
  markers <- dplyr::enquo(markers)
  res <- data %>%
    tidyr::pivot_longer(cols = !!markers,
                        names_to = "marker",
                        values_to = "value") %>%
    dplyr::filter(!is.na(.data$value)) %>%
    dplyr::group_by(.data$marker) %>%
    tidyr::nest(data = c(-.data$marker)) %>%
    dplyr::mutate(
      model = purrr::map(
        .x = .data$data,
        .f = ~stats::lm(
          formula = stats::as.formula(paste0("value ~ .batchvar +",
                                             paste(confounders,
                                                   collapse = " + ",
                                                   sep = " + "))),
          data = .x)),
      .batchvar = purrr::map(.x = .data$data,
                             .f = ~.x %>% dplyr::pull(.data$.batchvar) %>% levels()))

  values <- res %>%
    tidyr::unnest(cols = .data$.batchvar) %>%
    dplyr::mutate(
      data = purrr::map2(.x = .data$data, .y = .data$.batchvar,
                         .f = ~.x %>% dplyr::mutate(.batchvar = .y)),
      pred = purrr::map2(.x = .data$model, .y = .data$data, .f = stats::predict)) %>%
    dplyr::select(.data$marker, .data$.batchvar, .data$pred) %>%
    tidyr::unnest(cols = .data$pred) %>%
    dplyr::group_by(.data$marker, .data$.batchvar) %>%
    dplyr::summarize(batchmean = mean(.data$pred)) %>%
    dplyr::group_by(.data$marker) %>%
    dplyr::mutate(markermean = mean(.data$batchmean)) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(marker = .data$marker,
                     .batchvar = .data$.batchvar,
                     batchmean = .data$batchmean - .data$markermean)
  return(list(list(models = res %>% dplyr::ungroup() %>% dplyr::pull("model"),
                   values = values)))
}

#' Batch means for approach 4: IPW
#'
#' @param data Data set
#' @param markers Variables to batch-adjust
#' @param confounders Confounders: features that differ
#' @param truncate Lower and upper extreme quantiles to
#'   truncate stabilized weights at. Defaults to c(0.025, 0.975).
#'
#' @return Tibble of batch means per batch and marker
#' @noRd
batchmean_ipw <- function(data, markers, confounders,
                          truncate = c(0.025, 0.975)) {
  markers <- dplyr::enquo(markers)
  ipwbatch <- function(data, variable, confounders, truncate) {
    res <- data %>%
      dplyr::rename(variable = dplyr::one_of(variable)) %>%
      dplyr::filter(!is.na(.data$variable)) %>%
      tidyr::nest(data = dplyr::everything()) %>%
      dplyr::mutate(
        num = purrr::map(.x = .data$data,
                         .f = ~nnet::multinom(formula = .batchvar ~ 1,
                                              data = .x, trace = FALSE)),
        den = purrr::map(.x = .data$data,
                         .f = ~nnet::multinom(formula = stats::as.formula(
                           paste(".batchvar ~", confounders)),
                           data = .x, trace = FALSE)))

    values <- res %>%
      dplyr::mutate_at(.vars = dplyr::vars(.data$num, .data$den),
                       .funs = ~purrr::map(.x = ., .f = stats::predict, type = "probs") %>%
                         purrr::map(.x = ., .f = tibble::as_tibble) %>%
                         purrr::map2(.x = ., .y = .data$data,
                                     .f = ~.x %>%
                                       dplyr::mutate(.batchvar = .y %>%
                                                       purrr::pluck(".batchvar"))))

    # multinom()$fitted.values is just a vector of probabilities for
    # the 2nd outcome level if there are only two levels
    if(length(levels(factor(data$.batchvar))) == 2) {
      values <- values %>%
        dplyr::mutate_at(.vars = dplyr::vars(.data$num, .data$den),
                         .funs = ~purrr::map(.x = ., .f = ~.x %>%
                                               dplyr::mutate(
                                                 probs = dplyr::if_else(
                                                   .data$.batchvar ==
                                                     levels(factor(.data$.batchvar))[1],
                                                   true = 1 - .data$value,
                                                   false = .data$value)) %>%
                                               dplyr::pull(.data$probs)))
    # otherwise probabilities are a data frame
    } else {
      values <- values %>%
        dplyr::mutate_at(.vars = dplyr::vars(.data$num, .data$den),
                         .funs =
                           ~purrr::map(.x = .,
                                       .f = ~.x %>%
                                         tidyr::pivot_longer(-.data$.batchvar,
                                                             names_to = "batch",
                                                             values_to = "prob") %>%
                                         dplyr::filter(.data$batch == .data$.batchvar) %>%
                                         dplyr::pull(.data$prob)))
    }

    values <- values %>%
      tidyr::unnest(cols = c(.data$data, .data$num, .data$den)) %>%
      dplyr::mutate(sw = .data$num / .data$den,
                    trunc = dplyr::case_when(
                      .data$sw < stats::quantile(.data$sw, truncate[1]) ~
                        stats::quantile(.data$sw, truncate[1]),
                      .data$sw > stats::quantile(.data$sw, truncate[2]) ~
                        stats::quantile(.data$sw, truncate[2]),
                      TRUE ~ .data$sw))

    xlev <- unique(data %>% dplyr::pull(.data$.batchvar))

    values <- geepack::geeglm(formula = variable ~ .batchvar,
                    data = values, weights = values$trunc,
                    id = values$.id, corstr = "independence") %>%
      broom::tidy() %>%
      dplyr::filter(!stringr::str_detect(string = .data$term,
                                         pattern = "(Intercept)")) %>%
      dplyr::mutate(term = as.character(
        stringr::str_remove_all(string = .data$term,
                                pattern = ".batchvar"))) %>%
      dplyr::full_join(tibble::tibble(term = as.character(xlev)), by = "term") %>%
      dplyr::mutate(estimate = dplyr::if_else(is.na(.data$estimate),
                                              true = 0, false = .data$estimate),
                    estimate = .data$estimate - mean(.data$estimate),
                    marker   = variable,
                    term     = .data$term) %>%
      dplyr::arrange(.data$term) %>%
      dplyr::select(.data$marker, .batchvar = .data$term, batchmean = .data$estimate)
    list(values = values, models = res %>% dplyr::pull(.data$den))
  }

  purrr::map(.x = data %>% dplyr::select(!!markers) %>% names(),
                    .f = ipwbatch,
                    data = data, truncate = truncate,
                    confounders = paste(confounders, sep = " + ", collapse = " + "))
}


#' Quantiles for approach 5: Quantile regression
#'
#' @param data Data set
#' @param variable Single variable to batch-adjust
#' @param confounders Confounders: features that differ
#' @param tau Quantiles to use for scaling
#'
#' @return Tibble of quantiles per batch
#' @noRd
batchrq <- function(data, variable, confounders, tau) {
  variable <- dplyr::enquo(variable)
  res <- data %>%
    dplyr::rename(variable = !!variable) %>%
    dplyr::filter(!is.na(.data$variable)) %>%
    tidyr::nest(data = dplyr::everything()) %>%
    dplyr::mutate(
      un = purrr::map(.x = .data$data,
                      .f = ~quantreg::rq(formula = variable ~ .batchvar,
                                         data = .x, tau = tau, method = "fn")),
      ad = purrr::map(.x = .data$data,
                      .f = ~quantreg::rq(formula = stats::as.formula(
                        paste("variable ~ .batchvar", confounders)),
                        data = .x, tau = tau, method = "fn")),
      .batchvar = purrr::map(.x = .data$data,
                             .f = ~.x %>% dplyr::pull(.data$.batchvar) %>% levels()))

  values <- res %>%
    tidyr::unnest(cols = .data$.batchvar) %>%
    dplyr::mutate(
      data = purrr::map2(.x = .data$data, .y = .data$.batchvar,
                         .f = ~.x %>% dplyr::mutate(.batchvar = .y)),
      un = purrr::map2(.x = .data$un, .y = .data$data, .f = stats::predict),
      ad = purrr::map2(.x = .data$ad, .y = .data$data, .f = stats::predict),
      un = purrr::map(.x = .data$un,
                      .f = tibble::as_tibble,
                      .name_repair = ~c("un_lo", "un_hi")),
      ad = purrr::map(.x = .data$ad,
                      .f = tibble::as_tibble,
                      .name_repair = ~c("ad_lo", "ad_hi")),
      all_lo = purrr::map_dbl(.x = .data$data,
                              .f = ~stats::quantile(.x$variable, probs = 0.25)),
      all_hi = purrr::map_dbl(.x = .data$data,
                              .f = ~stats::quantile(.x$variable, probs = 0.75)),
      all_iq = .data$all_hi - .data$all_lo) %>%
    dplyr::select(.data$.batchvar, .data$un, .data$ad,
                  .data$all_lo, .data$all_hi, .data$all_iq) %>%
    tidyr::unnest(cols = c(.data$un, .data$ad)) %>%
    dplyr::group_by(.data$.batchvar) %>%
    dplyr::summarize(un_lo  = stats::quantile(.data$un_lo, probs = 0.25),
                     ad_lo  = stats::quantile(.data$ad_lo, probs = 0.25),
                     un_hi  = stats::quantile(.data$un_hi, probs = 0.75),
                     ad_hi  = stats::quantile(.data$ad_hi, probs = 0.75),
                     all_lo = stats::median(.data$all_lo),
                     all_iq = stats::median(.data$all_iq)) %>%
    dplyr::mutate(un_iq  = .data$un_hi - .data$un_lo,
                  ad_iq  = .data$ad_hi - .data$ad_lo,
                  marker = !!variable)

  models <- res %>% dplyr::pull(.data$ad)
  return(tibble::lst(values, models))
}

#
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
    dplyr::select_if(~!all(is.na(.))) %>%
    as.matrix() %>%
    limma::normalizeQuantiles() %>%
    tibble::as_tibble() %>%
    dplyr::transmute(result = purrr::pmap_dbl(.l = .,
                                              .f = function(...)
                                                mean(c(...), na.rm = TRUE)),
                     result = dplyr::if_else(is.nan(.data$result),
                                             true = NA_real_, false = .data$result)) %>%
    dplyr::pull(.data$result)
}

#' Adjust for batch effects
#'
#' @description
#' \code{adjust_batch} generates biomarker levels for the variable(s)
#' \code{markers} in the dataset \code{data} that are corrected
#' (adjusted) for batch effects, i.e. differential measurement
#' error between levels of \code{batch}.
#'
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
#'     defined by the \code{truncate} parameters. If no
#'     \code{confounders} are supplied, \code{method = simple}
#'     is equivalent and will be used.
#'   * \code{quantreg}  Lower quantiles (default: 25th percentile)
#'     and ranges between a lower and an upper quantile (default: 75th
#'     percentile) will be unified between batches, allowing for
#'     differences in both parameters due to confounders. Set the two
#'     quantiles using the \code{tau} parameters.
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
#' @param truncate Optional and used for \code{method = ipw} only:
#'   Lower and upper extreme quantiles to truncate stabilized
#'   weights at. Defaults to \code{c(0.025, 0.975)}.
#' @param tau Optional and used for \code{method = quantreg} only:
#'   Quantiles to scale. Defaults to \code{c(0.25, 0.75)}.
#'
#' @return The \code{data} dataset with batch effect-adjusted
#'   variable(s) added at the end. Model diagnostics, using
#'   the attribute \code{.batchtma} of this dataset, are available
#'   via the \code{\link[batchtma]{diagnose_models}} function.
#' @importFrom rlang :=
#' @export
#'
#' @references
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
#' Stopsack KH et al. Batch effects in tumor
#' biomarker studies using tissue microarrays: Extent,
#' impact, and remediation. In preparation.
#' (This R package; \code{method = c(standardize, ipw,
#' quantreg)}; method comparisons)
#'
#' @author Konrad H. Stopsack \url{stopsack@mskcc.org}
#' @seealso \url{https://stopsack.github.io/batchtma}
#'
#' @examples
#' # Data frame with two batches
#' # Batch 2 has higher values of biomarker and confounder
#' df <- data.frame(tma = rep(1:2, times = 10),
#'                  biomarker = rep(1:2, times = 10) +
#'                    runif(max = 5, n = 20),
#'                  confounder = rep(0:1, times = 10) +
#'                    runif(max = 10, n = 20))
#'
#' # Adjust for batch effects
#' # Using simple means, ignoring the confounder:
#' adjust_batch(data = df, markers = biomarker,
#'              batch = tma, method = simple)
#' # Returns data set with new variable "biomarker_adj2"
#'
#' # Use quantile regression, include the confounder,
#' # change suffix of returned variable:
#' adjust_batch(data = df, markers = biomarker,
#'              batch = tma, method = quantreg,
#'              confounders = confounder,
#'              suffix = "_batchadjusted")
#' # Returns data set with new variable "biomarker_batchadjusted"

adjust_batch <- function(data, markers, batch,
                         method = c("simple", "standardize", "ipw",
                                    "quantreg", "quantnorm"),
                         confounders = NULL,
                         suffix = "_adjX",
                         truncate = c(0.025, 0.975),
                         tau = c(0.25, 0.75)) {
  method <- as.character(dplyr::enexpr(method))
  allmethods <- c("simple", "standardize", "ipw", "quantreg", "quantnorm")
  markers     <- dplyr::enquo(markers)
  batch       <- dplyr::enquo(batch)
  confounders <- dplyr::enquo(confounders)
  data_orig <- data %>%
    dplyr::mutate(.id = dplyr::row_number())
  data <- data_orig %>%
    dplyr::rename(.batchvar = !!batch) %>%
    dplyr::mutate(.batchvar = factor(.data$.batchvar)) %>%
    dplyr::select(.data$.id, .data$.batchvar, !!markers, !!confounders)
  confounders <- data %>% dplyr::select(!!confounders) %>% names()

  # Check inputs: method and confounders
  if(!(method[1] %in% allmethods))
    stop(paste0("Method '", method[1],
                "' is not implemented.\nAvailable methods: ",
                paste(allmethods, collapse = ", "), ".\n",
                "See: help(\"adjust_batch\")."))

  if(method %in% c("simple", "quantnorm") &
     data %>% dplyr::select(!!confounders) %>% ncol() > 0) {
    message(paste0("Batch effect correction via 'method = ", method,
                   "' was requested.\n This method does not support ",
                   "adjustment for confounders (",
                   paste(confounders, sep = ", ", collapse = ", "),
                   "). They will be ignored."))
    data <- data %>% dplyr::select(-dplyr::any_of(confounders))
    confounders <- NULL
  }

  # Mean-based methods
  if(method %in% c("simple", "standardize", "ipw")) {
    if(method %in% c("standardize", "ipw") &
       data %>% dplyr::select(!!confounders) %>% ncol() == 0) {
      message(paste0("Batch effect correction via 'method = ", method,
                     "' was requested,\nbut no valid confounders were provided. ",
                     "'method = simple' is used instead."))
      method <- "simple"
    }

    res <- switch(
      method,
      "simple" = batchmean_simple(data = data, markers = !!markers),
      "standardize" = batchmean_standardize(data = data, markers = !!markers,
                                            confounders = confounders),
      "ipw"    = batchmean_ipw(   data = data, markers = !!markers,
                                  confounders = confounders))
    adjust_parameters <- purrr::map_dfr(.x = res, .f = ~purrr::pluck(.x, "values"))
    method_indices <- c("simple" = 2, "standardize" = 3, "ipw" = 4)
    if(suffix == "_adjX")
      suffix <- paste0("_adj", method_indices[method[1]])

    values <- data %>%
      dplyr::select(-dplyr::any_of(confounders)) %>%
      tidyr::pivot_longer(cols = c(-.data$.id, -.data$.batchvar),
                          names_to = "marker",
                          values_to = "value") %>%
      dplyr::left_join(adjust_parameters, by = c("marker", ".batchvar")) %>%
      dplyr::mutate(value_adjusted = .data$value - .data$batchmean,
                    marker = paste0(.data$marker, suffix)) %>%
      dplyr::select(-.data$batchmean, -.data$value)
  }

  # Quantile regression
  if(method == "quantreg") {
    res <- purrr::map(
      .x = data %>% dplyr::select(!!markers) %>% names(),
      .f = batchrq,
      data        = data,
      confounders = dplyr::if_else(confounders != "",
                                   true  = paste0("+ ",
                                                  paste(confounders,
                                                        sep = " + ", collapse = " + ")),
                                   false = ""),
      tau         = tau)
    adjust_parameters <- purrr::map_dfr(.x = res, .f = ~purrr::pluck(.x, "values"))
    if(suffix == "_adjX")
      suffix <- "_adj5"

    values <- data %>%
      tidyr::pivot_longer(cols = c(-.data$.id, -.data$.batchvar,
                                   -dplyr::any_of(confounders)),
                          names_to = "marker",
                          values_to = "value") %>%
      dplyr::left_join(adjust_parameters, by = c("marker", ".batchvar")) %>%
      dplyr::group_by(.data$marker) %>%
      dplyr::mutate(value_adjusted = (.data$value - .data$un_lo) /
                      .data$un_iq * .data$all_iq * (.data$un_iq / .data$ad_iq) +
                      .data$all_lo - .data$ad_lo + .data$un_lo,
                    marker = paste0(.data$marker, suffix)) %>%
      dplyr::select(-dplyr::any_of(confounders),
                    -.data$value, -.data$un_lo, -.data$un_hi,
                    -.data$ad_lo, -.data$ad_hi, -.data$un_iq, -.data$ad_iq,
                    -.data$all_iq, -.data$all_lo)
  }

  # Quantile normalization
  if(method == "quantnorm") {
    if(suffix == "_adjX")
      suffix <- "_adj6"
    values <- data %>%
      dplyr::select(-dplyr::any_of(confounders)) %>%
      tidyr::pivot_longer(cols = c(-.data$.id, -.data$.batchvar),
                          names_to = "marker",
                          values_to = "value") %>%
      dplyr::mutate(marker = paste0(.data$marker, suffix)) %>%
      dplyr::group_by(.data$marker) %>%
      dplyr::mutate(value_adjusted = batch_quantnorm(var = .data$value,
                                                     batch = .data$.batchvar)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-.data$value)
    res <- list(list(res = NULL, models = NULL))
    adjust_parameters <- tibble::tibble(marker = data %>%
                                          dplyr::select(!!markers) %>% names())
  }

  # Dataset to return
  values <- values %>%
    tidyr::pivot_wider(names_from  = .data$marker,
                       values_from = .data$value_adjusted) %>%
    dplyr::select(-.data$.batchvar) %>%
    dplyr::left_join(x = data_orig, by = ".id") %>%
    dplyr::select(-.data$.id)

  # Meta-data to return as attribute
  attr_list <- list(
    adjust_method     = method,
    markers           = data_orig %>% dplyr::select(!!markers) %>% names(),
    suffix            = suffix,
    batchvar          = data_orig %>% dplyr::select(!!batch) %>% names(),
    confounders       = confounders,
    adjust_parameters = adjust_parameters,
    model_fits        = purrr::map(.x = res, .f = ~purrr::pluck(.x, "models")))
  class(attr_list) <- c("batchtma", class(res))
  attr(values, which = ".batchtma") <- attr_list
  return(values)
}
