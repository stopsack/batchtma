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
batchmean_ipw <- function(
  data,
  markers,
  confounders,
  truncate = c(0.025, 0.975)
) {
  ipwbatch <- function(data, variable, confounders, truncate) {
    data <- data |>
      dplyr::rename(variable = dplyr::one_of(variable)) |>
      dplyr::filter(!is.na(.data$variable)) |>
      dplyr::mutate(.batchvar = factor_drop(.data$.batchvar))

    res <- data |>
      tidyr::nest(data = dplyr::everything()) |>
      dplyr::mutate(
        num = purrr::map(
          .x = .data$data,
          .f = \(x) {
            nnet::multinom(
              formula = .batchvar ~ 1,
              data = x,
              trace = FALSE
            )
          }
        ),
        den = purrr::map(
          .x = .data$data,
          .f = \(x) {
            nnet::multinom(
              formula = stats::as.formula(
                paste(".batchvar ~", confounders)
              ),
              data = x,
              trace = FALSE
            )
          }
        )
      )

    values <- res |>
      dplyr::mutate_at(
        .vars = dplyr::vars(.data$num, .data$den),
        .funs = \(num_den) {
          purrr::map(
            .x = num_den,
            .f = \(x) {
              stats::predict(
                x,
                type = "probs"
              )
            }
          ) |>
            purrr::map(
              .x = _,
              .f = tibble::as_tibble
            ) |>
            purrr::map2(
              .x = _,
              .y = .data$data,
              .f = \(x, y) {
                x |>
                  dplyr::mutate(
                    .batchvar = y |>
                      purrr::pluck(".batchvar")
                  )
              }
            )
        }
      )

    # multinom()$fitted.values is just a vector of probabilities for
    # the 2nd outcome level if there are only two levels
    if (length(levels(factor(data$.batchvar))) == 2) {
      values <- values |>
        dplyr::mutate_at(
          .vars = dplyr::vars(.data$num, .data$den),
          .funs = \(num_den) {
            purrr::map(
              .x = num_den,
              .f = \(x) {
                x |>
                  dplyr::mutate(
                    probs = dplyr::if_else(
                      .data$.batchvar == levels(factor(.data$.batchvar))[1],
                      true = 1 - .data$value,
                      false = .data$value
                    )
                  ) |>
                  dplyr::pull(.data$probs)
              }
            )
          }
        )
      # otherwise probabilities are a data frame
    } else {
      values <- values |>
        dplyr::mutate_at(
          .vars = dplyr::vars(.data$num, .data$den),
          .funs = \(num_den) {
            purrr::map(
              .x = num_den,
              .f = \(x) {
                x |>
                  tidyr::pivot_longer(
                    -.data$.batchvar,
                    names_to = "batch",
                    values_to = "prob"
                  ) |>
                  dplyr::filter(.data$batch == .data$.batchvar) |>
                  dplyr::pull(.data$prob)
              }
            )
          }
        )
    }

    values <- values |>
      tidyr::unnest(cols = c(.data$data, .data$num, .data$den)) |>
      dplyr::mutate(
        sw = .data$num / .data$den,
        trunc = dplyr::case_when(
          .data$sw < stats::quantile(.data$sw, truncate[1]) ~
            stats::quantile(.data$sw, truncate[1]),
          .data$sw > stats::quantile(.data$sw, truncate[2]) ~
            stats::quantile(.data$sw, truncate[2]),
          TRUE ~ .data$sw
        )
      )

    xlev <- unique(data |> dplyr::pull(.data$.batchvar))

    values <- geepack::geeglm(
      formula = variable ~ .batchvar,
      data = values,
      weights = values$trunc,
      id = values$.id,
      corstr = "independence"
    ) |>
      broom::tidy() |>
      dplyr::filter(
        !stringr::str_detect(
          string = .data$term,
          pattern = "(Intercept)"
        )
      ) |>
      dplyr::mutate(
        term = as.character(
          stringr::str_remove_all(
            string = .data$term,
            pattern = ".batchvar"
          )
        )
      ) |>
      dplyr::full_join(
        tibble::tibble(term = as.character(xlev)),
        by = "term"
      ) |>
      dplyr::mutate(
        estimate = dplyr::if_else(
          is.na(.data$estimate),
          true = 0,
          false = .data$estimate
        ),
        estimate = .data$estimate - mean(.data$estimate),
        marker = variable,
        term = .data$term
      ) |>
      dplyr::arrange(.data$term) |>
      dplyr::select(
        .data$marker,
        .batchvar = .data$term,
        batchmean = .data$estimate
      )
    list(values = values, models = res |> dplyr::pull(.data$den))
  }

  purrr::map(
    .x = data |> dplyr::select({{ markers }}) |> names(),
    .f = ipwbatch,
    data = data |>
      dplyr::filter(
        dplyr::if_all(
          .cols = dplyr::all_of(confounders),
          .fns = \(x) !is.na(x)
        )
      ),
    truncate = truncate,
    confounders = paste(confounders, sep = " + ", collapse = " + ")
  )
}
