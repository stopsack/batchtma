#' Plot biomarkers by batch
#'
#' @description
#' To provide a simple visualization of potential batch
#' effects, \code{plot_batch} generates a Tukey
#' box plot overlaid by a jittered
#' dot plot, inspired by the Stata plugin \code{stripplot}.
#'
#' Boxes span from the 1st to the 3rd quartile; thick lines
#' indicate medians; whiskers span up to 1.5 times the
#' interquartile range; and asterisks indicate means.
#'
#' More powerful visualizations of batch effects exist
#' in the BatchQC package, see references and links.
#'
#' @param data Dataset.
#' @param marker Variable indicating the biomarker.
#' @param batch Variable indicating the batch.
#' @param color Optional: third variable to use for symbol
#'   color and shape. For example, \code{color} can be used
#'   to show differences in a confounder.
#' @param maxlevels Optional: Maximum number of
#'   levels for \code{color} parameter to accept as a discrete
#'   variable, rather than a continuous variable.
#'   Defaults to \code{15}.
#' @param ... Optional: Passed on \code{\link[ggplot2]{ggplot}}.
#'
#' @return ggplot2 object, which can be further
#'   modified using standard ggplot2 functions. See examples.
#' @importFrom ggplot2 labs
#' @export
#'
#' @references
#' Cox NJ (2003). STRIPPLOT: Stata module for strip plots
#' (one-way dot plots). Statistical Software Components
#' S433401, Boston College Department of Economics,
#' revised 11 Oct 2020.
#'
#' Manimaran S, Selby HM, Okrah K, Ruberman C, Leek JT,
#' Quackenbush J, Haibe-Kains B, Bravo HC, Johnson WE (2016).
#' BatchQC: interactive software for evaluating sample
#' and batch effects in genomic data. Bioinformatics.
#' doi:10.1093/bioinformatics/btw538
#'
#' @seealso
#' \url{https://doi.org/10.1093/bioinformatics/btw538}
#' \url{http://bioconductor.org/packages/release/bioc/html/BatchQC.html}
#'
#' @examples
#' # Define example data
#' df <- data.frame(tma = rep(1:2, times = 10),
#'                  biomarker = rep(1:2, times = 10) +
#'                    runif(max = 5, n = 20),
#'                  confounder = rep(0:1, times = 10) +
#'                    runif(max = 10, n = 20))
#'
#' # Visualize batch effects:
#' plot_batch(data = df, marker = biomarker,
#'            batch = tma, color = confounder)
#'
#' # Label y-axis, changing graph like other ggplots:
#' plot_batch(data = df, marker = biomarker,
#'            batch = tma, color = confounder) +
#'   ggplot2::labs(y = "Biomarker (variable 'noisy')")
plot_batch <- function(data, marker, batch, color = NULL, maxlevels = 15, ...) {
  marker <- dplyr::enquo(marker)
  batch <- dplyr::enquo(batch)
  color <- dplyr::enquo(color)
  nlevels <- 101
  data2 <- data %>% dplyr::select(colvar = !!color)

  # Find out if color variable is "discrete"
  if(length(names(data2)) > 0)
    nlevels <- data2 %>%
    dplyr::mutate(colvar = factor(.data$colvar)) %>%
    dplyr::summarize(colvar = length(levels(.data$colvar))) %>%
    dplyr::pull(.data$colvar)
  if(nlevels < maxlevels)
    data <- data %>% dplyr::mutate(!!color := factor(!!color))

  myplot <- ggplot2::ggplot(data = dplyr::mutate(data, !!batch := factor(!!batch)),
                            mapping = ggplot2::aes(x = !!batch, y = !!marker), ...) +
    ggplot2::geom_boxplot(outlier.shape = NA, color = "black") +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                   axis.text = ggplot2::element_text(size = 10, color = "black")) +
    ggplot2::stat_summary(geom = "point", fun = "mean", col = "black",
                          size = 5, shape = 8, stroke = 1, fill = "black")

  if(nlevels < maxlevels)
    myplot +
    ggplot2::geom_jitter(width = 0.2, height = 0,
                         mapping = ggplot2::aes(color = !!color, shape = !!color)) +
    ggplot2::scale_shape_manual(name = color, values = 15:30) +
    ggplot2::scale_color_viridis_d(name = color, option = "cividis")
  else
    myplot +
    ggplot2::geom_jitter(width = 0.2, height = 0,
                         mapping = ggplot2::aes(color = !!color)) +
    ggplot2::scale_color_viridis_c(name = color, option = "cividis")
}
