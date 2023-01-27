
#' @name ggplot_tornado
#' @title ggplot tornado
#'
#' @description Create a tornado plot for a cost-effectiveness one-way sensitivity analysis.
#' Supply the parameter names and maximum and minimum values for an output
#' statistic of interest e.g. ICER or INMB.
#' These need to be calculated before hand and in correct format (see \code{create_tornado_data}).
#'
#' @param dat Data
#' @param ... Additional arguments
#'
#' @return plot
#' @export
#'
ggplot_tornado <- function(dat, ...) {
  UseMethod("ggplot_tornado", dat)
}

#' @rdname ggplot_tornado
#'
ggplot_tornado.default <- function(dat, ...) {
  stop("Input data must be tornado class data frame.")
}


#' @rdname ggplot_tornado
#'
#' @param dat Data frame of output maximum and minimum values. Format should be
#' \tabular{rrr}{
#'  val \tab names \tab output\cr
#' min \tab sens \tab 5\cr
#' max \tab sens \tab 11\cr
#' min \tab spec \tab 10\cr
#' max \tab spec \tab 1
#' }
#'
#' @param baseline_output Values of output for baseline input parameter values to
#'    compare maximum and minimum against (default: NA)
#' @param annotate_nudge Scale how much annotation is moved left and right (default: 0)
#' @param ORDER Automatically order the bars by length (default: TRUE)
#'
#' @return ggplot object
#' @export
#'
#' @seealso \code{\link{create_tornado_data}}
#' @examples
#'
#' library(magrittr)
#' library(reshape2)
#' library(plyr)
#' library(purrr)
#' library(dplyr)
#' library(ggplot2)
#'
#' ## user defined ##
#' dat <- data.frame(names = c("Specificity (min:0.8; max:1)",
#'                            "Sensitivity (min:0.8; max:1)",
#'                            "Cost of rule-out test (min:£10; max:£100)",
#'                            "Prevalence (min:40%; max:50%)",
#'                            "Dosanjh category 3 status (all active TB/all non-TB)",
#'                            "TB ruled-out non-TB patient 6 week follow-up (min:0%; max:10%)"),
#'                   min = c(1,2,2,2.4,2.5,1.6),
#'                   max = c(3,5,6,7,8,9))
#'
#' dat <- melt(dat, id.vars = "names",
#'             variable.name = "val",
#'             value.name = "output") %>%
#'               arrange(names)
#'
#' class(dat) <- c("tornado", class(dat))
#' attr(dat, "output_name") <- "output"
#'
#' baseline_output <- 3
#' ggplot_tornado(dat, baseline_output)
#'
#' ## model ouput ##
#' psa_dat <- data.frame(output = c(10,1,11,5,3),
#'                       sens = c(2,2,3,0,2),
#'                       spec = c(1,4,2,2,2))
#'
#' psa_dat <- model.frame(formula = output ~ sens + spec,
#'                        data = psa_dat)
#'
#' psa_dat %>%
#'    create_tornado_data %>%
#'    ggplot_tornado(baseline_output = 6)
#'
ggplot_tornado.tornado <- function(dat,
                                   baseline_output = NA,
                                   annotate_nudge = 0,
                                   ORDER = TRUE) {

  output_name <- attr(dat, "output_name")

  if (length(baseline_output) != 1)
    stop("Input baseline_output must be length one.")

  if (is.na(baseline_output)) {
    baseline_output <- attr(dat, "baseline")[output_name]
  }

  dat$baseline <- unlist(baseline_output, use.names = FALSE)

  # don't strictly need this
  # order output columns as decending and ascending
  datplot <-
    dat[, c(output_name, "baseline")] %>%
    dplyr::mutate("min" = apply(., 1, min),
                  "max" = apply(., 1, max)) %>%
    dplyr::select(min, max)

  datplot <- cbind(dat, datplot)
  NAMES <- as.character(datplot$names)

  # order by length of bars
  ##TODO## assumes symmetrical; what about otherwise?
  if (ORDER) {
    datplot$names <-
      factor(as.character(datplot$names),
             levels = rev(unique(datplot$names[order(datplot$min, decreasing = FALSE)])))
  }

  # check if parameter values are provided
  if (all(NAMES %in% names(datplot))) {
    barLabels <- datplot[, NAMES] %>%
      OpenMx::diag2vec()
  } else {barLabels <- ""}

  # shift annotation left or right
  nudge <-
    (with(datplot, eval(parse(text = output_name)) > baseline) - 0.5) * annotate_nudge

  ggplot2::ggplot(datplot,
                  aes(names, ymin = min, ymax = max, colour = val)) +
    geom_linerange(size = 10) +
    coord_flip() +
    xlab("") +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_hline(yintercept = dat$baseline, linetype = "dashed") +
    theme_bw() +
    theme(axis.text = element_text(size = 15),
          # legend.position = "none",
          legend.title = element_blank()) +
    annotate("text",
             x = datplot$names,
             y = datplot[, output_name] + nudge,
             label = barLabels)
}

