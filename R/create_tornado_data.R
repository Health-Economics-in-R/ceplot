
#' Convert Sensitivity Analysis Output Data to Tornado Plot Input Data
#'
#' A pre-processing step is required to use \code{ggplot_tornado}.
#'
#' This can be thought of as going from a multi-way design to a one-way design.
#'
#' The output data from a multivariate sensitivity analysis are usually in the form
#' of a grid of input parameter values and a column of associated output values.
#'
#' \tabular{rrrr}{
#'   \cr
#'   variable1 \tab variable2 \tab variable3 \tab output\cr
#'   0 \tab 0 \tab 0 \tab 1\cr
#'   1 \tab 0 \tab 0 \tab 2\cr
#'   0 \tab 1 \tab 0 \tab 5\cr
#' }
#'
#' For a tornado plot we only want to keep the maximum and minimum output values
#' for each parameter when all others are set at baseline (one-way analysis).
#'
#' We also want to record whether low parameter values give low output values
#' or vice-versa.
#'
#' @param psa_dat model.frame object
#' @param baseline_input Named vector of baseline parameter values
#'
#' @return Data frame of class tornado
#' @export
#'
#' @seealso \code{\link{ggplot_tornado}}
#' @examples
#'
#' psa_dat <- data.frame(output = c(10,1,11,5,3),
#'                       sens = c(2,2,3,0,2),
#'                       spec = c(1,4,2,2,2))
#'
#' psa_dat <- model.frame(formula = output ~ sens + spec,
#'                        data = psa_dat)
#'
#' create_tornado_data(psa_dat)
#'
create_tornado_data <- function(psa_dat,
                                baseline_input = NA){

  if (!(is.data.frame(psa_dat) & typeof(psa_dat) == "list")) {
    stop("Require model.frame type as input data.")
  }

  if (!is.vector(baseline_input)) stop("baseline_input must be a vector.")

  output_name <- terms(psa_dat)[[2]]
  design_matrix <- subset(x = psa_dat,
                          select = -eval(parse(text = output_name)))

  n.params <- ncol(design_matrix)

  # parameters upper and lower limits
  MINS <- apply(design_matrix, 2, min, na.rm = TRUE)
  MAXS <- apply(design_matrix, 2, max, na.rm = TRUE)

  # if baseline parameter not provided
  # use an average
  # assuming using each parameter average exist as a scenario
  ##TODO: when median is half way match(TRUE, round(0.05, 1:20) == x)
  if (is.na(baseline_input)) {
    baseline_input <- apply(design_matrix, 2, function(x) median(x))

    # maintain ordering
    var_names <- names(psa_dat)[names(psa_dat) %in% names(baseline_input)]

    baseline_input <-
      baseline_input %>%
      matrix(nrow = 1) %>%
      set_colnames(var_names)

    # match with output
    baseline_input <- merge(psa_dat, baseline_input, all.y = TRUE, all.x = FALSE)
  }

  print(paste("baseline values estimated as:", paste(baseline_input, collapse = " ")))

  # create empty array for combinations of
  # max/min and baseline values for each parameter
  # i.e. one-way sensitivity analysis
  diag_array  <- diag(length(MAXS))
  diag_array[diag_array == 0] <- NA

  param_names <- names(MAXS)

  subgrid_max <- data.frame(apply(diag_array, 1, function(y) MAXS*y),
                            val = "max",
                            names = param_names, row.names = NULL)

  subgrid_min <- data.frame(apply(diag_array, 1, function(y) MINS*y),
                            val = "min",
                            names = param_names, row.names = NULL)

  SUBGRID <- rbind(subgrid_max, subgrid_min) %>%
    set_names(c(names(MINS), "val", "names"))

  # substitute in the baseline values
  for (param in seq_len(n.params)) {
    where_baseline <- is.na(SUBGRID[, param])
    SUBGRID[where_baseline, param] <- baseline_input[param]
  }

  # join output values
  # assumes unique
  SUBGRID <- merge(SUBGRID, psa_dat, by = param_names) %>%
    arrange(names)

  class(SUBGRID) <- c("tornado", class(SUBGRID))

  attr(SUBGRID, "output_name") <-  as.character(output_name)
  attr(SUBGRID, "baseline") <- baseline_input

  return(SUBGRID)
}
