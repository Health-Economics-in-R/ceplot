
#' Plot a Cost-Effectiveness Acceptability Curve
#'
#' The function that comes with the \code{BCEA} package
#' is a bit basic and hard to modify.
#'
#' TODO: ggplot version
#'
#' @param screen.bcea A BCEA object from the BCEA package
#' @param new_window default: FALSE
#' @param scenario Specific scenario numbers to plot. default: NA
#'
#' @return
#' @export
#'
#' @examples
#'
my_ceac.plot <- function(screen.bcea,
                         new_window = FALSE,
                         scenario = NA) {

  require(RColorBrewer)

  if (is.na(scenario)) {
    NUM_SCENARIOS <- ncol(screen.bcea$ceac)
    scenario <- seq_len(NUM_SCENARIOS)
  }

  rainbow_cols <- colorRampPalette(c('red','blue','green'))(NUM_SCENARIOS)

  if (new_window) windows(width = 100, height = 50)

  plot(x = seq(0, 50000, by = 100),
       ylim = c(0, 1), xlim = c(10000, 30000),
       type = 'l',
       xlab = "Willingness to pay (£)",
       ylab = "Probability cost-effective")

  for (i in scenario) {

    lines(x = seq(0, 50000, by = 100), y = screen.bcea$ceac[ ,i],
          col = rainbow_cols[i], lty = i)
  }

  legend('topleft',
         legend = 1:NUM_SCENARIOS,
         col = rainbow_cols,
         lty = 1:NUM_SCENARIOS)
  abline(v = 20000, col = "grey")
}
