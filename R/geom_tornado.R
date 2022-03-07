#'
#' #' @export
#' draw_key_point <- function() {
#'
#' }
#'
#' #' GeomTornado
#' #'
#' #' @import ggplot2 grid
#' #' @export
#' #'
#' GeomTornado <- ggplot2::ggproto("GeomTornado", Geom,
#'             required_aes = c("ymin", "ymax"),
#'             default_aes = ggplot2::aes(),
#'             draw_key = draw_key_point,
#'             draw_panel = function(data, panel_scales, coord) {
#'               ## Transform the data first
#'               coords <- coord$transform(data, panel_scales)
#'
#'               ## Let's print out the structure of the 'coords' object
#'               str(coords)
#'
#'               ## Construct a grid grob
#'               pointsGrob(
#'                 x = coords$x,
#'                 y = coords$y,
#'                 pch = coords$shape
#'               )
#' })
#'
#' #' geom_tornado
#' #'
#' #' @export
#' #' @import ggplot2
#' #'
#' geom_tornado <- function(mapping = NULL, data = NULL, stat = "identity",
#'                          position = "identity", na.rm = FALSE,
#'                          show.legend = NA, inherit.aes = TRUE, ...) {
#'
#'   ggplot2::geom_linerange(size = 10) +
#'     ggplot2::coord_flip() +
#'     ggplot2::xlab("") +
#'     ggplot2::geom_hline(yintercept = 0, linetype = "dotted") +
#'     ggplot2::geom_hline(yintercept = dat$baseline, linetype = "dashed") +
#'     ggplot2::theme_bw() +
#'     ggplot2::theme(axis.text = ggplot2::element_text(size = 15),
#'                    legend.title = ggplot2::element_blank()) +
#'     ggplot2::annotate("text",
#'                       x = datplot$names,
#'                       y = datplot[, output_name] + nudge,
#'                       label = barLabels)
#' }
#'
