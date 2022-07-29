
#' my_contour2_facet
#'
#' @param dat List of class dectree output
#' @param ...
#'
#' @rdname ce_planes
#' @return plot
#' @export
#'
my_contour2_facet <- function(dat, ...) {

  require(gridExtra)
  require(grid)

  xx <- map2(.x = map(dat, 'e'),
             .y = map(map(dat, 'c'), function(x) -x),
             .f = bcea)

  ggout <- lapply(xx, my_contour2, graph = "ggplot2", title = "", ...)

  marrangeGrob(grobs = ggout,
               nrow = length(dat),
               ncol = 1,
               top = "")
}
