#' Save risk-of-bias plots to a file using sensible parameters
#'
#' @param rob_object Object created using either rob_summary() or
#'   rob_traffic_light()
#' @param file Destination file, with extension (e.g. "rob_figure.png")
#' @param height Height of resulting image, in inches. Defaults to "default"
#'   which uses recommended values based on the number of studies included.
#' @param width Width of resulting image, in inches. Defaults to "default" which
#'   uses recommended values based on the number of characters in the Study and
#'   Domain names. dpi
#' @param dpi Plot resolution.
#'
#' @family helper
#' @export

rob_save <- function(rob_object,
                     file = "rob_figure.png",
                     height = "default",
                     width = "default",
                     dpi = 800) {

  check_extension(file)

  width <- ifelse(width == "default", rob_object$rec_width, width)
  height <- ifelse(height == "default", rob_object$rec_height, height)

  ggplot2::ggsave(
    file,
    plot = rob_object,
    width = width,
    height = height,
    units = "in",
    dpi = dpi,
    limitsize = FALSE
  )
}
