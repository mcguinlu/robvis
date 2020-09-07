#' List tool templates available within the package.
#' @description rob_tools() will list the risk-of-bias assessment tools for
#'   which templates already exist within the robvis package. If the assessment
#'   tool you used does not appear in the list, use the "Generic" template.
#' @examples
#' rob_tools()
#' @family helper
#' @export

rob_tools <- function() {
  tools <- c("ROB2",
    "ROB2-Cluster",
    "ROBINS-I",
    "QUADAS-2",
    "Generic",
    "ROBINS-I ONLINE")
  message(
    paste0("Note: the \"ROB2-Cluster\" template is only available ",
           "for the rob_traffic_light() function.")
  )
  return(tools)
}
