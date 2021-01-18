#' List the risk-of-bias tools for which templates are available within the package.
#' @description rob_tools() will list the risk-of-bias assessment tools for
#'   which templates already exist within the robvis package. If the assessment
#'   tool you used does not appear in the list, use the "Generic" template.
#'
#' @param forest Show the tools supported by the forest/blobbogram functions
#'
#' @examples
#' rob_tools()
#' @family helper
#' @export

rob_tools <- function(forest = FALSE) {

  if (forest) {
    tools <- c("ROB2",
               "ROBINS-I")
  } else {
    tools <- c("ROB2",
               "ROB2-Cluster",
               "ROBINS-I",
               "QUADAS-2",
               "QUIPS",
               "Generic"
    )
      message(
    paste0("Note: the \"ROB2-Cluster\" template is only available ",
           "for the rob_traffic_light() function.")
  )
  }


  return(tools)
}

#' Extract weights from metafor results object and append to risk-of-bias data.
#'
#' @description Used to prepare a risk-of-bias dataset to be passed to the
#'   weighted barplot function: rob_summary(..., weighted = TRUE)
#'
#' @param data Risk of bias dataset (without a weight column)
#' @param res metafor results object
#'
#' @family helper
#'
#' @export
#'
#' @examples
#' \donttest{
#' dat.bcg <- metafor::dat.bcg[c(1:9),]
#'
#' dat <-
#'   metafor::escalc(
#'     measure = "RR",
#'     ai = tpos,
#'     bi = tneg,
#'     ci = cpos,
#'     di = cneg,
#'     data = dat.bcg,
#'     slab = paste(author, year)
#'   )
#'
#' res <- metafor::rma(yi, vi, data = dat)
#'
#' data_rob2$Study <- paste(dat$author,dat$year)
#'
#' rob_weighted_data <- rob_append_weights(data_rob2[,1:7], res)
#'
#' rob_summary(rob_weighted_data, tool = "ROB2", weighted = TRUE)
#' }

rob_append_weights <- function(data, res){

  if (!("rma" %in% class(res))) {
    stop("Result objects need to be of class \"meta\" - output from metafor package functions")
  }

  # Extract weights
  weights <- data.frame(Study = names(stats::weights(res)),
                        Weight = stats::weights(res),
                        row.names = NULL)

  # Merge by Study name to create new dataframe
  rob_df <- dplyr::left_join(data, weights, by = "Study")

  # Employ check to see if data has merged properly If a merge has failed, one
  # of the Weight cells will be NA, meaning the sum will also be NA
  if (is.na(sum(rob_df$Weight))) {
    stop(paste0("Problem with matching - weights do not equal 100. ",
                "Check that the names of studies are the same in the ROB ",
                "data and the res object (stored in slab)"))
  }

  return(rob_df)
}

