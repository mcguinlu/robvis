#' Create "realistic" dummy risk of bias assessment data
#'
#' @param n Number of assessments to create
#' @param tool Tool used for assessment (see rob_tools())
#'
#' @return
#' @export
#'
#' @examples
rob_dummy <- function(n, tool){

  if (tool == "ROB2") {
    data.frame(

    ) %>%

      robvis::data_rob2 %>%
      mutate(overall = case_when(any(dplyr::select(.,
        dplyr::matches("^D.$")
      ) == "High") ~ "banana",
      T ~ overall))
  }

}




