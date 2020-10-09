#' Produce forest plots with risk of bias results
#' @description A function to take a summary table of risk of bias assessments and produce a traffic light plot from it.
#' @param data A dataframe containing summary (domain) level risk-of-bias assessments, with the first column containing the study details, the second column containing the first domain of your assessments, and the final column containing a weight to assign to each study. The function assumes that the data includes a column for overall risk-of-bias. For example, a ROB2.0 dataset would have 8 columns (1 for study details, 5 for domain level judgements, 1 for overall judgements, and 1 for weights, in that order).
#' @export


rob_paired_plot <- function(data, tool, colour = "cochrane",
                            psize = 20, quiet = FALSE) {
  judgement <- NULL
  Study <- NULL
  domain <- NULL


  # Define colouring
  if (length(colour) > 1) {
    rob_colours$low_colour <- colour[c(1)]
    rob_colours$concerns_colour <- colour[c(2)]
    rob_colours$high_colour <- colour[c(3)]
    rob_colours$critical_colour <- colour[c(4)]
  } else {
    if (colour == "colourblind") {
      rob_colours$low_colour <- "#fef0d9"
      rob_colours$concerns_colour <- "#fdcc8a"
      rob_colours$high_colour <- "#fc8d59"
      rob_colours$critical_colour <- "#d7301f"
    }
    if (colour == "cochrane") {
      rob_colours$low_colour <- "#02C100"
      rob_colours$concerns_colour <- "#E2DF07"
      rob_colours$high_colour <- "#BF0000"
      rob_colours$critical_colour <- "#820000"
    }
  }


  myCsv <- getURL("https://docs.google.com/spreadsheet/pub?key=0AvykV4O1IaendFRSQVVVV3pYN3JZRlNKSzBud0FFa1E&output=csv")
}
