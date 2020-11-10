#' Produce summary weighted barplots of risk-of-bias assessments.
#' @description A function to convert standard risk-of-bias output to tidy data
#'   and plot a summary barplot.
#' @param data A dataframe containing summary (domain) level risk-of-bias
#'   assessments, with the first column containing the study details, the second
#'   column containing the first domain of your assessments, and the final
#'   column containing a weight to assign to each study. The function assumes
#'   that the data includes a column for overall risk-of-bias. For example, a
#'   ROB2.0 dataset would have 8 columns (1 for study details, 5 for domain
#'   level judgments, 1 for overall judgements, and 1 for weights, in that
#'   order).
#' @param tool The risk of bias assessment tool used. RoB2.0 (tool='ROB2'),
#'   ROBINS-I (tool='ROBINS-I'), and QUADAS-2 (tool='QUADAS-2') are currently
#'   supported.
#' @param overall An option to include a bar for overall risk-of-bias in the
#'   figure. Default is FALSE.
#' @param colour An argument to specify the colour scheme for the plot. Default
#'   is 'cochrane' which used the ubiquitous Cochrane colours, while a preset
#'   option for a colour-blind friendly palette is also available (colour =
#'   'colourblind').
#' @param weighted An option to specify whether weights should be used in the
#'   barplot. Default is TRUE, in line with current Cochrane Collaboration
#'   guidance.
#' @return Risk of bias assessment barplot figure.
#' @family main
#' @examples
#'
#' data <- data.frame(
#'   stringsAsFactors = FALSE,
#'   Study = c("Study 1", "Study 2"),
#'   D1 = c("Low", "Some concerns"),
#'   D2 = c("Low", "Low"),
#'   D3 = c("Low", "Low"),
#'   D4 = c("Low", "Low"),
#'   D5 = c("Low", "Low"),
#'   Overall = c("Low", "Low"),
#'   Weight = c(33.33333333, 33.33333333)
#' )
#'
#' rob_summary(data, "ROB2")
#' @export

rob_summary <- function(data,
                        tool,
                        overall = FALSE,
                        weighted = TRUE,
                        colour = "cochrane",
                        ...) {
  check_tool(tool)
  check_data(data)
  colour <- weird_spelling(colour)

  check_colour(tool = tool, colour = colour)

  # Define colours
  rob_colours <- get_colour(tool = tool, colour = colour)

  if (tool == "ROB2") {
    plot <- rob_summary_rob2(
      data = data,
      tool = tool,
      overall = overall,
      weighted = weighted,
      rob_colours = rob_colours
    )
  }

  if (tool == "ROBINS-I") {
    plot <- rob_summary_robinsi(
      data = data,
      tool = tool,
      overall = overall,
      weighted = weighted,
      rob_colours = rob_colours
    )
  }

  if (tool == "QUADAS-2") {
    plot <- rob_summary_quadas2(
      data = data,
      tool = tool,
      overall = overall,
      weighted = weighted,
      rob_colours = rob_colours
    )
  }

  if (tool == "QUIPS") {
    plot <- rob_summary_quips(
      data = data,
      tool = tool,
      overall = overall,
      weighted = weighted,
      rob_colours = rob_colours
    )
  }

  if (tool %in% c("Generic", "ROB1")) {
    plot <- rob_summary_generic(
      data = data,
      tool = tool,
      overall = overall,
      weighted = weighted,
      rob_colours = rob_colours,
      ...
    )
  }

  plot$rec_height <- get_height(
    type = "summ"
  )

  plot$rec_width <- get_width(
    type = "summ"
  )

  plot$rob_type <- "summary"

  return(plot)
}

# ROBINS-I======================================================================

rob_summary_rob2 <- function(data,
                             tool,
                             overall,
                             weighted,
                             rob_colours) {

  domain_names <- c(
    "Study",
    "Bias arising from the randomization process",
    "Bias due to deviations from intended interventions",
    "Bias due to missing outcome data",
    "Bias in measurement of the outcome",
    "Bias in selection of the reported result",
    "Overall risk of bias",
    "Weights"
  )

  rob.tidy <- tidy_data_summ(data,
                             max_domain_column = 7,
                             overall,
                             weighted,
                             domain_names,
                             levels = c("x", "n", "h", "s", "l"))

  # Create plot
  plot <- ggplot2::ggplot(data = rob.tidy) +
    rob_summ_theme() +
    ggplot2::scale_fill_manual(
      "Risk of Bias",
      values = c(
        l = rob_colours$low_colour,
        s = rob_colours$concerns_colour,
        h = rob_colours$high_colour,
        n = rob_colours$ni_colour,
        x = rob_colours$na_colour
      ),
      labels = c(
        n = "  No information  ",
        h = "  High risk       ",
        s = "  Some concerns   ",
        l = "  Low risk        ",
        x = "  N/A  "
      )
    )

  return(plot)
}




# ROBINS-I======================================================================

rob_summary_robinsi <- function(data,
                                tool,
                                overall,
                                weighted,
                                rob_colours) {

  domain_names <- c("Study",
                    "Bias due to confounding",
                    "Bias due to selection of participants",
                    "Bias in classification of interventions",
                    "Bias due to deviations from intended interventions",
                    "Bias due to missing data",
                    "Bias in measurement of outcomes",
                    "Bias in selection of the reported result",
                    "Overall risk of bias",
                    "Weights")

  rob.tidy <- tidy_data_summ(data,
                             max_domain_column = 9,
                             overall,
                             weighted,
                             domain_names,
                             levels = c("x","n","c","s","m","l"))

  plot <-
    ggplot2::ggplot(data = rob.tidy) +
    rob_summ_theme() +
    ggplot2::scale_fill_manual(
      values = c(
        n = rob_colours$ni_colour,
        m = rob_colours$concerns_colour,
        s = rob_colours$high_colour,
        l = rob_colours$low_colour,
        c = rob_colours$critical_colour,
        x = rob_colours$na_colour
      ),
      labels = c(
        n = " No information ",
        c = " Critical risk  ",
        s = " Serious risk  ",
        m = " Moderate risk ",
        l = " Low risk  ",
        x = " N/A "
      )
    )

  return(plot)
}

# QUADAS-2======================================================================

rob_summary_quadas2 <- function(data,
                                tool,
                                overall,
                                weighted,
                                rob_colours) {

  domain_names <- c(
    "Study",
    "Patient selection",
    "Index test",
    "Reference standard",
    "Flow & timing",
    "Overall risk of bias",
    "Weights"
  )

  rob.tidy <- tidy_data_summ(data,
                             max_domain_column = 6,
                             overall,
                             weighted,
                             domain_names,
                             levels = c("x","n","h","s","l"))

  plot <-
    ggplot2::ggplot(data = rob.tidy) +
    rob_summ_theme() +
    ggplot2::scale_fill_manual(
      "Risk of Bias",
      values = c(
        n = rob_colours$ni_colour,
        h = rob_colours$high_colour,
        s = rob_colours$concerns_colour,
        l = rob_colours$low_colour,
        x = rob_colours$na_colour
      ),
      labels = c(
        n = "  No information   ",
        h = "  High risk of bias   ",
        s = "  Some concerns      ",
        l = "  Low risk of bias  ",
        x = "  N/A  "
      )
    )
}

# QUIPS ======================================================================

rob_summary_quips <- function(data,
                                tool,
                                overall,
                                weighted,
                                rob_colours) {

  domain_names <- c(
    "Study",
    "Bias due to participation",
    "Bias due to attrition",
    "Bias due to prognostic factor measurement",
    "Bias due to outcome measurement",
    "Bias due to confounding",
    "Bias in statistical analysis and reporting",
    "Overall",
    "Weights"
  )

  rob.tidy <- tidy_data_summ(data,
                             max_domain_column = 8,
                             overall,
                             weighted,
                             domain_names,
                             levels = c("x","n", "h", "m", "l"))

  plot <-
    ggplot2::ggplot(data = rob.tidy) +
    rob_summ_theme() +
    ggplot2::scale_fill_manual(
      "Risk of Bias",
      values = c(
        n = rob_colours$ni_colour,
        h = rob_colours$high_colour,
        m = rob_colours$concerns_colour,
        l = rob_colours$low_colour,
        x = rob_colours$na_colour
      ),
      labels = c(
        n = "  No information   ",
        h = "  High risk of bias   ",
        m = "  Moderate risk of bias   ",
        l = "  Low risk of bias  ",
        x = "  N/A  "
      )
    )
}


# Generic=================================================================

rob_summary_generic <- function(data,
                                tool,
                                overall,
                                weighted,
                                rob_colours,
                                judgement_labels = c("Low risk of bias",
                                                     "Some concerns",
                                                     "High risk of bias",
                                                     "No information")) {
  rob1_warning(tool)

  # Data preprocessing
  for (i in 2:(ncol(data) - 1)) {
    data[[i]] <- tolower(data[[i]])
    data[[i]] <- trimws(data[[i]])
    data[[i]] <- substr(data[[i]], 0, 1)
    data[[i]] <- gsub("u", "s", data[[i]])
    data[[i]] <- gsub("m", "s", data[[i]])
  }

  # Define weights if FALSE and check if there is a weight
  # column if TRUE
  if (weighted == FALSE) {
    data[, ncol(data)] <- rep(1, length(nrow(data)))
  } else {
    if (is.numeric(data[2, ncol(data)]) == FALSE) {
      stop(
        "Error. The final column does not seem to contain numeric values (expected for weights)."
      )
    }
  }

  # Clean and rename column headings. as needed
  data.tmp <- data
  for (i in 2:(ncol(data) - 1)) {
    names(data.tmp)[i] <- invisible(gsub(".", " ",
      names(data.tmp)[i],
      fixed = TRUE
    ))
  }
  names(data.tmp)[ncol(data.tmp)] <- "Weights"



  # Define dataframe based on value of 'overall'


  if (overall == "FALSE") {
    data.tmp <- data.tmp[, c(
      2:(ncol(data.tmp) - 2),
      ncol(data.tmp)
    )]
  } else {
    data.tmp <- data.tmp[, c(2:ncol(data.tmp))]
  }

  # Gather data, convert to factors and set levels
  rob.tidy <- suppressWarnings(tidyr::gather(
    data.tmp,
    domain, judgement, -Weights
  ))

  rob.tidy$judgement <-
    factor(rob.tidy$judgement, levels = c(
      "n",
      "h",
      "s",
      "l"
    ))

  for (i in 1:(ncol(data.tmp) - 1)) {
    levels(rob.tidy$domain)[i] <- colnames(data.tmp)[i]
  }

  rob.tidy$domain <-
    factor(rob.tidy$domain, levels = rev(levels(rob.tidy$domain)))

  judgement_levels <- c("l", "s", "h", "n")
  names(judgement_labels) <- judgement_levels

  # Create plot
  plot <-
    ggplot2::ggplot(data = rob.tidy) +
    rob_summ_theme() +
    ggplot2::scale_fill_manual(
      "Risk of Bias",
      values = c(
        l = rob_colours$low_colour,
        s = rob_colours$concerns_colour,
        h = rob_colours$high_colour,
        n = rob_colours$ni_colour
      ),
      labels = judgement_labels
    )

  return(plot)
}
