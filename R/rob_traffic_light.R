#' Produce traffic-light plots of risk-of-bias assessments.
#' @description A function to take a summary table of risk of bias assessments
#'   and produce a traffic light plot from it.
#'
#' @param data A dataframe containing summary (domain) level risk-of-bias
#'   assessments, with the first column containing the study details, the second
#'   column containing the first domain of your assessments, and the final
#'   column containing a weight to assign to each study. The function assumes
#'   that the data includes a column for overall risk-of-bias. For example, a
#'   ROB2.0 dataset would have 7 columns (1 for study details, 5 for domain
#'   level judgments, and 1 for overall judgement, in that
#'   order). See
#' @param tool The risk of bias assessment tool used. RoB2.0 (tool='ROB2'),
#'   ROBINS-I (tool='ROBINS-I'), and QUADAS-2 (tool='QUADAS-2') are currently
#'   supported.
#' @param colour An argument to specify the colour scheme for the plot. Default
#'   is 'cochrane' which used the ubiquitous Cochrane colours, while a preset
#'   option for a colour-blind friendly palette is also available (colour =
#'   'colourblind').
#' @param psize Control the size of the traffic lights. Default is 10.
#' @param overall Logical, specifying whether to include an "Overall" risk of
#'   bias column in the resulting plot
#' @param ... Arguments to be passed to the tool specific functions.
#'
#' @return Risk-of-bias assessment traffic light plot (ggplot2 object)
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
#'   Overall = c("Low", "Low")
#' )
#'
#' rob_traffic_light(data, "ROB2")
#' @export

rob_traffic_light <-
  function(data,
           tool,
           colour = "cochrane",
           psize = 10,
           overall = TRUE,
           ...) {

    check_tool(tool)
    check_data(data)
    colour <- weird_spelling(colour)

    check_colour(tool = tool, colour = colour)

    # Define colours
    rob_colours <- get_colour(tool = tool, colour = colour)

    if (tool == "ROB2") {
      plot <- rob_traffic_light_rob2(
        data = data,
        tool = tool,
        rob_colours = rob_colours,
        psize = psize,
        overall = overall
      )
    }

    if (tool == "ROB2-Cluster") {
      plot <- rob_traffic_light_rob2_cluster(
        data = data,
        tool = tool,
        rob_colours = rob_colours,
        psize = psize,
        overall = overall
      )
    }

    if (tool == "ROBINS-I") {
      plot <- rob_traffic_light_robinsi(
        data = data,
        tool = tool,
        rob_colours = rob_colours,
        psize = psize,
        overall = overall
      )
    }

    if (tool == "ROBINS-E") {
      plot <- rob_traffic_light_robinse(
        data = data,
        tool = tool,
        rob_colours = rob_colours,
        psize = psize,
        overall = overall
      )
    }

    if (tool == "QUADAS-2") {
      plot <- rob_traffic_light_quadas2(
        data = data,
        tool = tool,
        rob_colours = rob_colours,
        psize = psize,
        overall = overall
      )
    }

    if (tool == "QUIPS") {
      plot <- rob_traffic_light_quips(
        data = data,
        tool = tool,
        rob_colours = rob_colours,
        psize = psize,
        overall = overall
      )
    }

    if (tool %in% c("Generic", "ROB1")) {
      plot <- rob_traffic_light_generic(
        data = data,
        tool = tool,
        rob_colours = rob_colours,
        psize = psize,
        overall = overall,
        ...
      )
    }

    # Add recommended saving height to the plot object
    plot$rec_height <- get_height(
      data = data,
      tool = tool,
      psize = psize,
      type = "tf"
    )

    # Add recommended saving width to the plot object
    plot$rec_width <- get_width(data = data,
                                psize = psize,
                                type = "tf")

    plot$rob_type <- "traffic"

    return(plot)
  }

# ROB-2=========================================================================

rob_traffic_light_rob2 <- function(data,
                                   tool,
                                   rob_colours,
                                   psize,
                                   overall) {

  max_domain_column <- 7
  domain_names <- c("Study", "D1", "D2", "D3", "D4", "D5", "Overall")

  rob.tidy <- tidy_data(data,
                        max_domain_column = max_domain_column,
                        domain_names = domain_names,
                        overall = overall,
                        levels = c("h", "s", "l", "n", "x"))

  ssize <- psize - (psize / 4)

  adjust_caption <- get_caption_adjustment(rob.tidy)

  trafficlightplot <-
    ggplot2::ggplot(rob.tidy,
                    ggplot2::aes(x = 1,
                                 y = 1,
                                 colour = judgement)) +
    rob_tf_theme(rob.tidy,
                 domain_names,
                 psize,
                 ssize,
                 adjust_caption,
                 overall) +
    ggplot2::labs(
      caption = "  Domains:
  D1: Bias arising from the randomization process.
  D2: Bias due to deviations from intended intervention.
  D3: Bias due to missing outcome data.
  D4: Bias in measurement of the outcome.
  D5: Bias in selection of the reported result.


                "
    ) +
    ggplot2::scale_colour_manual(
      values = c(
        h = rob_colours$high_colour,
        s = rob_colours$concerns_colour,
        l = rob_colours$low_colour,
        n = rob_colours$ni_colour,
        x = rob_colours$na_colour
      ),
      labels = c(
        h = "High",
        s = "Some concerns",
        l = "Low",
        n = "No information",
        x = "Not applicable"
      ),
      drop = TRUE,
      limits = force
    ) +
    ggplot2::scale_shape_manual(
      values = c(
        h = 120,
        s = 45,
        l = 43,
        n = 63,
        x = 32
      ),
      labels = c(
        h = "High",
        s = "Some concerns",
        l = "Low",
        n = "No information",
        x = "Not applicable"
      ),
      drop = TRUE,
      limits = force
    )

  return(trafficlightplot)
}

# ROB-2 Cluster=================================================================

rob_traffic_light_rob2_cluster <- function(data,
                                           tool,
                                           rob_colours,
                                           psize,
                                           overall) {


  max_domain_column <- 8
  domain_names <- c("Study", "D1", "D1b", "D2", "D3", "D4", "D5", "Overall")

  rob.tidy <- tidy_data(data,
                        max_domain_column = max_domain_column,
                        domain_names = domain_names,
                        overall = overall,
                        levels = c("h", "s", "l", "n", "x"))

  ssize <- psize - (psize / 4)

  adjust_caption <- get_caption_adjustment(rob.tidy)

  trafficlightplot <- ggplot2::ggplot(rob.tidy,
                                      ggplot2::aes(x = 1,
                                                   y = 1,
                                                   colour = judgement)) +
    rob_tf_theme(rob.tidy,
                 domain_names,
                 psize,
                 ssize,
                 adjust_caption,
                 overall) +
    ggplot2::labs(
      caption = "  Domains:
  D1 :  Bias arising from the randomization process.
  D1b: Bias arising from the timing of identification
          and recruitment of Individual participants in
          relation to timing of randomization.
  D2 :  Bias due to deviations from intended intervention.
  D3 :  Bias due to missing outcome data.
  D4 :  Bias in measurement of the outcome.
  D5 :  Bias in selection of the reported result.
                "
    ) +
    ggplot2::scale_colour_manual(
      values = c(
        h = rob_colours$high_colour,
        s = rob_colours$concerns_colour,
        l = rob_colours$low_colour,
        n = rob_colours$ni_colour,
        x = rob_colours$na_colour
      ),
      labels = c(
        h = "High",
        s = "Some concerns",
        l = "Low",
        n = "No information",
        x = "Not applicable"
      ),
      drop = TRUE,
      limits = force
    ) +
    ggplot2::scale_shape_manual(
      values = c(
        h = 120,
        s = 45,
        l = 43,
        n = 63,
        x = 32
      ),
      labels = c(
        h = "High",
        s = "Some concerns",
        l = "Low",
        n = "No information",
        x = "Not applicable"
      ),
      drop = TRUE,
      limits = force
    )


  return(trafficlightplot)
}



# ROBINS-I======================================================================

rob_traffic_light_robinsi <- function(data,
                                      tool,
                                      rob_colours,
                                      psize,
                                      overall) {


  max_domain_column <- 9
  domain_names <-
    c("Study", "D1", "D2", "D3", "D4", "D5", "D6", "D7", "Overall")

  rob.tidy <- tidy_data(data,
                        max_domain_column = max_domain_column,
                        domain_names = domain_names,
                        overall = overall,
                        levels = c("c", "s", "m", "l", "n", "x"))

  ssize <- psize - (psize / 4)

  adjust_caption <- get_caption_adjustment(rob.tidy)

  trafficlightplot <- ggplot2::ggplot(rob.tidy,
                                      ggplot2::aes(x = 1,
                                                   y = 1,
                                                   colour = judgement)) +
    rob_tf_theme(rob.tidy,
                 domain_names,
                 psize,
                 ssize,
                 adjust_caption,
                 overall) +
    ggplot2::labs(
      caption = "  Domains:
  D1: Bias due to confounding.
  D2: Bias due to selection of participants.
  D3: Bias in classification of interventions.
  D4: Bias due to deviations from intended interventions.
  D5: Bias due to missing data.
  D6: Bias in measurement of outcomes.
  D7: Bias in selection of the reported result.


                  "
    ) +
    ggplot2::scale_colour_manual(
      values = c(
        c = rob_colours$critical_colour,
        s = rob_colours$high_colour,
        m = rob_colours$concerns_colour,
        l = rob_colours$low_colour,
        n = rob_colours$ni_colour,
        x = rob_colours$na_colour
      ),
      labels = c(
        c = "Critical",
        s = "Serious",
        m = "Moderate",
        l = "Low",
        n = "No information",
        x = "Not applicable"
      ),
      drop = TRUE,
      limits = force
    ) +
    ggplot2::scale_shape_manual(
      values = c(
        c = 33,
        s = 120,
        m = 45,
        l = 43,
        n = 63,
        x = 32
      ),
      labels = c(
        c = "Critical",
        s = "Serious",
        m = "Moderate",
        l = "Low",
        n = "No information",
        x = "Not applicable"
      ),
      drop = TRUE,
      limits = force
    )

  return(trafficlightplot)
}


# ROBINS-E======================================================================

rob_traffic_light_robinse <- function(data,
                                      tool,
                                      rob_colours,
                                      psize,
                                      overall) {


  max_domain_column <- 9
  domain_names <-
    c("Study", "D1", "D2", "D3", "D4", "D5", "D6", "D7", "Overall")

  rob.tidy <- tidy_data(data,
                        max_domain_column = max_domain_column,
                        domain_names = domain_names,
                        overall = overall,
                        levels = c("v", "h", "s", "l", "n", "x"))

  ssize <- psize - (psize / 4)

  adjust_caption <- get_caption_adjustment(rob.tidy)

  trafficlightplot <- ggplot2::ggplot(rob.tidy,
                                      ggplot2::aes(x = 1,
                                                   y = 1,
                                                   colour = judgement)) +
    rob_tf_theme(rob.tidy,
                 domain_names,
                 psize,
                 ssize,
                 adjust_caption,
                 overall) +
    ggplot2::labs(
      caption = "  Domains:
  D1: Bias due to confounding.
  D2: Bias arising from measurement of the exposure.
  D3: Bias in selection of participants into the study (or into the analysis).
  D4: Bias due to post-exposure interventions.
  D5: Bias due to missing data.
  D6: Bias arising from measurement of the outcome.
  D7: Bias in selection of the reported result.


                  "
    ) +
    ggplot2::scale_colour_manual(
      values = c(
        v = rob_colours$critical_colour,
        h = rob_colours$high_colour,
        s = rob_colours$concerns_colour,
        l = rob_colours$low_colour,
        n = rob_colours$ni_colour,
        x = rob_colours$na_colour
      ),
      labels = c(
        v = "Very high",
        h = "High",
        s = "Some concerns",
        l = "Low",
        n = "No information",
        x = "Not applicable"
      ),
      drop = TRUE,
      limits = force
    ) +
    ggplot2::scale_shape_manual(
      values = c(
        v = 33,
        h = 120,
        s = 45,
        l = 43,
        n = 63,
        x = 32
      ),
      labels = c(
        v = "Very high",
        h = "High",
        s = "Some concerns",
        l = "Low",
        n = "No information",
        x = "Not applicable"
      ),
      drop = TRUE,
      limits = force
    )

  return(trafficlightplot)
}

# QUADAS-2======================================================================


rob_traffic_light_quadas2 <- function(data,
                                      tool,
                                      rob_colours,
                                      psize,
                                      overall) {

  max_domain_column <- 6
  domain_names <- c("Study", "D1", "D2", "D3", "D4", "Overall")

  rob.tidy <- tidy_data(data,
                        max_domain_column = max_domain_column,
                        domain_names = domain_names,
                        overall = overall,
                        levels = c("h", "s", "l", "n", "x"))

  ssize <- psize - (psize / 4)

  adjust_caption <- get_caption_adjustment(rob.tidy)

  trafficlightplot <- ggplot2::ggplot(rob.tidy,
                                      ggplot2::aes(x = 1,
                                                   y = 1,
                                                   colour = judgement)) +
    rob_tf_theme(rob.tidy,
                 domain_names,
                 psize,
                 ssize,
                 adjust_caption,
                 overall) +
    ggplot2::labs(
      caption = "  Domains:
  D1: Patient selection.
  D2: Index test.
  D3: Reference standard.
  D4: Flow & timing.


                  "
    ) +
    ggplot2::scale_colour_manual(
      values = c(
        h = rob_colours$high_colour,
        s = rob_colours$concerns_colour,
        l = rob_colours$low_colour,
        n = rob_colours$ni_colour,
        x = rob_colours$na_colour
      ),
      labels = c(
        h = "High",
        s = "Some concerns",
        l = "Low",
        n = "No information",
        x = "Not applicable"
      ),
      drop = TRUE,
      limits = force
    ) +
    ggplot2::scale_shape_manual(
      values = c(
        h = 120,
        s = 45,
        l = 43,
        n = 63,
        x = 32
      ),
      labels = c(
        h = "High",
        s = "Some concerns",
        l = "Low",
        n = "No information",
        x = "Not applicable"
      ),
      drop = TRUE,
      limits = force
    )

  return(trafficlightplot)
}

# QUIPS ========================================================================


rob_traffic_light_quips <- function(data,
                                      tool,
                                      rob_colours,
                                      psize,
                                    overall) {

  max_domain_column <- 8
  domain_names <- c("Study", "D1", "D2", "D3", "D4", "D5", "D6", "Overall")

  rob.tidy <- tidy_data(data,
                        max_domain_column = max_domain_column,
                        domain_names = domain_names,
                        overall = overall,
                        levels = c("h", "m", "l", "n","x"))

  ssize <- psize - (psize / 4)

  adjust_caption <- get_caption_adjustment(rob.tidy)

  trafficlightplot <- ggplot2::ggplot(rob.tidy,
                                      ggplot2::aes(x = 1,
                                                   y = 1,
                                                   colour = judgement)) +
    rob_tf_theme(rob.tidy,
                 domain_names,
                 psize,
                 ssize,
                 adjust_caption,
                 overall) +
    ggplot2::labs(
      caption = "  Domains:
  D1: Bias due to participation.
  D2: Bias due to attrition.
  D3: Bias due to prognostic factor measurement.
  D4: Bias due to outcome measurement.
  D5: Bias due to confounding.
  D6: Bias in statistical analysis and reporting.

                  "
    ) +
    ggplot2::scale_colour_manual(
      values = c(
        h = rob_colours$high_colour,
        m = rob_colours$concerns_colour,
        l = rob_colours$low_colour,
        n = rob_colours$ni_colour,
        x = rob_colours$na_colour
      ),
      labels = c(
        h = "High",
        m = "Moderate",
        l = "Low",
        n = "No information",
        x = "Not applicable"
      ),
      drop = TRUE,
      limits = force
    ) +
    ggplot2::scale_shape_manual(
      values = c(
        h = 120,
        m = 45,
        l = 43,
        n = 63,
        x = 32
      ),
      labels = c(
        h = "High",
        m = "Moderate",
        l = "Low",
        n = "No information",
        x = "Not applicable"
      ),
      drop = TRUE,
      limits = force
    )

  return(trafficlightplot)
}

# ROB-1/Generic=================================================================

rob_traffic_light_generic <- function(data,
                                      tool,
                                      rob_colours,
                                      psize,
                                      overall,
                                      x_title = "Risk of bias domains",
                                      y_title = "Study",
                                      judgement_title = "Judgement",
                                      judgement_labels = c("Critical",
                                                           "High",
                                                           "Unclear",
                                                           "Low",
                                                           "No information",
                                                           "Not applicable")) {

  rob1_warning(tool)

  # Determine if the uploaded dataset contains weights
  if (unique(grepl("^[-]{0,1}[0-9]{0,}.{0,1}[0-9]{1,}$",
                   data[[ncol(data)]])) == TRUE) {
    for (i in 2:(ncol(data) - 1)) {
      # Convert "serious" to "high" so that it maps appropriately
      data[[i]] <- gsub("\\bse", "High", stringr::str_to_lower(data[[i]]))
      data[[i]] <- clean_data(data[[i]])
      data[[i]] <- gsub("u", "s", data[[i]])
      data[[i]] <- gsub("m", "s", data[[i]])
    }

    # Select relevant columns, excluding the 'Weights' column
    data <- data[, c(1:(ncol(data) - 1))]
  } else {
    for (i in 2:(ncol(data))) {
      # Convert "serious" to "high" so that it maps appropriately
      data[[i]] <- gsub("\\bse", "High", stringr::str_to_lower(data[[i]]))
      data[[i]] <- clean_data(data[[i]])
      data[[i]] <- gsub("u", "s", data[[i]])
      data[[i]] <- gsub("m", "s", data[[i]])
    }
  }

  # Deal with scenarios that don't have overall column
  if(overall == FALSE){
    col_adjust = 0
  } else {
    col_adjust = 1
  }

  max_domain_column <- dim(data)[2] - col_adjust

  data.tmp <- data

  # Remove dots from column names
  for (i in 1:(ncol(data.tmp))) {
    names(data.tmp)[i] <- invisible(gsub(".", " ",
                                         names(data.tmp)[i],
                                         fixed = TRUE))
  }

  # Create caption vector, and add line breaks to maintain spacing
  captiondf <- data.frame(V1 = rep("", 8), stringsAsFactors = FALSE)
  for (i in 2:max_domain_column) {
    if (i == 2) {
      captiondf[i - 1, 1] <- paste0(" D", i - 1,
                                    ": ", names(data.tmp)[i], "\n")
    } else {
      captiondf[i - 1, 1] <- paste0("D", i - 1, ": ",
                                    names(data.tmp)[i], "\n")
    }
  }

  captiondf[captiondf == ""] <- "\n"

  caption <- paste(captiondf$V1, collapse = " ")
  # Rename columns headings
  names(data.tmp)[1] <- "Study"
  for (i in 2:max_domain_column) {
    names(data.tmp)[i] <- paste0("D", i - 1)
  }

  # Convert to long format
  rob.tidy <- suppressWarnings(tidyr::gather(data.tmp,
                                             domain, judgement,-Study))

  # Relevel rob.tidy$domain
  for (i in 1:(ncol(data.tmp))) {
    levels(rob.tidy$domain)[i] <- colnames(data.tmp)[i]
  }

  domain_names <- levels(rob.tidy$domain)

  # rob.tidy$domain <-
  #   factor(rob.tidy$domain, levels = levels(rob.tidy$domain))

  rob.tidy$Study <-
    factor(rob.tidy$Study, levels = unique(data.tmp$Study))

  rob.tidy$judgement <- as.factor(rob.tidy$judgement)
  # add judgment levels variable
  judgement_levels <- c("c", "h", "s", "l", "n", "x")

  rob.tidy$judgement <-
    factor(rob.tidy$judgement, levels = judgement_levels)

  adjust_caption <- get_caption_adjustment(rob.tidy)

  # Set sizes
  ssize <- psize - (psize / 4)

  # name the provided judgement labels with appropriate judgement levels to
  # enable this to be passed as a named character variable to the
  # ggplot::scale_colour_manual()
  names(judgement_labels) <- judgement_levels

  overall_name <- names(data.tmp)[length(names(data.tmp))]

  # PLot graph
  trafficlightplot <- ggplot2::ggplot(rob.tidy,
                                      ggplot2::aes(x = 1,
                                                   y = 1,
                                                   colour = judgement)) +
    rob_tf_theme(rob.tidy,
                 domain_names,
                 psize,
                 ssize,
                 adjust_caption,
                 overall,
                 judgement_title,
                 overall_name,
                 x_title,
                 y_title) +
    ggplot2::labs(caption = caption) +
    ggplot2::scale_colour_manual(
      values = c(
        l = rob_colours$low_colour,
        s = rob_colours$concerns_colour,
        h = rob_colours$high_colour,
        c = rob_colours$critical_colour,
        n = rob_colours$ni_colour,
        x = rob_colours$na_colour
      ),
      labels = judgement_labels,
      drop = TRUE,
      limits = force
    ) +
    ggplot2::scale_shape_manual(values = c(
      l = 43,
      s = 45,
      h = 120,
      c = 33,
      n = 63,
      x = 32
    ),
    labels = judgement_labels,
    drop = TRUE,
    limits = force)

  return(trafficlightplot)

}
