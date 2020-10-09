#' Produce traffic-light plots of risk-of-bias assessments.
#' @description A function to take a summary table of risk of bias assessments and produce a traffic light plot from it.
#'
#' @param data A dataframe containing summary (domain) level risk-of-bias assessments, with the first column containing the study details, the second column containing the first domain of your assessments, and the final column containing a weight to assign to each study. The function assumes that the data includes a column for overall risk-of-bias. For example, a ROB2.0 dataset would have 8 columns (1 for study details, 5 for domain level judgments, 1 for overall judgements, and 1 for weights, in that order).
#' @param tool The risk of bias assessment tool used. RoB2.0 (tool='ROB2'), ROBINS-I (tool='ROBINS-I'), and QUADAS-2 (tool='QUADAS-2') are currently supported.
#' @param colour An argument to specify the colour scheme for the plot. Default is 'cochrane' which used the ubiquitous Cochrane colours, while a preset option for a colour-blind friendly palette is also available (colour = 'colourblind').
#' @param psize Control the size of the traffic lights. Default is 20.
#' @param ... Arguments to be passed to the tool specific functions.
#'
#' @return Risk-of-bias assessment traffic light plot (ggplot2 object)
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
#' rob_traffic_light(data, "ROB2")
#' @export

rob_traffic_light <- function(data, tool, colour = "cochrane", psize = 20, ...) {

  # Allow for depreciated "ROB1" argument
  tools <- c(rob_tools(), "ROB1")

  if ((tool %in% tools) == FALSE) {
    stop(
      paste(
        "\nTool name \"",
        tool,
        "\" not recognised \nAcceptable tools names can be found using the rob_tools() function"
      )
    )
  }


  # Define colours
  rob_colours <- get_colour(tool = tool, colour = colour)


  if (tool == "ROB2") {
    plot <- rob_traffic_light_rob2(
      data = data,
      tool = tool,
      rob_colours = rob_colours,
      psize = psize
    )
  }

  if (tool == "ROB2-Cluster") {
    plot <- rob_traffic_light_rob2_cluster(
      data = data,
      tool = tool,
      rob_colours = rob_colours,
      psize = psize
    )
  }

  if (tool == "ROBINS-I") {
    plot <- rob_traffic_light_robinsi(
      data = data,
      tool = tool,
      rob_colours = rob_colours,
      psize = psize
    )
  }

  if (tool == "ROBINS-I ONLINE") {
    plot <- rob_traffic_light_robinsi_online(
      data = data,
      tool = tool,
      rob_colours = rob_colours,
      psize = psize
    )
  }

  if (tool == "QUADAS-2") {
    plot <- rob_traffic_light_quadas2(
      data = data,
      tool = tool,
      rob_colours = rob_colours,
      psize = psize
    )
  }

  if (tool == "Generic") {
    plot <- rob_traffic_light_generic(
      data = data,
      tool = tool,
      rob_colours = rob_colours,
      psize = psize,
      ...
    )
  }

  return(plot)
}

# ROB-2=========================================================================



rob_traffic_light_rob2 <- function(data,
                                   tool,
                                   rob_colours,
                                   psize) {
  if (NCOL(data) < 7) {
    stop("Column missing (number of columns < 7).")
  }

  data.tmp <- cbind(data[, 1], data.frame(lapply(data[, 2:7], clean_data), stringsAsFactors = F))

  rob2_name <- c("Study", "D1", "D2", "D3", "D4", "D5", "Overall")
  names(data.tmp) <- rob2_name

  rob.tidy <- suppressWarnings(tidyr::gather(
    data.tmp,
    domain, judgement, -Study
  ))

  ssize <- psize - (psize / 4)

  rob.tidy$Study <- factor(rob.tidy$Study, levels = unique(data.tmp$Study))

  rob.tidy$judgement <- as.factor(rob.tidy$judgement)

  rob.tidy$judgement <- factor(rob.tidy$judgement, levels = c("h", "s", "l", "n", "x"))

  adjust_caption <- -0.7 + length(unique(rob.tidy$judgement)) * -0.6

  trafficlightplot <- ggplot2::ggplot(rob.tidy, ggplot2::aes(
    x = 1,
    y = 1, colour = judgement
  )) +
    ggplot2::facet_grid(Study ~
    factor(domain, levels = rob2_name), switch = "y", space = "free") +
    ggplot2::geom_point(size = 6) +
    ggplot2::geom_point(
      size = 4,
      colour = "black", ggplot2::aes(shape = judgement)
    ) +
    ggplot2::geom_rect(
      data = rob.tidy[which(rob.tidy$domain !=
        "Overall"), ], fill = "#ffffff", xmin = -Inf,
      xmax = Inf, ymin = -Inf, ymax = Inf, show.legend = FALSE
    ) +
    ggplot2::geom_rect(
      data = rob.tidy[which(rob.tidy$domain ==
        "Overall"), ], fill = "#d3d3d3", xmin = -Inf,
      xmax = Inf, ymin = -Inf, ymax = Inf, show.legend = FALSE
    ) +
    ggplot2::geom_point(size = psize, show.legend = FALSE) +
    ggplot2::geom_point(
      data = rob.tidy[which(rob.tidy$judgement !=
        "x"), ], shape = 1, colour = "black",
      size = psize, show.legend = FALSE
    ) +
    ggplot2::geom_point(
      size = ssize,
      colour = "black", ggplot2::aes(shape = judgement),
      show.legend = FALSE
    ) +
    ggplot2::labs(caption = "  Domains:
  D1: Bias arising from the randomization process.
  D2: Bias due to deviations from intended intervention.
  D3: Bias due to missing outcome data.
  D4: Bias in measurement of the outcome.
  D5: Bias in selection of the reported result.


                ") +
    ggplot2::scale_x_discrete(position = "top", name = "Risk of bias domains") +
    ggplot2::scale_y_continuous(
      limits = c(1, 1), labels = NULL,
      breaks = NULL, name = "Study", position = "left"
    ) +
    ggplot2::scale_colour_manual(values = c(
      h = rob_colours$high_colour,
      s = rob_colours$concerns_colour, l = rob_colours$low_colour, n = rob_colours$ni_colour, x = rob_colours$na_colour
    ), labels = c(
      h = "High",
      s = "Some concerns", l = "Low", n = "No information", x = "Not applicable"
    )) +
    ggplot2::scale_shape_manual(values = c(
      h = 120,
      s = 45, l = 43, n = 63, x = 32
    ), labels = c(
      h = "High", s = "Some concerns",
      l = "Low", n = "No information", x = "Not applicable"
    )) +
    ggplot2::scale_size(range = c(
      5,
      20
    )) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.border = ggplot2::element_rect(colour = "grey"),
      panel.spacing = ggplot2::unit(0, "line"), legend.position = "bottom",
      legend.justification = "right", legend.direction = "vertical",
      legend.margin = ggplot2::margin(
        t = -0.2, r = 0,
        b = adjust_caption, l = -10, unit = "cm"
      ),
      strip.text.x = ggplot2::element_text(size = 10),
      strip.text.y.left = ggplot2::element_text(
        angle = 0,
        size = 10
      ), legend.text = ggplot2::element_text(size = 9),
      legend.title = ggplot2::element_text(size = 9),
      strip.background = ggplot2::element_rect(fill = "#a9a9a9"),
      plot.caption = ggplot2::element_text(
        size = 10,
        hjust = 0, vjust = 1
      )
    ) +
    ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(fill = NA))) +
    ggplot2::labs(shape = "Judgement", colour = "Judgement") # Need to be exactly the same


  return(trafficlightplot)
}

# ROB-2 Cluster=================================================================

rob_traffic_light_rob2_cluster <- function(data,
                                           tool,
                                           rob_colours,
                                           psize) {
  if (NCOL(data) < 8) {
    stop("Column missing (number of columns < 8).")
  }

  data.tmp <- cbind(data[, 1], data.frame(lapply(data[, 2:8], clean_data), stringsAsFactors = F))

  rob2cluster_name <- c("Study", "D1", "D1b", "D2", "D3", "D4", "D5", "Overall")

  names(data.tmp) <- rob2cluster_name

  rob.tidy <- suppressWarnings(tidyr::gather(
    data.tmp,
    domain, judgement, -Study
  ))

  ssize <- psize - (psize / 4)

  rob.tidy$Study <- factor(rob.tidy$Study, levels = unique(data.tmp$Study))

  rob.tidy$judgement <- as.factor(rob.tidy$judgement)

  rob.tidy$judgement <- factor(rob.tidy$judgement, levels = c("h", "s", "l", "n", "x"))

  adjust_caption <- -0.7 + length(unique(rob.tidy$judgement)) * -0.6

  trafficlightplot <- ggplot2::ggplot(rob.tidy, ggplot2::aes(
    x = 1,
    y = 1, colour = judgement
  )) +
    ggplot2::facet_grid(Study ~
    factor(domain, levels = rob2cluster_name),
    switch = "y",
    space = "free"
    ) +
    ggplot2::geom_point(size = 6) +
    ggplot2::geom_point(
      size = 4,
      colour = "black", ggplot2::aes(shape = judgement)
    ) +
    ggplot2::geom_rect(
      data = rob.tidy[which(rob.tidy$domain !=
        "Overall"), ], fill = "#ffffff", xmin = -Inf,
      xmax = Inf, ymin = -Inf, ymax = Inf, show.legend = FALSE
    ) +
    ggplot2::geom_rect(
      data = rob.tidy[which(rob.tidy$domain ==
        "Overall"), ], fill = "#d3d3d3", xmin = -Inf,
      xmax = Inf, ymin = -Inf, ymax = Inf, show.legend = FALSE
    ) +
    ggplot2::geom_point(size = psize, show.legend = FALSE) +
    ggplot2::geom_point(
      data = rob.tidy[which(rob.tidy$judgement !=
        "x"), ], shape = 1, colour = "black",
      size = psize, show.legend = FALSE
    ) +
    ggplot2::geom_point(
      size = ssize,
      colour = "black", ggplot2::aes(shape = judgement),
      show.legend = FALSE
    ) +
    ggplot2::labs(caption = "  Domains:
  D1 :  Bias arising from the randomization process.
  D1b: Bias arising from the timing of identification
          and recruitment of Individual participants in
          relation to timing of randomization.
  D2 :  Bias due to deviations from intended intervention.
  D3 :  Bias due to missing outcome data.
  D4 :  Bias in measurement of the outcome.
  D5 :  Bias in selection of the reported result.
                ") +
    ggplot2::scale_x_discrete(position = "top", name = "Risk of bias domains") +
    ggplot2::scale_y_continuous(
      limits = c(1, 1), labels = NULL,
      breaks = NULL, name = "Study", position = "left"
    ) +
    ggplot2::scale_colour_manual(values = c(
      h = rob_colours$high_colour,
      s = rob_colours$concerns_colour, l = rob_colours$low_colour, n = rob_colours$ni_colour, x = rob_colours$na_colour
    ), labels = c(
      h = "High",
      s = "Some concerns", l = "Low", n = "No information", x = "Not applicable"
    )) +
    ggplot2::scale_shape_manual(values = c(
      h = 120,
      s = 45, l = 43, n = 63, x = 32
    ), labels = c(
      h = "High", s = "Some concerns",
      l = "Low", n = "No information", x = "Not applicable"
    )) +
    ggplot2::scale_size(range = c(
      5,
      20
    )) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.border = ggplot2::element_rect(colour = "grey"),
      panel.spacing = ggplot2::unit(0, "line"), legend.position = "bottom",
      legend.justification = "right", legend.direction = "vertical",
      legend.margin = ggplot2::margin(
        t = -0.2, r = 0,
        b = adjust_caption, l = -10, unit = "cm"
      ),
      strip.text.x = ggplot2::element_text(size = 10),
      strip.text.y.left = ggplot2::element_text(
        angle = 0,
        size = 10
      ), legend.text = ggplot2::element_text(size = 9),
      legend.title = ggplot2::element_text(size = 9),
      strip.background = ggplot2::element_rect(fill = "#a9a9a9"),
      plot.caption = ggplot2::element_text(
        size = 10,
        hjust = 0, vjust = 1
      )
    ) +
    ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(fill = NA))) +
    ggplot2::labs(shape = "Judgement", colour = "Judgement") # Need to be exactly the same

  return(trafficlightplot)
}



# ROBINS-I======================================================================

rob_traffic_light_robinsi <- function(data,
                                      tool,
                                      rob_colours,
                                      psize) {
  if (NCOL(data) < 9) {
    stop("Column missing (number of columns < 9).")
  }

  data.tmp <- cbind(data[, 1], data.frame(lapply(data[, 2:9], clean_data), stringsAsFactors = F))

  rob2robinsi_name <- c("Study", "D1", "D2", "D3", "D4", "D5", "D6", "D7", "Overall")

  names(data.tmp) <- rob2robinsi_name

  rob.tidy <- suppressWarnings(tidyr::gather(
    data.tmp,
    domain, judgement, -Study
  ))

  ssize <- psize - (psize / 4)

  rob.tidy$Study <- factor(rob.tidy$Study, levels = unique(data.tmp$Study))

  rob.tidy$judgement <- as.factor(rob.tidy$judgement)

  rob.tidy$judgement <- factor(rob.tidy$judgement, levels = c("c", "s", "m", "l", "n", "x"))

  adjust_caption <- -0.7 + (length(unique(rob.tidy$judgement)) * -0.6)

  trafficlightplot <- ggplot2::ggplot(rob.tidy, ggplot2::aes(
    x = 1,
    y = 1, colour = judgement
  )) +
    ggplot2::facet_grid(Study ~
    factor(domain, levels = rob2robinsi_name),
    switch = "y",
    space = "free"
    ) +
    ggplot2::geom_point(size = 6) +
    ggplot2::geom_point(
      size = 4, colour = "black",
      ggplot2::aes(shape = judgement)
    ) +
    ggplot2::geom_rect(
      data = rob.tidy[which(rob.tidy$domain !=
        "Overall"), ], fill = "#ffffff", xmin = -Inf, xmax = Inf,
      ymin = -Inf, ymax = Inf, show.legend = FALSE
    ) +
    ggplot2::geom_rect(
      data = rob.tidy[which(rob.tidy$domain ==
        "Overall"), ], fill = "#d3d3d3", xmin = -Inf,
      xmax = Inf, ymin = -Inf, ymax = Inf, show.legend = FALSE
    ) +
    ggplot2::geom_point(size = psize, show.legend = FALSE) +
    ggplot2::geom_point(
      shape = 1, colour = "black",
      size = psize, show.legend = FALSE
    ) +
    ggplot2::geom_point(
      size = ssize,
      colour = "black", ggplot2::aes(shape = judgement),
      show.legend = FALSE
    ) +
    ggplot2::labs(caption = "  Domains:
  D1: Bias due to confounding.
  D2: Bias due to selection of participants.
  D3: Bias in classification of interventions.
  D4: Bias due to deviations from intended interventions.
  D5: Bias due to missing data.
  D6: Bias in measurement of outcomes.
  D7: Bias in selection of the reported result.


                  ") +
    ggplot2::scale_x_discrete(position = "top", name = "Risk of bias domains") +
    ggplot2::scale_y_continuous(
      limits = c(1, 1), labels = NULL,
      breaks = NULL, name = "Study", position = "left"
    ) +
    ggplot2::scale_colour_manual(
      values = c(
        c = rob_colours$critical_colour,
        s = rob_colours$high_colour, m = rob_colours$concerns_colour, l = rob_colours$low_colour, n = rob_colours$ni_colour, x = rob_colours$na_colour
      ),
      labels = c(
        c = "Critical", s = "Serious", m = "Moderate",
        l = "Low", n = "No information", x = "Not applicable"
      )
    ) +
    ggplot2::scale_shape_manual(values = c(
      c = 33,
      s = 120, m = 45, l = 43, n = 63, x = 32
    ), labels = c(
      c = "Critical",
      s = "Serious", m = "Moderate", l = "Low", n = "No information", x = "Not applicable"
    )) +
    ggplot2::scale_size(range = c(
      5,
      20
    )) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.border = ggplot2::element_rect(colour = "grey"),
      panel.spacing = ggplot2::unit(0, "line"), legend.position = "bottom",
      legend.justification = "right", legend.direction = "vertical",
      legend.margin = ggplot2::margin(
        t = -0.2, r = 0,
        b = adjust_caption, l = -10, unit = "cm"
      ),
      strip.text.x = ggplot2::element_text(size = 10),
      strip.text.y.left = ggplot2::element_text(
        angle = 0,
        size = 10
      ), legend.text = ggplot2::element_text(size = 9),
      legend.title = ggplot2::element_text(size = 9),
      strip.background = ggplot2::element_rect(fill = "#a9a9a9"),
      plot.caption = ggplot2::element_text(
        size = 10,
        hjust = 0, vjust = 1
      )
    ) +
    ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(fill = NA))) +
    ggplot2::labs(shape = "Judgement", colour = "Judgement") # Need to be exactly the same

  return(trafficlightplot)
}


# ROBINS-I-ONLINE===============================================================

rob_traffic_light_robinsi_online <- function(data,
                                             tool,
                                             rob_colours,
                                             psize) {
  data <- data[, grepl("studyId|RBJ_answer", names(data))]
  data <- data[, colSums(is.na(data)) != nrow(data)]

  data.tmp <- cbind(data[, 1], data.frame(lapply(data[, 2:9], clean_data), stringsAsFactors = F))

  rob2robinsionline_name <- c("Study", "D1", "D2", "D3", "D4", "D5", "D6", "D7", "Overall")

  names(data.tmp) <- rob2robinsionline_name

  if (NCOL(data.tmp) < 9) {
    stop("Column missing (number of columns < 9).")
  }

  rob.tidy <- suppressWarnings(tidyr::gather(
    data.tmp,
    domain, judgement, -Study
  ))

  ssize <- psize - (psize / 4)

  rob.tidy$Study <- factor(rob.tidy$Study, levels = unique(data.tmp$Study))

  rob.tidy$judgement <- as.factor(rob.tidy$judgement)

  rob.tidy$judgement <- factor(rob.tidy$judgement, levels = c("c", "s", "m", "l", "n", "x"))

  adjust_caption <- -0.7 + length(unique(rob.tidy$judgement)) * -0.6

  trafficlightplot <- ggplot2::ggplot(rob.tidy, ggplot2::aes(
    x = 1,
    y = 1, colour = judgement
  )) +
    ggplot2::facet_grid(Study ~
    factor(domain, levels = rob2robinsionline_name),
    switch = "y",
    space = "free"
    ) +
    ggplot2::geom_point(size = 6) +
    ggplot2::geom_point(
      size = 4, colour = "black",
      ggplot2::aes(shape = judgement)
    ) +
    ggplot2::geom_rect(
      data = rob.tidy[which(rob.tidy$domain !=
        "Overall"), ], fill = "#ffffff", xmin = -Inf, xmax = Inf,
      ymin = -Inf, ymax = Inf, show.legend = FALSE
    ) +
    ggplot2::geom_rect(
      data = rob.tidy[which(rob.tidy$domain ==
        "Overall"), ], fill = "#d3d3d3", xmin = -Inf,
      xmax = Inf, ymin = -Inf, ymax = Inf, show.legend = FALSE
    ) +
    ggplot2::geom_point(size = psize, show.legend = FALSE) +
    ggplot2::geom_point(
      shape = 1, colour = "black",
      size = psize, show.legend = FALSE
    ) +
    ggplot2::geom_point(
      size = ssize,
      colour = "black", ggplot2::aes(shape = judgement),
      show.legend = FALSE
    ) +
    ggplot2::labs(caption = "  Domains:
  D1: Bias due to confounding.
  D2: Bias due to selection of participants.
  D3: Bias in classification of interventions.
  D4: Bias due to deviations from intended interventions.
  D5: Bias due to missing data.
  D6: Bias in measurement of outcomes.
  D7: Bias in selection of the reported result.


                  ") +
    ggplot2::scale_x_discrete(position = "top", name = "Risk of bias domains") +
    ggplot2::scale_y_continuous(
      limits = c(1, 1), labels = NULL,
      breaks = NULL, name = "Study", position = "left"
    ) +
    ggplot2::scale_colour_manual(
      values = c(
        c = rob_colours$critical_colour,
        s = rob_colours$high_colour, m = rob_colours$concerns_colour, l = rob_colours$low_colour, x = rob_colours$na_colour
      ),
      labels = c(
        c = "Critical", s = "Serious", m = "Moderate",
        l = "Low", x = "Not applicable"
      )
    ) +
    ggplot2::scale_shape_manual(values = c(
      c = 33,
      s = 120, m = 45, l = 43, x = 32
    ), labels = c(
      c = "Critical",
      s = "Serious", m = "Moderate", l = "Low", x = "Not applicable"
    )) +
    ggplot2::scale_size(range = c(
      5,
      20
    )) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.border = ggplot2::element_rect(colour = "grey"),
      panel.spacing = ggplot2::unit(0, "line"), legend.position = "bottom",
      legend.justification = "right", legend.direction = "vertical",
      legend.margin = ggplot2::margin(
        t = -0.2, r = 0,
        b = adjust_caption, l = -10, unit = "cm"
      ),
      strip.text.x = ggplot2::element_text(size = 10),
      strip.text.y.left = ggplot2::element_text(
        angle = 0,
        size = 10
      ), legend.text = ggplot2::element_text(size = 9),
      legend.title = ggplot2::element_text(size = 9),
      strip.background = ggplot2::element_rect(fill = "#a9a9a9"),
      plot.caption = ggplot2::element_text(
        size = 10,
        hjust = 0, vjust = 1
      )
    ) +
    ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(fill = NA))) +
    ggplot2::labs(shape = "Judgement", colour = "Judgement") # Need to be exactly the same

  return(trafficlightplot)
}

# QUADAS-2======================================================================


rob_traffic_light_quadas2 <- function(data,
                                      tool,
                                      rob_colours,
                                      psize) {
  if (NCOL(data) < 6) {
    stop("Column missing (number of columns < 6).")
  }

  data.tmp <- cbind(data[, 1], data.frame(lapply(data[, 2:6], clean_data), stringsAsFactors = F))

  rob2quandas2_name <- c("Study", "D1", "D2", "D3", "D4", "Overall")

  names(data.tmp) <- rob2quandas2_name

  rob.tidy <- suppressWarnings(tidyr::gather(
    data.tmp,
    domain, judgement, -Study
  ))

  ssize <- psize - (psize / 4)

  rob.tidy$Study <- factor(rob.tidy$Study, levels = unique(data.tmp$Study))

  rob.tidy$judgement <- as.factor(rob.tidy$judgement)

  rob.tidy$judgement <- factor(rob.tidy$judgement, levels = c("h", "s", "l", "n", "x"))

  adjust_caption <- -0.7 + length(unique(rob.tidy$judgement)) * -0.6

  trafficlightplot <- ggplot2::ggplot(rob.tidy, ggplot2::aes(
    x = 1,
    y = 1, colour = judgement
  )) +
    ggplot2::facet_grid(Study ~
    factor(domain, levels = rob2quandas2_name), switch = "y", space = "free") +
    ggplot2::geom_point(size = 6) +
    ggplot2::geom_point(
      size = 4,
      colour = "black", ggplot2::aes(shape = judgement)
    ) +
    ggplot2::geom_rect(
      data = rob.tidy[which(rob.tidy$domain !=
        "Overall"), ], fill = "#ffffff", xmin = -Inf,
      xmax = Inf, ymin = -Inf, ymax = Inf, show.legend = FALSE
    ) +
    ggplot2::geom_rect(
      data = rob.tidy[which(rob.tidy$domain ==
        "Overall"), ], fill = "#d3d3d3", xmin = -Inf,
      xmax = Inf, ymin = -Inf, ymax = Inf, show.legend = FALSE
    ) +
    ggplot2::geom_point(size = psize, show.legend = FALSE) +
    ggplot2::geom_point(
      shape = 1, colour = "black",
      size = psize, show.legend = FALSE
    ) +
    ggplot2::geom_point(
      size = ssize,
      colour = "black", ggplot2::aes(shape = judgement),
      show.legend = FALSE
    ) +
    ggplot2::labs(caption = "  Domains:
  D1: Patient selection.
  D2: Index test.
  D3: Reference standard.
  D4: Flow & timing.


                  ") +
    ggplot2::scale_x_discrete(position = "top", name = "Risk of bias domains") +
    ggplot2::scale_y_continuous(
      limits = c(1, 1), labels = NULL,
      breaks = NULL, name = "Study", position = "left"
    ) +
    ggplot2::scale_colour_manual(values = c(
      h = rob_colours$high_colour,
      s = rob_colours$concerns_colour, l = rob_colours$low_colour, n = rob_colours$ni_colour, x = rob_colours$na_colour
    ), labels = c(
      h = "High",
      s = "Some concerns", l = "Low", n = "No information", x = "Not applicable"
    )) +
    ggplot2::scale_shape_manual(values = c(
      h = 120,
      s = 45, l = 43, n = 63, x = 32
    ), labels = c(
      h = "High", s = "Some concerns",
      l = "Low", n = "No information", x = "Not applicable"
    )) +
    ggplot2::scale_size(range = c(
      5,
      20
    )) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.border = ggplot2::element_rect(colour = "grey"),
      panel.spacing = ggplot2::unit(0, "line"), legend.position = "bottom",
      legend.justification = "right", legend.direction = "vertical",
      legend.margin = ggplot2::margin(
        t = -0.2, r = 0,
        b = adjust_caption, l = -10, unit = "cm"
      ),
      strip.text.x = ggplot2::element_text(size = 10),
      strip.text.y.left = ggplot2::element_text(
        angle = 0,
        size = 10
      ), legend.text = ggplot2::element_text(size = 9),
      legend.title = ggplot2::element_text(size = 9),
      strip.background = ggplot2::element_rect(fill = "#a9a9a9"),
      plot.caption = ggplot2::element_text(
        size = 10,
        hjust = 0, vjust = 1
      )
    ) +
    ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(fill = NA))) +
    ggplot2::labs(shape = "Judgement", colour = "Judgement") # Need to be exactly the same

  return(trafficlightplot)
}

# ROB-1/Generic=================================================================

rob_traffic_light_generic <- function(data,
                                      tool,
                                      rob_colours,
                                      psize,
                                      x_title = "Risk of bias domains",
                                      y_title = "Study",
                                      judgement_title = "Judgement",
                                      judgement_labels = c(
                                        "Critical",
                                        "High",
                                        "Unclear",
                                        "Low",
                                        "No information",
                                        "Not applicable"
                                      )) {
  if (tool %in% c("Generic", "ROB1")) {
    if (tool == "ROB1") {
      message(
        paste0(
          "Note: In future versions of robvis, the 'tool = \"ROB1\"' ",
          "argument will be depreciated.\n",
          "Please use 'tool = \"Generic\"' instead."
        )
      )
    }

    # Determine if the uploaded dataset contains weights
    if (unique(grepl(
      "^[-]{0,1}[0-9]{0,}.{0,1}[0-9]{1,}$",
      data[[ncol(data)]]
    )) == TRUE) {
      for (i in 2:(ncol(data) - 1)) {
        data[[i]] <- clean_data(data[[i]])
        data[[i]] <- gsub("u", "s", data[[i]])
        data[[i]] <- gsub("m", "s", data[[i]])
      }

      # Select relevant columns, excluding the 'Weights' column
      data <- data[, c(1:(ncol(data) - 1))]
    } else {
      for (i in 2:(ncol(data))) {
        data[[i]] <- clean_data(data[[i]])
        data[[i]] <- gsub("u", "s", data[[i]])
        data[[i]] <- gsub("m", "s", data[[i]])
      }
    }

    data.tmp <- data

    # Remove dots from column names
    for (i in 1:(ncol(data.tmp))) {
      names(data.tmp)[i] <- invisible(gsub(".", " ",
        names(data.tmp)[i],
        fixed = TRUE
      ))
    }

    # Create caption vector, and add line breaks to maintain spacing
    captiondf <- data.frame(V1 = rep("", 8), stringsAsFactors = FALSE)
    for (i in 2:(ncol(data.tmp) - 1)) {
      if (i == 2) {
        captiondf[i - 1, 1] <- paste0(
          " D", i - 1,
          ": ", names(data.tmp)[i], "\n"
        )
      } else {
        captiondf[i - 1, 1] <- paste0(
          "D", i - 1, ": ",
          names(data.tmp)[i], "\n"
        )
      }
    }

    captiondf[captiondf == ""] <- "\n"

    caption <- paste(captiondf$V1, collapse = " ")
    # Rename columns headings
    names(data.tmp)[1] <- "Study"
    for (i in 2:(ncol(data.tmp) - 1)) {
      names(data.tmp)[i] <- paste0("D", i - 1)
    }

    # Convert to long format
    rob.tidy <- suppressWarnings(tidyr::gather(
      data.tmp,
      domain, judgement, -Study
    ))

    # Relevel rob.tidy$domain
    for (i in 1:(ncol(data.tmp))) {
      levels(rob.tidy$domain)[i] <- colnames(data.tmp)[i]
    }

    rob.tidy$domain <- factor(rob.tidy$domain, levels = levels(rob.tidy$domain))

    rob.tidy$Study <- factor(rob.tidy$Study, levels = unique(data.tmp$Study))

    rob.tidy$judgement <- as.factor(rob.tidy$judgement)
    # add judgment levels variable
    judgement_levels <- c("c", "h", "s", "l", "n", "x")

    rob.tidy$judgement <- factor(rob.tidy$judgement, levels = judgement_levels)

    adjust_caption <- -0.7 + length(unique(rob.tidy$judgement)) * -0.6

    # Set sizes
    ssize <- psize - (psize / 4)

    # name the provided judgement labels with appropriate judgement levels to enable this to be passed
    # as a named character variable to the ggplot::scale_colour_manual()
    names(judgement_labels) <- judgement_levels

    overall_name <- names(data.tmp)[length(names(data.tmp))]
    # PLot graph
    trafficlightplot <- ggplot2::ggplot(rob.tidy, ggplot2::aes(
      x = 1,
      y = 1, colour = judgement
    )) +
      ggplot2::facet_grid(Study ~
      factor(domain), switch = "y", space = "free") +
      ggplot2::geom_point(size = 6) +
      ggplot2::geom_point(
        size = 4,
        colour = "black", ggplot2::aes(shape = judgement)
      ) +
      ggplot2::geom_rect(
        data = rob.tidy[which(rob.tidy$domain !=
          overall_name), ], fill = "#ffffff", xmin = -Inf,
        xmax = Inf, ymin = -Inf, ymax = Inf, show.legend = FALSE
      ) +
      ggplot2::geom_rect(
        data = rob.tidy[which(rob.tidy$domain ==
          overall_name), ], fill = "#d3d3d3", xmin = -Inf,
        xmax = Inf, ymin = -Inf, ymax = Inf, show.legend = FALSE
      ) +
      ggplot2::geom_point(size = psize, show.legend = FALSE) +
      ggplot2::geom_point(
        shape = 1, colour = "black",
        size = psize, show.legend = FALSE
      ) +
      ggplot2::geom_point(
        size = ssize,
        colour = "black", ggplot2::aes(shape = judgement),
        show.legend = FALSE
      ) +
      ggplot2::labs(caption = caption) +
      ggplot2::scale_x_discrete(position = "top", name = x_title) +
      ggplot2::scale_y_continuous(
        limits = c(1, 1), labels = NULL,
        breaks = NULL, name = y_title, position = "left"
      ) +
      ggplot2::scale_colour_manual(
        values = c(
          l = rob_colours$low_colour,
          s = rob_colours$concerns_colour, h = rob_colours$high_colour, c = rob_colours$critical_colour, n = rob_colours$ni_colour, x = rob_colours$na_colour
        ),

        labels = judgement_labels
      ) +
      ggplot2::scale_shape_manual(values = c(
        l = 43,
        s = 45, h = 120, c = 33, n = 63, x = 32
      ), labels = judgement_labels) +
      ggplot2::scale_size(range = c(
        5,
        20
      )) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        panel.border = ggplot2::element_rect(colour = "grey"),
        panel.spacing = ggplot2::unit(0, "line"), legend.position = "bottom",
        legend.justification = "right", legend.direction = "vertical",
        legend.margin = ggplot2::margin(
          t = -0.2, r = 0,
          b = adjust_caption, l = -10, unit = "cm"
        ),
        strip.text.x = ggplot2::element_text(size = 10),
        strip.text.y.left = ggplot2::element_text(
          angle = 0,
          size = 10
        ), legend.text = ggplot2::element_text(size = 9),
        legend.title = ggplot2::element_text(size = 9),
        strip.background = ggplot2::element_rect(fill = "#a9a9a9"),
        plot.caption = ggplot2::element_text(
          size = 10,
          hjust = 0, vjust = 1
        )
      ) +
      ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(fill = NA))) +
      ggplot2::labs(shape = judgement_title, colour = judgement_title)
  }

  # Return-plot===================================================================

  return(trafficlightplot)
}
