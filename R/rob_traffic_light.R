#' Produce traffic-light plots of risk-of-bias assessments.
#' @description A function to take a summary table of risk of bias assessments and produce a traffic light plot from it.
#' @param data A dataframe containing summary (domain) level risk-of-bias assessments, with the first column containing the study details, the second column containing the first domain of your assessments, and the final column containing a weight to assign to each study. The function assumes that the data includes a column for overall risk-of-bias. For example, a ROB2.0 dataset would have 8 columns (1 for study details, 5 for domain level judgements, 1 for overall judgements, and 1 for weights, in that order).
#' @param tool The risk of bias assessment tool used. RoB2.0 (tool="ROB2"), ROBINS-I (tool="ROBINS-I"), and QUADAS-2 (tool="QUADAS-2") are currently supported.
#' @param colour An argument to specify the colour scheme for the plot. Default is "cochrane" which used the ubiquitous Cochrane colours, while a preset option for a colour-blind friendly palette is also available (colour = "colourblind").
#' @param psize Controll the size of the traffic lights
#' @param quiet An option to quietly produce and save the plot without it displaying in R/Rstudio.
#' @return Risk-of-bias assessment traffic light plot (ggplot2 object)
#' @export

rob_traffic_light <- function(data, tool, colour = "cochrane", psize = 20, quiet = FALSE) {

judgement <- NULL
Study <- NULL
domain <- NULL

# ROB 2 ========================================================================

if (tool == "ROB2") {

  # Define colouring
  if (length(colour) > 1) {
    low_colour <- colour[c(1)]
    concerns_colour <- colour[c(2)]
    high_colour <- colour[c(3)]
  }
  else {
    if (colour == "colourblind") {
      low_colour <- "#fee8c8"
      concerns_colour <- "#fdbb84"
      high_colour <- "#e34a33"
    }
    if (colour == "cochrane") {
      low_colour <- "#02C100"
      concerns_colour <- "#E2DF07"
      high_colour <- "#BF0000"
    }
  }



  for (i in 2:7) {
    data[[i]] <- tolower(data[[i]])
    data[[i]] <- trimws(data[[i]])
    data[[i]] <- substr(data[[i]], 0, 1)
  }

  data.tmp <- data
  if(NCOL(data.tmp) < 7){stop("Column missing (number of columns < 7).")}
  names(data.tmp)[1] <- "Study"
  names(data.tmp)[2] <- "D1"
  names(data.tmp)[3] <- "D2"
  names(data.tmp)[4] <- "D3"
  names(data.tmp)[5] <- "D4"
  names(data.tmp)[6] <- "D5"
  names(data.tmp)[7] <- "Overall"
  names(data.tmp)[8] <- "Weight"

  data.tmp <- data.tmp[, c(1:7)]

  rob.tidy <- suppressWarnings(tidyr::gather(data.tmp,
                                            domain, judgement, -Study))

  ssize <- psize - (psize/4)

trafficlightplot <-  ggplot2::ggplot(rob.tidy, ggplot2::aes(x=1, y=1, colour = judgement)) +
  ggplot2::facet_grid(Study ~ factor(domain, levels=c("D1",
                                             "D2",
                                             "D3",
                                             "D4",
                                             "D5",
                                             "Overall")), switch = "y", space = "free") +
  ggplot2::geom_point(size = psize) +
  ggplot2::geom_point(shape = 1, colour = "black", size = psize) +
  ggplot2::geom_point(size =ssize, colour = "black", ggplot2::aes(shape=judgement)) +
  ggplot2::labs(caption = "  D1: Bias due to randomisation.
  D2: Bias due to deviations from intended intervention.
  D3: Bias due to missing data.
  D4: Bias due to outcome measurement.
  D5: Bias due to selection of reported result.") +
  ggplot2::scale_x_discrete(position = "top", name = "Risk of bias domains") +
  ggplot2::scale_y_continuous(limits = c(1, 1), labels = NULL, breaks = NULL, name = "Study", position = "left") +
  ggplot2::scale_colour_manual(values =c(high_colour,low_colour,concerns_colour)) +
  ggplot2::scale_shape_manual(values = c(45,43,63)) +
  ggplot2::scale_size(range = c(5,20)) +
  ggplot2::theme_bw() +
  ggplot2::theme(panel.border = ggplot2::element_rect(colour = "grey"),
        panel.spacing = ggplot2::unit(0, "line"),
        legend.position = "none",
        strip.text.x = ggplot2::element_text(size = 10),
        strip.text.y = ggplot2::element_text(angle = 180, size = 10),
        plot.caption = ggplot2::element_text(hjust = 0))
  }

# ROBINS-I =====================================================================

if (tool == "ROBINS-I") {

  # Define colouring
  if (length(colour) > 1) {
    low_colour <- colour[c(1)]
    concerns_colour <- colour[c(2)]
    high_colour <- colour[c(3)]
    critical_colour <- colour[c(4)]
  }
  else {
    if (colour == "colourblind") {
      low_colour <- "#fef0d9"
      concerns_colour <- "#fdcc8a"
      high_colour <- "#fc8d59"
      critical_colour <- "#d7301f"
    }
    if (colour == "cochrane") {
      low_colour <- "#02C100"
      concerns_colour <- "#E2DF07"
      high_colour <- "#BF0000"
      critical_colour <- "#820000"
    }
  }

  for (i in 2:9) {
    data[[i]] <- tolower(data[[i]])
    data[[i]] <- trimws(data[[i]])
    data[[i]] <- substr(data[[i]], 0, 1)
  }

  data.tmp <- data
  if(NCOL(data.tmp) < 9){stop("Column missing (number of columns < 9).")}
  names(data.tmp)[1] <- "Study"
  names(data.tmp)[2] <- "D1"
  names(data.tmp)[3] <- "D2"
  names(data.tmp)[4] <- "D3"
  names(data.tmp)[5] <- "D4"
  names(data.tmp)[6] <- "D5"
  names(data.tmp)[7] <- "D6"
  names(data.tmp)[8] <- "D7"
  names(data.tmp)[9] <- "Overall"
  names(data.tmp)[10] <- "Weight"

  data.tmp <- data.tmp[, c(1:9)]

  rob.tidy <- suppressWarnings(tidyr::gather(data.tmp,
                                             domain, judgement, -Study))

  ssize <- psize - (psize/4)

  trafficlightplot <-  ggplot2::ggplot(rob.tidy, ggplot2::aes(x=1, y=1, colour = judgement)) +
    ggplot2::facet_grid(Study ~ factor(domain, levels=c("D1",
                                               "D2",
                                               "D3",
                                               "D4",
                                               "D5",
                                               "D6",
                                               "D7",
                                               "Overall")), switch = "y", space = "free") +
    ggplot2::geom_point(size = psize) +
    ggplot2::geom_point(shape = 1, colour = "black", size = psize) +
    ggplot2::geom_point(size =ssize, colour = "black", ggplot2::aes(shape=judgement)) +
    ggplot2::labs(caption = "  D1: Bias due to confounding.
  D2: Bias due to selection of participants.
  D3: Bias in classification of interventions.
  D4: Bias due to deviations from intended interventions.
  D5: Bias due to missing data.
  D6: Bias in measurement of outcomes.
  D7: Bias in selection of the reported result.") +
    ggplot2::scale_x_discrete(position = "top", name = "Risk of bias domains") +
    ggplot2::scale_y_continuous(limits = c(1, 1), labels = NULL, breaks = NULL, name = "Study", position = "left") +
    ggplot2::scale_colour_manual(values =c(critical_colour,high_colour,low_colour,concerns_colour)) +
    ggplot2::scale_shape_manual(values = c(33,45,43,63)) +
    ggplot2::scale_size(range = c(5,20)) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.border = ggplot2::element_rect(colour = "grey"),
          panel.spacing = ggplot2::unit(0, "line"),
          legend.position = "none",
          strip.text.x = ggplot2::element_text(size = 10),
          strip.text.y = ggplot2::element_text(angle = 180, size = 10),
          plot.caption = ggplot2::element_text(hjust = 0))
  }

# QUADAS-2 =====================================================================

if (tool == "QUADAS-2") {

  # Define colouring
  if (length(colour) > 1) {
    low_colour <- colour[c(1)]
    concerns_colour <- colour[c(2)]
    high_colour <- colour[c(3)]
  }
  else {
    if (colour == "colourblind") {
      low_colour <- "#fee8c8"
      concerns_colour <- "#fdbb84"
      high_colour <- "#e34a33"
    }
    if (colour == "cochrane") {
      low_colour <- "#02C100"
      concerns_colour <- "#E2DF07"
      high_colour <- "#BF0000"
    }
  }

  for (i in 2:6) {
    data[[i]] <- tolower(data[[i]])
    data[[i]] <- trimws(data[[i]])
    data[[i]] <- substr(data[[i]], 0, 1)
  }

  data.tmp <- data
  if(NCOL(data.tmp) < 6){stop("Column missing (number of columns < 6).")}
  names(data.tmp)[1] <- "Study"
  names(data.tmp)[2] <- "D1"
  names(data.tmp)[3] <- "D2"
  names(data.tmp)[4] <- "D3"
  names(data.tmp)[5] <- "D4"
  names(data.tmp)[6] <- "Overall"
  names(data.tmp)[7] <- "Weight"


  data.tmp <- data.tmp[, c(1:6)]

  rob.tidy <- suppressWarnings(tidyr::gather(data.tmp,
                                             domain, judgement, -Study))

  ssize <- psize - (psize/4)

trafficlightplot <-  ggplot2::ggplot(rob.tidy, ggplot2::aes(x=1, y=1, colour = judgement)) +
    ggplot2::facet_grid(Study ~ factor(domain, levels=c("D1",
                                               "D2",
                                               "D3",
                                               "D4",
                                               "Overall")), switch = "y", space = "free") +
    ggplot2::geom_point(size = psize) +
    ggplot2::geom_point(shape = 1, colour = "black", size = psize) +
    ggplot2::geom_point(size =ssize, colour = "black", ggplot2::aes(shape=judgement)) +
    ggplot2::labs(caption = "  D1: Patient selection.
  D2: Index test.
  D3: Reference standard.
  D4: Flow & timing.") +
    ggplot2::scale_x_discrete(position = "top", name = "Risk of bias domains") +
    ggplot2::scale_y_continuous(limits = c(1, 1), labels = NULL, breaks = NULL, name = "Study", position = "left") +
    ggplot2::scale_colour_manual(values =c(high_colour,low_colour,concerns_colour)) +
    ggplot2::scale_shape_manual(values = c(45,43,63)) +
    ggplot2::scale_size(range = c(5,20)) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.border = ggplot2::element_rect(colour = "grey"),
          panel.spacing = ggplot2::unit(0, "line"),
          legend.position = "none",
          strip.text.x = ggplot2::element_text(size = 10),
          strip.text.y = ggplot2::element_text(angle = 180, size = 10),
          plot.caption = ggplot2::element_text(hjust = 0))

  }


# ROB-1/Generic=================================================================

if (tool == "ROB1") {

  # Define colouring
  if (length(colour) > 1) {
    low_colour <- colour[c(1)]
    concerns_colour <- colour[c(2)]
    high_colour <- colour[c(3)]
  } else {
    if (colour == "colourblind") {
      low_colour <- "#fee8c8"
      concerns_colour <- "#fdbb84"
      high_colour <- "#e34a33"
    }
    if (colour == "cochrane") {
      low_colour <- "#02C100"
      concerns_colour <- "#E2DF07"
      high_colour <- "#BF0000"
    }
  }

  for (i in 2:(ncol(data)-1)) {
    data[[i]] <- tolower(data[[i]])
    data[[i]] <- trimws(data[[i]])
    data[[i]] <- substr(data[[i]], 0, 1)
  }

  data.tmp <- data

  #Select relevant columns (excluding weights)
  data.tmp <- data.tmp[, c(1:(ncol(data.tmp)-1))]

  #Remove dots from column names
  for (i in 1:(ncol(data.tmp))) {
    names(data.tmp)[i] <- invisible(gsub(".", " ", names(data.tmp)[i], fixed = TRUE))
  }

  # Create caption
  captiondf <- data.frame()
  for (i in 2:(ncol(data.tmp)-1)) {
    if (i == 2) {
    captiondf[i-1,1] <- paste0(" D",i-1,": ", names(data.tmp)[i],"\n")
    }else{
    captiondf[i-1,1] <- paste0("D",i-1,": ", names(data.tmp)[i],"\n")
    }
  }

  caption <- paste(captiondf$V1, collapse=" ")

  # Rename columns headings
  names(data.tmp)[1] <- "Study"
  for (i in 2:(ncol(data.tmp)-1)) {
    names(data.tmp)[i] <- paste0("D",i-1)
  }

  # Convert to long format
  rob.tidy <- suppressWarnings(tidyr::gather(data.tmp,
                                             domain, judgement, -Study))

  #Relevel rob.tidy$domain
  for (i in 1:(ncol(data.tmp))) {
    levels(rob.tidy$domain)[i] <- colnames(data.tmp)[i]
  }

  rob.tidy$domain <- factor(rob.tidy$domain, levels = levels(rob.tidy$domain))

  # Set sizes
  ssize <- psize - (psize/4)

  # PLot graph
  trafficlightplot <-  ggplot2::ggplot(rob.tidy, ggplot2::aes(x=1, y=1, colour = judgement)) +
    ggplot2::facet_grid(Study ~ factor(domain), switch = "y", space = "free") +
    ggplot2::geom_point(size = psize) +
    ggplot2::geom_point(shape = 1, colour = "black", size = psize) +
    ggplot2::geom_point(size =ssize, colour = "black", ggplot2::aes(shape=judgement)) +
    ggplot2::labs(caption = caption) +
    ggplot2::scale_x_discrete(position = "top", name = "Risk of bias domains") +
    ggplot2::scale_y_continuous(limits = c(1, 1), labels = NULL, breaks = NULL, name = "Study", position = "left") +
    ggplot2::scale_colour_manual(values =c(high_colour,low_colour,concerns_colour)) +
    ggplot2::scale_shape_manual(values = c(45,43,63)) +
    ggplot2::scale_size(range = c(5,20)) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.border = ggplot2::element_rect(colour = "grey"),
                   panel.spacing = ggplot2::unit(0, "line"),
                   legend.position = "none",
                   strip.text.x = ggplot2::element_text(size = 10),
                   strip.text.y = ggplot2::element_text(angle = 180, size = 10),
                   plot.caption = ggplot2::element_text(hjust = 0))
}

  if(quiet != TRUE) {
    return(trafficlightplot)
  }

}
