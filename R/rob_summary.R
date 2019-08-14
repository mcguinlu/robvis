#' Produce summary weighted barplots of risk-of-bias assessments.
#' @description A function to convert standard risk-of-bias output to tidy data and plot a summary barplot.
#' @param data A dataframe containing summary (domain) level risk-of-bias assessments, with the first column containing the study details, the second column containing the first domain of your assessments, and the final column containing a weight to assign to each study. The function assumes that the data includes a column for overall risk-of-bias. For example, a ROB2.0 dataset would have 8 columns (1 for study details, 5 for domain level judgements, 1 for overall judgements, and 1 for weights, in that order).
#' @param tool The risk of bias assessment tool used. RoB2.0 (tool="ROB2"), ROBINS-I (tool="ROBINS-I"), and QUADAS-2 (tool="QUADAS-2") are currently supported.
#' @param overall An option to include a bar for overall risk-of-bias in the figure. Default is FALSE.
#' @param quiet An option to quietly produce the plot without displaying it.
#' @param colour An argument to specify the colour scheme for the plot. Default is "cochrane" which used the ubiquitous Cochrane colours, while a preset option for a colour-blind friendly palette is also available (colour = "colourblind").
#' @param weighted An option to specify whether weights should be used in the barplot. Default is TRUE, in line with current Cochrane Collaboration guidance.
#' @return Risk of bias assessment barplot figure.
#' @export

rob_summary <- function(data, tool, overall = FALSE, weighted = TRUE, colour = "cochrane", quiet = FALSE) {

  judgement <- NULL
  Weights <- NULL
  domain <- NULL

# ROB2 =========================================================================

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

    # Data preprocessing
    for (i in 2:7) {
      data[[i]] <- tolower(data[[i]])
      data[[i]] <- trimws(data[[i]])
      data[[i]] <- substr(data[[i]], 0, 1)
    }

    # Define weights if FALSE and check if there is a weight column if TRUE
    if(weighted == FALSE) {
      data[,8] <- rep(1,length(nrow(data)))
    } else {
      if(NCOL(data) < 8){stop("Column missing (number of columns < 8). Likely that a column detailing weights for each study is missing.")}
    }

    # Rename column headings
    data.tmp <- data
    names(data.tmp)[2] <- "Bias arising from the randomization process"
    names(data.tmp)[3] <- "Bias due to deviations from intended interventions"
    names(data.tmp)[4] <- "Bias due to missing outcome data"
    names(data.tmp)[5] <- "Bias in measurement of the outcome"
    names(data.tmp)[6] <- "Bias in selection of the reported result"
    names(data.tmp)[7] <- "Overall risk of bias"
    names(data.tmp)[8] <- "Weights"


    #Define dataframe based on value of "overall"
    if (overall == "FALSE") {
      data.tmp <- data.tmp[, c(2:6, 8)]
    }
    else {
      data.tmp <- data.tmp[, c(2:8)]
    }


    #Gather data, convert to factors and set levels
    rob.tidy <- suppressWarnings(tidyr::gather(data.tmp,
                                               domain, judgement,
                                               -Weights))

    rob.tidy$judgement <- as.factor(rob.tidy$judgement)

    rob.tidy$domain <- as.factor(rob.tidy$domain)

    rob.tidy$domain <- factor(rob.tidy$domain,
                              levels(rob.tidy$domain)[c(6, 5, 3, 2, 1, 4)])

    rob.tidy$judgement <- factor(rob.tidy$judgement,
                                 levels(rob.tidy$judgement)[c(1, 3, 2)])

    # Create plot
    plot <- ggplot2::ggplot(data = rob.tidy) +
      ggplot2::geom_bar(
        mapping = ggplot2::aes(x = domain, fill = judgement, weight = Weights),
        width = 0.7,
        position = "fill",
        color = "black"
      ) +
      ggplot2::coord_flip(ylim = c(0, 1)) +
      ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE)) +
      ggplot2::scale_fill_manual(
        "Risk of Bias",
        values = c(
          "l" = low_colour,
          "s" = concerns_colour,
          "h" = high_colour
        ),
        labels = c("  High risk of bias  ",
                   "  Some concerns      ",
                   "  Low risk of bias   ")
      ) +
      ggplot2::scale_y_continuous(labels = scales::percent) +
      ggplot2::theme(
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(size = 10, color = "black"),
        axis.line.x = ggplot2::element_line(
          colour = "black",
          size = 0.5,
          linetype = "solid"
        ),
        legend.position = "bottom",
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        legend.background = ggplot2::element_rect(linetype = "solid",
                                                  colour = "black"),
        legend.title = ggplot2::element_blank(),
        legend.key.size = ggplot2::unit(0.75, "cm"),
        legend.text = ggplot2::element_text(size = 8)
      )
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


    # Data preprocessing
    for (i in 2:9) {
      data[[i]] <- tolower(data[[i]])
      data[[i]] <- trimws(data[[i]])
      data[[i]] <- substr(data[[i]], 0, 1)
    }

    # Define weights if FALSE and check if there is a weight column if TRUE
    if(weighted == FALSE) {
      data[,10] <- rep(1,length(nrow(data)))
    } else {
      if(NCOL(data) < 10){stop("Column missing (number of columns < 8). Likely that a column detailing weights for each study is missing.")}
    }


    data.tmp <- data
    names(data.tmp)[2] <- "Bias due to confounding"
    names(data.tmp)[3] <- "Bias due to selection of participants"
    names(data.tmp)[4] <- "Bias in classification of interventions"
    names(data.tmp)[5] <- "Bias due to deviations from intended interventions"
    names(data.tmp)[6] <- "Bias due to missing data"
    names(data.tmp)[7] <- "Bias in measurement of outcomes"
    names(data.tmp)[8] <- "Bias in selection of the reported result"
    names(data.tmp)[9] <- "Overall risk of bias"
    names(data.tmp)[10] <- "Weights"

    #Define dataframe based on value of "overall"
    if (overall == "FALSE") {
      data.tmp <- data.tmp[, c(2:8, 10)]
    }
    else{
      data.tmp <- data.tmp[, c(2:10)]
    }


    rob.tidy <- suppressWarnings(tidyr::gather(data.tmp,
                                               domain, judgement,
                                               -Weights))

    rob.tidy$judgement <- as.factor(rob.tidy$judgement)

    rob.tidy$domain <- as.factor(rob.tidy$domain)

    rob.tidy$domain <- factor(rob.tidy$domain,
                              levels(rob.tidy$domain)[c(8, 7, 6, 3, 5, 2, 4, 1)])

    rob.tidy$judgement <- factor(rob.tidy$judgement,
                                 levels(rob.tidy$judgement)[c(1,4,3,2)])

    plot <- ggplot2::ggplot(data = rob.tidy) +
      ggplot2::geom_bar(
        mapping = ggplot2::aes(x = domain, fill = judgement, weight = Weights),
        width = 0.7,
        position = "fill",
        color = "black"
      ) +
      ggplot2::coord_flip(ylim = c(0, 1)) +
      ggplot2::scale_fill_manual(
        values = c(
          "m" = concerns_colour,
          "s" = high_colour,
          "l" = low_colour,
          "c" = critical_colour
        ),
        labels = c(
          " Critical risk  ",
          " Serious risk  ",
          " Moderate risk ",
          " Low risk  "
        )
      ) +
      ggplot2::scale_y_continuous(labels = scales::percent) +
      ggplot2::theme(
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(size = 10, color = "black"),
        axis.line.x = ggplot2::element_line(
          colour = "black",
          size = 0.5,
          linetype = "solid"
        ),
        legend.position = "bottom",
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        legend.background = ggplot2::element_rect(linetype = "solid",
                                                  colour = "black"),
        legend.title = ggplot2::element_blank(),
        legend.key.size = ggplot2::unit(0.7, "cm"),
        legend.text = ggplot2::element_text(size = 8)
      ) +
      ggplot2::guides(fill = ggplot2::guide_legend(title = "Risk of bias judgement",
                                                   title.position = "bottom",
                                                   title.hjust = 0.5,
                                                   reverse = TRUE))
  }

# ROBINS-I ONLINE============================================================
  if (tool == "ROBINS-I Online") {

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

    data <- data[,grepl("studyId|RBJ_answer",names(data))]
    data <- data[,which(is.na(data) == FALSE)]

    # Data preprocessing
    for (i in 2:9) {
      data[[i]] <- tolower(data[[i]])
      data[[i]] <- trimws(data[[i]])
      data[[i]] <- substr(data[[i]], 0, 1)
    }

    # Define weights if FALSE and check if there is a weight column if TRUE
    if(weighted == FALSE) {
      data[,10] <- rep(1,length(nrow(data)))
    } else {
      if(NCOL(data) < 10){stop("Column missing (number of columns < 10). Likely that a column detailing weights for each study is missing.")}
    }


    data.tmp <- data
    names(data.tmp)[2] <- "Bias due to confounding"
    names(data.tmp)[3] <- "Bias due to selection of participants"
    names(data.tmp)[4] <- "Bias in classification of interventions"
    names(data.tmp)[5] <- "Bias due to deviations from intended interventions"
    names(data.tmp)[6] <- "Bias due to missing data"
    names(data.tmp)[7] <- "Bias in measurement of outcomes"
    names(data.tmp)[8] <- "Bias in selection of the reported result"
    names(data.tmp)[9] <- "Overall risk of bias"
    names(data.tmp)[10] <- "Weights"

    #Define dataframe based on value of "overall"
    if (overall == "FALSE") {
      data.tmp <- data.tmp[, c(2:8, 10)]
    }
    else{
      data.tmp <- data.tmp[, c(2:10)]
    }


    rob.tidy <- suppressWarnings(tidyr::gather(data.tmp,
                                               domain, judgement,
                                               -Weights))

    rob.tidy$judgement <- as.factor(rob.tidy$judgement)

    rob.tidy$domain <- as.factor(rob.tidy$domain)

    rob.tidy$domain <- factor(rob.tidy$domain,
                              levels(rob.tidy$domain)[c(8, 7, 6, 3, 5, 2, 4, 1)])

    rob.tidy$judgement <- factor(rob.tidy$judgement,
                                 levels(rob.tidy$judgement)[c(1,4,3,2)])

    plot <- ggplot2::ggplot(data = rob.tidy) +
      ggplot2::geom_bar(
        mapping = ggplot2::aes(x = domain, fill = judgement, weight = Weights),
        width = 0.7,
        position = "fill",
        color = "black"
      ) +
      ggplot2::coord_flip(ylim = c(0, 1)) +
      ggplot2::scale_fill_manual(

        values = c(
          "m" = concerns_colour,
          "s" = high_colour,
          "l" = low_colour,
          "c" = critical_colour
        ),
        labels = c(
          " Critical risk  ",
          " Serious risk  ",
          " Moderate risk ",
          " Low risk  "
        )
      ) +
      ggplot2::scale_y_continuous(labels = scales::percent) +
      ggplot2::theme(
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(size = 10, color = "black"),
        axis.line.x = ggplot2::element_line(
          colour = "black",
          size = 0.5,
          linetype = "solid"
        ),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        legend.background = ggplot2::element_rect(linetype = "solid",
                                                  colour = "black"),
        legend.title = ggplot2::element_blank(),
        legend.key.size = ggplot2::unit(0.7, "cm"),
        legend.text = ggplot2::element_text(size = 8)
      ) +
      ggplot2::guides(fill = ggplot2::guide_legend(title = "Risk of bias judgement",
                                                   title.position = "bottom",
                                                   title.hjust = 0.5,
                                                   reverse = TRUE))
  }

# QUADAS-2 =====================================================================

if (tool == "QUADAS-2") {

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


    # Data preprocessing
    for (i in 2:6) {
      data[[i]] <- tolower(data[[i]])
      data[[i]] <- trimws(data[[i]])
      data[[i]] <- substr(data[[i]], 0, 1)
    }

    # Define weights if FALSE and check if there is a weight column if TRUE
    if(weighted == FALSE) {
      data[,7] <- rep(1,length(nrow(data)))
    } else {
      if(NCOL(data) < 7){stop("Column missing (number of columns < 8). Likely that a column detailing weights for each study is missing.")}
    }

    data.tmp <- data
    names(data.tmp)[2] <- "Patient selection"
    names(data.tmp)[3] <- "Index test"
    names(data.tmp)[4] <- "Reference standard"
    names(data.tmp)[5] <- "Flow & timing"
    names(data.tmp)[6] <- "Overall risk of bias"
    names(data.tmp)[7] <- "Weights"

    #Define dataframe based on value of "overall"
    if (overall == "FALSE") {
      data.tmp <- data.tmp[, c(2:5, 7)]
    }
    else{
      data.tmp <- data.tmp[, c(2:7)]
    }

    rob.tidy <- suppressWarnings(tidyr::gather(data.tmp,
                                               domain, judgement,
                                               -Weights))

    rob.tidy$judgement <- as.factor(rob.tidy$judgement)

    rob.tidy$domain <- as.factor(rob.tidy$domain)

    rob.tidy$judgement <- factor(rob.tidy$judgement,
                                 levels(rob.tidy$judgement)[c(1, 3, 2)])

    rob.tidy$domain <- factor(rob.tidy$domain,
                              levels(rob.tidy$domain)[c(3,1,5,2,4)])

    plot <- ggplot2::ggplot(data = rob.tidy) +
      ggplot2::geom_bar(
        mapping = ggplot2::aes(x = domain, fill = judgement, weight = Weights),
        width = 0.7,
        position = "fill",
        color = "black"
      ) +
      ggplot2::coord_flip(ylim = c(0, 1)) +
      ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE)) +
      ggplot2::scale_fill_manual(
        "Risk of Bias",
        values = c(
          "h" = high_colour,
          "s" = concerns_colour,
          "l" = low_colour
        ),
        labels = c(
          "  High risk of bias   ",
          "  Some concerns      ",
          "  Low risk of bias  ")
      ) +
      ggplot2::scale_y_continuous(labels = scales::percent) +
      ggplot2::theme(
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(size = 12, color = "black"),
        axis.line.x = ggplot2::element_line(
          colour = "black",
          size = 0.5,
          linetype = "solid"
        ),
        legend.position = "bottom",
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        legend.background = ggplot2::element_rect(linetype = "solid",
                                                  colour = "black"),
        legend.title = ggplot2::element_blank(),
        legend.key.size = ggplot2::unit(0.75, "cm"),
        legend.text = ggplot2::element_text(size = 10)
      )
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

    # Data preprocessing
    for (i in 2:(ncol(data)-1)) {
      data[[i]] <- tolower(data[[i]])
      data[[i]] <- trimws(data[[i]])
      data[[i]] <- substr(data[[i]], 0, 1)
      data[[i]] <- gsub("u","s",data[[i]])
    }

    # Define weights if FALSE and check if there is a weight column if TRUE
    if(weighted == FALSE) {
      data[,ncol(data)] <- rep(1,length(nrow(data)))
    } else {
      if(is.numeric(data[2,ncol(data)]) == FALSE){stop("Error. The final column does not seem to contain numeric values (expected for weights).")}
    }

    # Clean and rename column headings. as needed
    data.tmp <- data
    for (i in 2:(ncol(data)-1)) {
      names(data.tmp)[i] <- invisible(gsub(".", " ", names(data.tmp)[i], fixed = TRUE))
    }
    names(data.tmp)[ncol(data.tmp)] <- "Weights"



    # Define dataframe based on value of "overall"


    if (overall == "FALSE") {
      data.tmp <- data.tmp[, c(2:(ncol(data.tmp)-2), ncol(data.tmp))]
    } else {
      data.tmp <- data.tmp[, c(2:ncol(data.tmp))]
    }

    #Gather data, convert to factors and set levels
    rob.tidy <- suppressWarnings(tidyr::gather(data.tmp,
                                               domain, judgement,
                                               -Weights))

    rob.tidy$judgement <- as.factor(rob.tidy$judgement)

    rob.tidy$judgement <- factor(rob.tidy$judgement,
                                 levels(rob.tidy$judgement)[c(1, 3, 2)])

    for (i in 1:(ncol(data.tmp)-1)) {
    levels(rob.tidy$domain)[i] <- colnames(data.tmp)[i]
    }

    rob.tidy$domain <- factor(rob.tidy$domain, levels = rev(levels(rob.tidy$domain)))

    # Create plot
    plot <- ggplot2::ggplot(data = rob.tidy) +
      ggplot2::geom_bar(
        mapping = ggplot2::aes(x = domain, fill = judgement, weight = Weights),
        width = 0.7,
        position = "fill",
        color = "black"
      ) +
      ggplot2::coord_flip(ylim = c(0, 1)) +
      ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE)) +
      ggplot2::scale_fill_manual(
        "Risk of Bias",
        values = c(
          "l" = low_colour,
          "s" = concerns_colour,
          "h" = high_colour
        ),
        labels = c("  High risk of bias  ",
                   "  Some concerns      ",
                   "  Low risk of bias   ")
      ) +
      ggplot2::scale_y_continuous(labels = scales::percent) +
      ggplot2::theme(
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(size = 10, color = "black"),
        axis.line.x = ggplot2::element_line(
          colour = "black",
          size = 0.5,
          linetype = "solid"
        ),
        legend.position = "bottom",
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        legend.background = ggplot2::element_rect(linetype = "solid",
                                                  colour = "black"),
        legend.title = ggplot2::element_blank(),
        legend.key.size = ggplot2::unit(0.75, "cm"),
        legend.text = ggplot2::element_text(size = 8)
      )
  }



# Return plot ==================================================================

  if(quiet != TRUE) {
    return(plot)
  }
}
