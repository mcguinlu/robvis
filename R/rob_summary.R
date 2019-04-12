#' Produce summary figure of risk of bias assessments.
#' @description A function to convert standard risk-of-bias output to tidy data .
#' @param data A .csv file of your summary (domain) level risk of bias assessments, with the first column containing the study name, and the second column containing the first domain of your assessments.
#' @param tool The risk of bias assessment tool used. RoB2.0 (tool="ROB2"), ROBINS-I (tool="ROBINS-I"), and QUADAS-2 (tool="QUADAS-2") are currently supported.
#' @param save An option to save the plot as the specified file type. Default is "No", and available extensions are eps/ps, tex (pictex), pdf, jpeg, tiff, png, bmp, svg and wmf (windows only).
#' @param overall An option to include a bar for overall risk-of-bias in the figure. Default is FALSE.
#' @return Risk of bias assessment figure in .png output.
#' @export

rob_summary <- function(data, tool, overall = FALSE, save = FALSE) {

if (tool == "ROB2") {
    data.tmp <- data
    if(NCOL(data.tmp) < 8){stop("Column missing (number of columns < 8). Likely that a column detailing weights for each study is missing.")}
    names(data.tmp)[2] <- "Bias due to randomisation"
    names(data.tmp)[3] <- "Bias due to deviations from intended intervention"
    names(data.tmp)[4] <- "Bias due to missing data"
    names(data.tmp)[5] <- "Bias due to outcome measurement"
    names(data.tmp)[6] <- "Bias due to selection of reported result"
    names(data.tmp)[7] <- "Overall risk of bias"
    names(data.tmp)[8] <- "Weight"

    #Define dataframe based on value of "overall"
    if (overall == "FALSE") {
      data.tmp <- data.tmp[, c(2:6, 8)]
    }
    else{
      data.tmp <- data.tmp[, c(2:8)]
    }

    rob.tidy <- suppressWarnings(tidyr::gather(data.tmp,
                                             domain, judgement,
                                             -Weight))

    rob.tidy$judgement <- as.factor(rob.tidy$judgement)

    rob.tidy$domain <- as.factor(rob.tidy$domain)

    rob.tidy$domain <- factor(rob.tidy$domain,
                                      levels(rob.tidy$domain)[c(6, 5, 3, 2, 1, 4)])

    rob.tidy$judgement <- factor(rob.tidy$judgement,
                                 levels(rob.tidy$judgement)[c(1, 3, 2)])

    plot <- ggplot2::ggplot(data = rob.tidy) +
      ggplot2::geom_bar(
        mapping = ggplot2::aes(x = domain, fill = judgement,weight = Weight),
        width = 0.7,
        position = "fill",
        color = "black"
      ) +
      ggplot2::coord_flip(ylim = c(0, 1)) +
      ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE)) +
      ggplot2::scale_fill_manual(
        "Risk of Bias",
        values = c(
          "Low" = "#66c2a5",
          "Some concerns" = "#808080",
          "High" = "#fc8d62"
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

if (tool == "ROBINS-I") {
    data.tmp <- data
    if(NCOL(data.tmp) < 10){stop("Column missing (number of columns < 10). Likely that a column detailing weights for each study is missing.")}
    names(data.tmp)[2] <- "Bias due to confounding"
    names(data.tmp)[3] <- "Bias due to selection of participants"
    names(data.tmp)[4] <- "Bias in classification of interventions"
    names(data.tmp)[5] <- "Bias due to deviations from intended interventions"
    names(data.tmp)[6] <- "Bias due to missing data"
    names(data.tmp)[7] <- "Bias in measurement of outcomes"
    names(data.tmp)[8] <- "Bias in selection of the reported result"
    names(data.tmp)[9] <- "Overall risk of bias"
    names(data.tmp)[10] <- "Weight"

    #Define dataframe based on value of "overall"
    if (overall == "FALSE") {
      data.tmp <- data.tmp[, c(2:8, 10)]
    }
    else{
      data.tmp <- data.tmp[, c(2:10)]
    }
    rob.tidy <- suppressWarnings(tidyr::gather(data.tmp,
                                               domain, judgement,
                                               -Weight))

    rob.tidy$judgement <- as.factor(rob.tidy$judgement)

    rob.tidy$domain <- as.factor(rob.tidy$domain)

    rob.tidy$domain <- factor(rob.tidy$domain,
                                      levels(rob.tidy$domain)[c(8, 7, 6, 3, 5, 2, 4, 1)])

    rob.tidy$judgement <- factor(rob.tidy$judgement,
                                 levels(rob.tidy$judgement)[c(1, 2, 4, 3)])

    plot <- ggplot2::ggplot(data = rob.tidy) +
      ggplot2::geom_bar(
        mapping = ggplot2::aes(x = domain, fill = judgement),
        width = 0.7,
        position = "fill",
        color = "black"
      ) +
      ggplot2::coord_flip(ylim = c(0, 1)) +
      ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE)) +
      ggplot2::scale_fill_manual(
        "Risk of Bias",
        values = c(
          "High" = "#fc8d62",
          "Some concerns" = "#808080",
          "Low" = "#66c2a5",
          "Critical" = "#ff0000"
        ),
        labels = c(
          "  Critical risk of bias  ",
          "  High risk of bias  ",
          "  Some concerns  ",
          "  Low risk of bias  "
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
        legend.key.size = ggplot2::unit(0.75, "cm"),
        legend.text = ggplot2::element_text(size = 7)
      )
  }

if (tool == "QUADAS-2") {
  data.tmp <- data
  if(NCOL(data.tmp) < 7){stop("Column missing (number of columns < 7). Likely that a column detailing weights for each study is missing.")}
  names(data.tmp)[2] <- "Patient selection"
  names(data.tmp)[3] <- "Index test"
  names(data.tmp)[4] <- "Reference standard"
  names(data.tmp)[5] <- "Flow & timing"
  names(data.tmp)[6] <- "Overall risk of bias"
  names(data.tmp)[7] <- "Weight"

  #Define dataframe based on value of "overall"
  if (overall == "FALSE") {
    data.tmp <- data.tmp[, c(2:5, 7)]
  }
  else{
    data.tmp <- data.tmp[, c(2:7)]
  }

  rob.tidy <- suppressWarnings(tidyr::gather(data.tmp,
                                             domain, judgement,
                                             -Weight))

  rob.tidy$judgement <- as.factor(rob.tidy$judgement)

    rob.tidy$domain <- as.factor(rob.tidy$domain)

    rob.tidy$judgement <- factor(rob.tidy$judgement,
                                 levels(rob.tidy$judgement)[c(1, 3, 2)])

    rob.tidy$domain <- factor(rob.tidy$domain,
                                      levels(rob.tidy$domain)[c(3,1,5,2,4)])

    plot <- ggplot2::ggplot(data = rob.tidy) +
      ggplot2::geom_bar(
        mapping = ggplot2::aes(x = domain, fill = judgement),
        width = 0.7,
        position = "fill",
        color = "black"
      ) +
      ggplot2::coord_flip(ylim = c(0, 1)) +
      ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE)) +
      ggplot2::scale_fill_manual(
        "Risk of Bias",
        values = c(
          "High" = "#fc8d62",
          "Some concerns" = "#808080",
          "Low" = "#66c2a5"
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

  if (save != FALSE) {
    extension <- paste(save)
    filename <- paste(tool, "_summary_figure",extension, sep = "")
    ggplot2::ggsave(filename, width = 8, height = 2.41)
  }

  return(plot)
}
