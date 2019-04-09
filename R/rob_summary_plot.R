#' Produce summary figure of risk of bias assessments.
#' @description TBC.
#' @param data A .csv file of your summary (domain) level risk of bias assessments, with the first column containing the study name, and the second column containing the first domain of your assessments.
#' @param tool The risk of bias assessment tool used. RoB2.0 (tool="ROB2"), ROBINS-I (tool="ROBINS-I"), and QUADAS-2 (tool="QUADAS-2") are currently supported.
#' @param save An option to save the plot as the specified file type. Default is "No", and available extensions are eps/ps, tex (pictex), pdf, jpeg, tiff, png, bmp, svg and wmf (windows only).
#' @return Risk of bias assessment figure in .png output.
#' @export

rob_summary <- function(data, tool, save = "No") {
  #End column depends on number of domains in tool (end <- no. of domains + 1)
  if (tool == "ROBINS-I") {
    end <- 8
  }
  if (tool == "QUADAS-2") {
    end <- 5
  }

if (tool == "ROB2") {
    start <- 2
    end <- 6
    data.tmp <- data
    print("Renaming columns...")
    names(data.tmp)[2] <- "Bias due to randomisation"
    names(data.tmp)[3] <- "Bias due to deviations from intended intervention"
    names(data.tmp)[4] <- "Bias due to missing data"
    names(data.tmp)[5] <- "Bias due to outcome measurement"
    names(data.tmp)[6] <- "Bias due to selection of reported result"

    rob.tidy <- suppressWarnings(tidyr::gather(data.tmp,
                                             domain, judgement,
                                             start:end))
    print("Data tidied")

    # rob.tidy <-

    rob.tidy$judgement <- as.factor(rob.tidy$judgement)
    rob.tidy$domain <- as.factor(rob.tidy$domain)

    # rob.tidy$domain <- as.factor(rob.tidy$domain)

    # rob.tidy$domain <- factor(rob.tidy$domain,
    #                                   levels(rob.tidy$domain)[c(5, 2, 3, 1, 4)])

    rob.tidy$domain <- factor(rob.tidy$domain,
                                      levels(rob.tidy$domain)[c(5, 2, 3, 1, 4)])


    rob.tidy$judgement <- factor(rob.tidy$judgement,
                                 levels(rob.tidy$judgement)[c(1, 3, 2)])

    plot <- ggplot2::ggplot(data = rob.tidy) +
      geom_bar(
        mapping = aes(x = domain, fill = judgement),
        width = 0.7,
        position = "fill",
        color = "black"
      ) +
      coord_flip(ylim = c(0, 1)) +
      guides(fill = guide_legend(reverse = TRUE)) +
      scale_fill_manual(
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
      scale_y_continuous(labels = scales::percent) +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 10, color = "black"),
        axis.line.x = element_line(
          colour = "black",
          size = 0.5,
          linetype = "solid"
        ),
        legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.background = element_rect(linetype = "solid",
                                         colour = "black"),
        legend.title = element_blank(),
        legend.key.size = unit(0.75, "cm"),
        legend.text = element_text(size = 8)
      )
  }

if (tool == "ROBINS-I") {
    start <- 2
    end <- 8
    data.tmp <- data
    print("Renaming columns...")
    names(data.tmp)[2] <- "Bias due to confounding"
    names(data.tmp)[3] <- "Bias due to selection of participants"
    names(data.tmp)[4] <- "Bias in classification of interventions"
    names(data.tmp)[5] <- "Bias due to deviations from intended interventions"
    names(data.tmp)[6] <- "Bias due to missing data"
    names(data.tmp)[7] <- "Bias in measurement of outcomes"
    names(data.tmp)[8] <- "Bias in selection of the reported result"

    rob.tidy <- suppressWarnings(tidyr::gather(data.tmp,
                                               domain, judgement,
                                               start:end))

    print("Data tidied")

    rob.tidy$judgement <- as.factor(rob.tidy$judgement)

    rob.tidy$domain <- as.factor(rob.tidy$domain)

    rob.tidy$domain <- factor(rob.tidy$domain,
                                      levels(rob.tidy$domain)[c(7, 6, 3, 5, 2, 4, 1)])

    rob.tidy$judgement <- factor(rob.tidy$judgement,
                                 levels(rob.tidy$judgement)[c(1, 2, 4, 3)])

    plot <- ggplot2::ggplot(data = rob.tidy) +
      geom_bar(
        mapping = aes(x = domain, fill = judgement),
        width = 0.7,
        position = "fill",
        color = "black"
      ) +
      coord_flip(ylim = c(0, 1)) +
      guides(fill = guide_legend(reverse = TRUE)) +
      scale_fill_manual(
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
      scale_y_continuous(labels = scales::percent) +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 10, color = "black"),
        axis.line.x = element_line(
          colour = "black",
          size = 0.5,
          linetype = "solid"
        ),
        legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.background = element_rect(linetype = "solid",
                                         colour = "black"),
        legend.title = element_blank(),
        legend.key.size = unit(0.75, "cm"),
        legend.text = element_text(size = 7)
      )
  }

if (tool == "QUADAS-2") {
  start <- 2
  end <- 5
  data.tmp <- data
  print("Renaming columns...")
  names(data.tmp)[2] <- "Patient selection"
  names(data.tmp)[3] <- "Index test"
  names(data.tmp)[4] <- "Reference standard"
  names(data.tmp)[5] <- "Flow & timing"

  rob.tidy <- suppressWarnings(tidyr::gather(data.tmp,
                                             domain, judgement,
                                             start:end))
  print("Data tidied")

    rob.tidy$judgement <- as.factor(rob.tidy$judgement)

    rob.tidy$domain <- as.factor(rob.tidy$domain)

    rob.tidy$judgement <- factor(rob.tidy$judgement,
                                 levels(rob.tidy$judgement)[c(1, 3, 2)])

    rob.tidy$domain <- factor(rob.tidy$domain,
                                      levels(rob.tidy$domain)[c(1, 4, 2, 3)])

    plot <- ggplot2::ggplot(data = rob.tidy) +
      geom_bar(
        mapping = aes(x = domain, fill = judgement),
        width = 0.7,
        position = "fill",
        color = "black"
      ) +
      coord_flip(ylim = c(0, 1)) +
      guides(fill = guide_legend(reverse = TRUE)) +
      scale_fill_manual(
        "Risk of Bias",
        values = c(
          "Low" = "#66c2a5",
          "Some concerns" = "#808080",
          "High" = "#fc8d62"
        ),
        labels = c("  Low risk of bias   ",
                   "  Some concerns      ",
                   "  High risk of bias  ")
      ) +
      scale_y_continuous(labels = scales::percent) +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.line.x = element_line(
          colour = "black",
          size = 0.5,
          linetype = "solid"
        ),
        legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.background = element_rect(linetype = "solid",
                                         colour = "black"),
        legend.title = element_blank(),
        legend.key.size = unit(0.75, "cm"),
        legend.text = element_text(size = 10)
      )
  }

  if (save != "No") {
    extension <- paste(save)
    filename <- paste(tool, "_summary_figure",extension, sep = "")
    ggplot2::ggsave(filename, width = 8, height = 2.41)
    print("Figure saved")
  }

  return(plot)
}
