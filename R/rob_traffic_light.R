#' Produce traffic-light plots of risk-of-bias assessments.
#' @description A function to take a summary table of risk of bias assessments and produce a traffic light plot from it.
#' @param data A dataframe containing summary (domain) level risk-of-bias assessments, with the first column containing the study details, the second column containing the first domain of your assessments, and the final column containing a weight to assign to each study. The function assumes that the data includes a column for overall risk-of-bias. For example, a ROB2.0 dataset would have 8 columns (1 for study details, 5 for domain level judgments, 1 for overall judgements, and 1 for weights, in that order).
#' @param tool The risk of bias assessment tool used. RoB2.0 (tool='ROB2'), ROBINS-I (tool='ROBINS-I'), and QUADAS-2 (tool='QUADAS-2') are currently supported.
#' @param colour An argument to specify the colour scheme for the plot. Default is 'cochrane' which used the ubiquitous Cochrane colours, while a preset option for a colour-blind friendly palette is also available (colour = 'colourblind').
#' @param psize Control the size of the traffic lights. Default is 20.
#' @param quiet An option to quietly produce the plot without displaying it.
#' @return Risk-of-bias assessment traffic light plot (ggplot2 object)
#' @examples
#'
#' data <- data.frame(stringsAsFactors=FALSE,
#'                    Study = c("Study 1", "Study 2"),
#'                    D1 = c("Low", "Some concerns"),
#'                    D2 = c("Low", "Low"),
#'                    D3 = c("Low", "Low"),
#'                    D4 = c("Low", "Low"),
#'                    D5 = c("Low", "Low"),
#'                    Overall = c("Low", "Low"),
#'                    Weight = c(33.33333333, 33.33333333)
#'                    )
#'
#' rob_traffic_light(data, "ROB2")
#'
#' @export

rob_traffic_light <- function(data, tool, colour = "cochrane",
    psize = 20, quiet = FALSE) {

    judgement <- NULL
    Study <- NULL
    domain <- NULL

    # Allow for depreciated "ROB1" argument
    tools <- c(rob_tools(),"ROB1")

    if((tool %in% tools)==FALSE) {
      stop(
        paste("\nTool name \"",
              tool,
              "\" not recognised \nAcceptable tools names can be found using the rob_tools() function")
      )
    }

    # Define colours
    na_colour <- "#cccccc"

    if (tool == "ROB2" || tool == "ROB2-Cluster" || tool == "QUADAS-2") {
      if (length(colour) > 1) {
        low_colour <- colour[c(1)]
        concerns_colour <- colour[c(2)]
        high_colour <- colour[c(3)]
        ni_colour <- colour[c(4)]
      } else {
        if (colour == "colourblind") {
          low_colour <- "#fed98e"
          concerns_colour <- "#fe9929"
          high_colour <- "#d95f0e"
          ni_colour <- "#ffffff"
        }
        if (colour == "cochrane") {
          low_colour <- "#02C100"
          concerns_colour <- "#E2DF07"
          high_colour <- "#BF0000"
          ni_colour <- "#4EA1F7"
        }
      }
    }else{
      if (length(colour) > 1) {
        low_colour <- colour[c(1)]
        concerns_colour <- colour[c(2)]
        high_colour <- colour[c(3)]
        critical_colour <- colour[c(4)]
        ni_colour <- colour[c(5)]
      } else {
        if (colour == "colourblind") {
          low_colour <- "#fed98e"
          concerns_colour <- "#fe9929"
          high_colour <- "#d95f0e"
          critical_colour <- "#993404"
          ni_colour <- "#ffffff"
        }
        if (colour == "cochrane") {
          low_colour <- "#02C100"
          concerns_colour <- "#E2DF07"
          high_colour <- "#BF0000"
          critical_colour <- "#820000"
          ni_colour <- "#4EA1F7"
        }
      }
    }


# ROB-2=========================================================================

    if (tool == "ROB2") {



        for (i in 2:7) {
            data[[i]] <- gsub('\\b(\\pL)\\pL{1,}|.','\\U\\1',data[[i]],perl = TRUE)
            data[[i]] <- tolower(data[[i]])
            data[[i]] <- trimws(data[[i]])
            data[[i]] <- ifelse(data[[i]]%in%c("na","n")|is.na(data[[i]]),"x",data[[i]])
            data[[i]] <- substr(data[[i]], 0, 1)
        }

        data.tmp <- data[, c(1:7)]
        if (NCOL(data.tmp) < 7) {
            stop("Column missing (number of columns < 7).")
        }
        names(data.tmp)[1] <- "Study"
        names(data.tmp)[2] <- "D1"
        names(data.tmp)[3] <- "D2"
        names(data.tmp)[4] <- "D3"
        names(data.tmp)[5] <- "D4"
        names(data.tmp)[6] <- "D5"
        names(data.tmp)[7] <- "Overall"

        rob.tidy <- suppressWarnings(tidyr::gather(data.tmp,
            domain, judgement, -Study))

        ssize <- psize - (psize/4)

        rob.tidy$Study <- factor(rob.tidy$Study, levels = unique(data.tmp$Study))

        rob.tidy$judgement <- as.factor(rob.tidy$judgement)

        rob.tidy$judgement <- factor(rob.tidy$judgement, levels = c("h", "s", "l", "n", "x"))

        adjust_caption <- -0.7 + length(unique(rob.tidy$judgement))*-0.6

        trafficlightplot <- ggplot2::ggplot(rob.tidy, ggplot2::aes(x = 1,
            y = 1, colour = judgement)) + ggplot2::facet_grid(Study ~
            factor(domain, levels = c("D1", "D2", "D3", "D4",
                "D5", "Overall")), switch = "y", space = "free") +
            ggplot2::geom_point(size = 6) + ggplot2::geom_point(size = 4,
            colour = "black", ggplot2::aes(shape = judgement)) +
            ggplot2::geom_rect(data = rob.tidy[which(rob.tidy$domain !=
                "Overall"), ], fill = "#ffffff", xmin = -Inf,
                xmax = Inf, ymin = -Inf, ymax = Inf, show.legend = FALSE) +
            ggplot2::geom_rect(data = rob.tidy[which(rob.tidy$domain ==
                "Overall"), ], fill = "#d3d3d3", xmin = -Inf,
                xmax = Inf, ymin = -Inf, ymax = Inf, show.legend = FALSE) +
            ggplot2::geom_point(size = psize, show.legend = FALSE) +
            ggplot2::geom_point(data = rob.tidy[which(rob.tidy$judgement !=
                                                        "x"), ],shape = 1, colour = "black",
                size = psize, show.legend = FALSE) +
          ggplot2::geom_point(size = ssize,
            colour = "black", ggplot2::aes(shape = judgement),
            show.legend = FALSE) +
            ggplot2::labs(caption = "  Domains:
  D1: Bias arising from the randomization process
  D2: Bias due to deviations from intended intervention.
  D3: Bias due to missing outcome data.
  D4: Bias in measurement of the outcome.
  D5: Bias in selection of the reported result.


                ") +
            ggplot2::scale_x_discrete(position = "top", name = "Risk of bias domains") +
            ggplot2::scale_y_continuous(limits = c(1, 1), labels = NULL,
                breaks = NULL, name = "Study", position = "left") +
            ggplot2::scale_colour_manual(values = c(h = high_colour,
                s = concerns_colour, l = low_colour, n= ni_colour, x = na_colour), labels = c(h = "High",
                s = "Some concerns", l = "Low", n= "No information", x = "Not applicable")) +
          ggplot2::scale_shape_manual(values = c(h = 120,
            s = 45, l = 43, n = 63, x = 32), labels = c(h = "High", s = "Some concerns",
            l = "Low", n= "No information", x = "Not applicable")) +
          ggplot2::scale_size(range = c(5,
            20)) + ggplot2::theme_bw() +
          ggplot2::theme(panel.border = ggplot2::element_rect(colour = "grey"),
            panel.spacing = ggplot2::unit(0, "line"), legend.position = "bottom",
            legend.justification = "right", legend.direction = "vertical",
            legend.margin = ggplot2::margin(t = -0.2, r = 0,
                b = adjust_caption, l = -10, unit = "cm"),
            strip.text.x = ggplot2::element_text(size = 10),
            strip.text.y.left = ggplot2::element_text(angle = 0,
                size = 10), legend.text = ggplot2::element_text(size = 9),
            legend.title = ggplot2::element_text(size = 9),
            strip.background = ggplot2::element_rect(fill = "#a9a9a9"),
            plot.caption = ggplot2::element_text(size = 10,
                hjust = 0, vjust = 1)) + ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(fill = NA))) +
            ggplot2::labs(shape = "Judgement", colour = "Judgement")  # Need to be exactly the same
    }

# ROB-2 Cluster=================================================================

    if (tool == "ROB2-Cluster") {

      for (i in 2:8) {
        data[[i]] <- gsub('\\b(\\pL)\\pL{1,}|.','\\U\\1',data[[i]],perl = TRUE)
        data[[i]] <- tolower(data[[i]])
        data[[i]] <- trimws(data[[i]])
        data[[i]] <- ifelse(data[[i]]%in%c("na","n")|is.na(data[[i]]),"x",data[[i]])
        data[[i]] <- substr(data[[i]], 0, 1)
      }

      data.tmp <- data[, c(1:8)]
      if (NCOL(data.tmp) < 7) {
        stop("Column missing (number of columns < 8).")
      }
      names(data.tmp)[1] <- "Study"
      names(data.tmp)[2] <- "D1"
      names(data.tmp)[3] <- "D1b"
      names(data.tmp)[4] <- "D2"
      names(data.tmp)[5] <- "D3"
      names(data.tmp)[6] <- "D4"
      names(data.tmp)[7] <- "D5"
      names(data.tmp)[8] <- "Overall"

      rob.tidy <- suppressWarnings(tidyr::gather(data.tmp,
                                                 domain, judgement, -Study))

      ssize <- psize - (psize/4)

      rob.tidy$Study <- factor(rob.tidy$Study, levels = unique(data.tmp$Study))

      rob.tidy$judgement <- as.factor(rob.tidy$judgement)

      rob.tidy$judgement <- factor(rob.tidy$judgement, levels = c("h", "s", "l", "n", "x"))

      adjust_caption <- -0.7 + length(unique(rob.tidy$judgement))*-0.6

      trafficlightplot <- ggplot2::ggplot(rob.tidy, ggplot2::aes(x = 1,
                                                                 y = 1, colour = judgement)) +
        ggplot2::facet_grid(Study ~
                              factor(domain, levels = c(
                                "D1", "D1b", "D2", "D3", "D4",
                                "D5", "Overall"
                              )),
                            switch = "y",
                            space = "free") +
        ggplot2::geom_point(size = 6) + ggplot2::geom_point(size = 4,
                                                            colour = "black", ggplot2::aes(shape = judgement)) +
        ggplot2::geom_rect(data = rob.tidy[which(rob.tidy$domain !=
                                                   "Overall"), ], fill = "#ffffff", xmin = -Inf,
                           xmax = Inf, ymin = -Inf, ymax = Inf, show.legend = FALSE) +
        ggplot2::geom_rect(data = rob.tidy[which(rob.tidy$domain ==
                                                   "Overall"), ], fill = "#d3d3d3", xmin = -Inf,
                           xmax = Inf, ymin = -Inf, ymax = Inf, show.legend = FALSE) +
        ggplot2::geom_point(size = psize, show.legend = FALSE) +
        ggplot2::geom_point(data = rob.tidy[which(rob.tidy$judgement !=
                                                    "x"), ],shape = 1, colour = "black",
                            size = psize, show.legend = FALSE) +
        ggplot2::geom_point(size = ssize,
                            colour = "black", ggplot2::aes(shape = judgement),
                            show.legend = FALSE) +
        ggplot2::labs(caption = "  Domains:
  D1 :  Bias arising from the randomization process
  D1b: Bias arising from the timing of identification
          and recruitment of Individual participants in
          relation to timing of randomization
  D2 :  Bias due to deviations from intended intervention.
  D3 :  Bias due to missing outcome data.
  D4 :  Bias in measurement of the outcome.
  D5 :  Bias in selection of the reported result.
                ") +
        ggplot2::scale_x_discrete(position = "top", name = "Risk of bias domains") +
        ggplot2::scale_y_continuous(limits = c(1, 1), labels = NULL,
                                    breaks = NULL, name = "Study", position = "left") +
        ggplot2::scale_colour_manual(values = c(h = high_colour,
                                                s = concerns_colour, l = low_colour, n= ni_colour, x = na_colour), labels = c(h = "High",
                                                                                                                              s = "Some concerns", l = "Low", n= "No information", x = "Not applicable")) +
        ggplot2::scale_shape_manual(values = c(h = 120,
                                               s = 45, l = 43, n = 63, x = 32), labels = c(h = "High", s = "Some concerns",
                                                                                           l = "Low", n= "No information", x = "Not applicable")) +
        ggplot2::scale_size(range = c(5,
                                      20)) + ggplot2::theme_bw() +
        ggplot2::theme(panel.border = ggplot2::element_rect(colour = "grey"),
                       panel.spacing = ggplot2::unit(0, "line"), legend.position = "bottom",
                       legend.justification = "right", legend.direction = "vertical",
                       legend.margin = ggplot2::margin(t = -0.2, r = 0,
                                                       b = adjust_caption, l = -10, unit = "cm"),
                       strip.text.x = ggplot2::element_text(size = 10),
                       strip.text.y.left = ggplot2::element_text(angle = 0,
                                                                 size = 10), legend.text = ggplot2::element_text(size = 9),
                       legend.title = ggplot2::element_text(size = 9),
                       strip.background = ggplot2::element_rect(fill = "#a9a9a9"),
                       plot.caption = ggplot2::element_text(size = 10,
                                                            hjust = 0, vjust = 1)) + ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(fill = NA))) +
        ggplot2::labs(shape = "Judgement", colour = "Judgement")  # Need to be exactly the same
    }



# ROBINS-I======================================================================

    if (tool == "ROBINS-I") {

        for (i in 2:9) {
          data[[i]] <- gsub('\\b(\\pL)\\pL{1,}|.','\\U\\1',data[[i]],perl = TRUE)
          data[[i]] <- tolower(data[[i]])
          data[[i]] <- trimws(data[[i]])
          data[[i]] <- ifelse(data[[i]]%in%c("na","n")|is.na(data[[i]]),"x",data[[i]])
          data[[i]] <- substr(data[[i]], 0, 1)
        }

        data.tmp <- data[, c(1:9)]
        if (NCOL(data.tmp) < 9) {
            stop("Column missing (number of columns < 9).")
        }
        names(data.tmp)[1] <- "Study"
        names(data.tmp)[2] <- "D1"
        names(data.tmp)[3] <- "D2"
        names(data.tmp)[4] <- "D3"
        names(data.tmp)[5] <- "D4"
        names(data.tmp)[6] <- "D5"
        names(data.tmp)[7] <- "D6"
        names(data.tmp)[8] <- "D7"
        names(data.tmp)[9] <- "Overall"

        rob.tidy <- suppressWarnings(tidyr::gather(data.tmp,
            domain, judgement, -Study))

        ssize <- psize - (psize/4)



        rob.tidy$Study <- factor(rob.tidy$Study, levels = unique(data.tmp$Study))

        rob.tidy$judgement <- as.factor(rob.tidy$judgement)

        rob.tidy$judgement <- factor(rob.tidy$judgement, levels = c("c","s", "m", "l", "n","x"))

        adjust_caption <- -0.7 + (length(unique(rob.tidy$judgement))*-0.6)

        trafficlightplot <- ggplot2::ggplot(rob.tidy, ggplot2::aes(x = 1,
            y = 1, colour = judgement)) + ggplot2::facet_grid(Study ~
            factor(domain, levels = c("D1", "D2", "D3", "D4",
                "D5", "D6", "D7", "Overall")), switch = "y",
            space = "free") + ggplot2::geom_point(size = 6) +
            ggplot2::geom_point(size = 4, colour = "black",
                ggplot2::aes(shape = judgement)) + ggplot2::geom_rect(data = rob.tidy[which(rob.tidy$domain !=
            "Overall"), ], fill = "#ffffff", xmin = -Inf, xmax = Inf,
            ymin = -Inf, ymax = Inf, show.legend = FALSE) +
            ggplot2::geom_rect(data = rob.tidy[which(rob.tidy$domain ==
                "Overall"), ], fill = "#d3d3d3", xmin = -Inf,
                xmax = Inf, ymin = -Inf, ymax = Inf, show.legend = FALSE) +

        ggplot2::geom_point(size = psize, show.legend = FALSE) +
            ggplot2::geom_point(shape = 1, colour = "black",
                size = psize, show.legend = FALSE) + ggplot2::geom_point(size = ssize,
            colour = "black", ggplot2::aes(shape = judgement),
            show.legend = FALSE) + ggplot2::labs(caption = "  Domains:
  D1: Bias due to confounding.
  D2: Bias due to selection of participants.
  D3: Bias in classification of interventions.
  D4: Bias due to deviations from intended interventions.
  D5: Bias due to missing data.
  D6: Bias in measurement of outcomes.
  D7: Bias in selection of the reported result.


                  ") +
            ggplot2::scale_x_discrete(position = "top", name = "Risk of bias domains") +
            ggplot2::scale_y_continuous(limits = c(1, 1), labels = NULL,
                breaks = NULL, name = "Study", position = "left") +
            ggplot2::scale_colour_manual(values = c(c = critical_colour,
                s = high_colour, m = concerns_colour, l = low_colour, n = ni_colour, x=na_colour),
                labels = c(c = "Critical", s = "Serious", m = "Moderate",
                  l = "Low", n = "No information",x = "Not applicable")) + ggplot2::scale_shape_manual(values = c(c = 33,
            s = 120, m = 45, l = 43, n =63, x=32), labels = c(c = "Critical",
            s = "Serious", m = "Moderate", l = "Low",n = "No information",x = "Not applicable")) + ggplot2::scale_size(range = c(5,
            20)) + ggplot2::theme_bw() +
          ggplot2::theme(panel.border = ggplot2::element_rect(colour = "grey"),
            panel.spacing = ggplot2::unit(0, "line"), legend.position = "bottom",
            legend.justification = "right", legend.direction = "vertical",
            legend.margin = ggplot2::margin(t = -0.2, r = 0,
                b = adjust_caption, l = -10, unit = "cm"),
            strip.text.x = ggplot2::element_text(size = 10),
            strip.text.y.left = ggplot2::element_text(angle = 0,
                size = 10), legend.text = ggplot2::element_text(size = 9),
            legend.title = ggplot2::element_text(size = 9),
            strip.background = ggplot2::element_rect(fill = "#a9a9a9"),
            plot.caption = ggplot2::element_text(size = 10,
                hjust = 0, vjust = 1)) + ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(fill = NA))) +
            ggplot2::labs(shape = "Judgement", colour = "Judgement")  # Need to be exactly the same
    }


# ROBINS-I-ONLINE===============================================================

    if (tool == "ROBINS-I Online") {

        data <- data[, grepl("studyId|RBJ_answer", names(data))]
        data <- data[, colSums(is.na(data)) != nrow(data)]

        for (i in 2:9) {
          data[[i]] <- gsub('\\b(\\pL)\\pL{1,}|.','\\U\\1',data[[i]],perl = TRUE)
          data[[i]] <- tolower(data[[i]])
          data[[i]] <- trimws(data[[i]])
          data[[i]] <- ifelse(data[[i]]%in%c("na","n")|is.na(data[[i]]),"x",data[[i]])
          data[[i]] <- substr(data[[i]], 0, 1)
        }

        data.tmp <- data[, c(1:9)]
        if (NCOL(data.tmp) < 9) {
            stop("Column missing (number of columns < 9).")
        }
        names(data.tmp)[1] <- "Study"
        names(data.tmp)[2] <- "D1"
        names(data.tmp)[3] <- "D2"
        names(data.tmp)[4] <- "D3"
        names(data.tmp)[5] <- "D4"
        names(data.tmp)[6] <- "D5"
        names(data.tmp)[7] <- "D6"
        names(data.tmp)[8] <- "D7"
        names(data.tmp)[9] <- "Overall"


        rob.tidy <- suppressWarnings(tidyr::gather(data.tmp,
            domain, judgement, -Study))

        ssize <- psize - (psize/4)



        rob.tidy$Study <- factor(rob.tidy$Study, levels = unique(data.tmp$Study))

        rob.tidy$judgement <- as.factor(rob.tidy$judgement)

        rob.tidy$judgement <- factor(rob.tidy$judgement, levels = c("c", "s", "m", "l", "n","x"))

        adjust_caption <- -0.7 + length(unique(rob.tidy$judgement))*-0.6

        trafficlightplot <- ggplot2::ggplot(rob.tidy, ggplot2::aes(x = 1,
            y = 1, colour = judgement)) + ggplot2::facet_grid(Study ~
            factor(domain, levels = c("D1", "D2", "D3", "D4",
                "D5", "D6", "D7", "Overall")), switch = "y",
            space = "free") + ggplot2::geom_point(size = 6) +
            ggplot2::geom_point(size = 4, colour = "black",
                ggplot2::aes(shape = judgement)) + ggplot2::geom_rect(data = rob.tidy[which(rob.tidy$domain !=
            "Overall"), ], fill = "#ffffff", xmin = -Inf, xmax = Inf,
            ymin = -Inf, ymax = Inf, show.legend = FALSE) +
            ggplot2::geom_rect(data = rob.tidy[which(rob.tidy$domain ==
                "Overall"), ], fill = "#d3d3d3", xmin = -Inf,
                xmax = Inf, ymin = -Inf, ymax = Inf, show.legend = FALSE) +

        ggplot2::geom_point(size = psize, show.legend = FALSE) +
            ggplot2::geom_point(shape = 1, colour = "black",
                size = psize, show.legend = FALSE) + ggplot2::geom_point(size = ssize,
            colour = "black", ggplot2::aes(shape = judgement),
            show.legend = FALSE) + ggplot2::labs(caption = "  Domains:
  D1: Bias due to confounding.
  D2: Bias due to selection of participants.
  D3: Bias in classification of interventions.
  D4: Bias due to deviations from intended interventions.
  D5: Bias due to missing data.
  D6: Bias in measurement of outcomes.
  D7: Bias in selection of the reported result.


                  ") +
            ggplot2::scale_x_discrete(position = "top", name = "Risk of bias domains") +
            ggplot2::scale_y_continuous(limits = c(1, 1), labels = NULL,
                breaks = NULL, name = "Study", position = "left") +
            ggplot2::scale_colour_manual(values = c(c = critical_colour,
                s = high_colour, m = concerns_colour, l = low_colour, x=na_colour),
                labels = c(c = "Critical", s = "Serious", m = "Moderate",
                  l = "Low",x = "Not applicable")) + ggplot2::scale_shape_manual(values = c(c = 33,
            s = 120, m = 45, l = 43, x = 32), labels = c(c = "Critical",
            s = "Serious", m = "Moderate", l = "Low", x = "Not applicable")) + ggplot2::scale_size(range = c(5,
            20)) + ggplot2::theme_bw() + ggplot2::theme(panel.border = ggplot2::element_rect(colour = "grey"),
            panel.spacing = ggplot2::unit(0, "line"), legend.position = "bottom",
            legend.justification = "right", legend.direction = "vertical",
            legend.margin = ggplot2::margin(t = -0.2, r = 0,
                b = adjust_caption, l = -10, unit = "cm"),
            strip.text.x = ggplot2::element_text(size = 10),
            strip.text.y.left = ggplot2::element_text(angle = 0,
                size = 10), legend.text = ggplot2::element_text(size = 9),
            legend.title = ggplot2::element_text(size = 9),
            strip.background = ggplot2::element_rect(fill = "#a9a9a9"),
            plot.caption = ggplot2::element_text(size = 10,
                hjust = 0, vjust = 1)) + ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(fill = NA))) +
            ggplot2::labs(shape = "Judgement", colour = "Judgement")  # Need to be exactly the same
    }

# QUADAS-2======================================================================

    if (tool == "QUADAS-2") {

        for (i in 2:6) {
          data[[i]] <- gsub('\\b(\\pL)\\pL{1,}|.','\\U\\1',data[[i]],perl = TRUE)
          data[[i]] <- tolower(data[[i]])
          data[[i]] <- trimws(data[[i]])
          data[[i]] <- ifelse(data[[i]]%in%c("na","n")|is.na(data[[i]]),"x",data[[i]])
          data[[i]] <- substr(data[[i]], 0, 1)
        }

        data.tmp <- data[, c(1:6)]
        if (NCOL(data.tmp) < 6) {
            stop("Column missing (number of columns < 6).")
        }
        names(data.tmp)[1] <- "Study"
        names(data.tmp)[2] <- "D1"
        names(data.tmp)[3] <- "D2"
        names(data.tmp)[4] <- "D3"
        names(data.tmp)[5] <- "D4"
        names(data.tmp)[6] <- "Overall"

        rob.tidy <- suppressWarnings(tidyr::gather(data.tmp,
            domain, judgement, -Study))

        ssize <- psize - (psize/4)

        rob.tidy$Study <- factor(rob.tidy$Study, levels = unique(data.tmp$Study))

        rob.tidy$judgement <- as.factor(rob.tidy$judgement)

        rob.tidy$judgement <- factor(rob.tidy$judgement, levels = c("h", "s", "l", "n", "x"))

        adjust_caption <- -0.7 + length(unique(rob.tidy$judgement))*-0.6

        trafficlightplot <- ggplot2::ggplot(rob.tidy, ggplot2::aes(x = 1,
            y = 1, colour = judgement)) + ggplot2::facet_grid(Study ~
            factor(domain, levels = c("D1", "D2", "D3", "D4",
                "Overall")), switch = "y", space = "free") +
            ggplot2::geom_point(size = 6) + ggplot2::geom_point(size = 4,
            colour = "black", ggplot2::aes(shape = judgement)) +
            ggplot2::geom_rect(data = rob.tidy[which(rob.tidy$domain !=
                "Overall"), ], fill = "#ffffff", xmin = -Inf,
                xmax = Inf, ymin = -Inf, ymax = Inf, show.legend = FALSE) +
            ggplot2::geom_rect(data = rob.tidy[which(rob.tidy$domain ==
                "Overall"), ], fill = "#d3d3d3", xmin = -Inf,
                xmax = Inf, ymin = -Inf, ymax = Inf, show.legend = FALSE) +
            ggplot2::geom_point(size = psize, show.legend = FALSE) +
            ggplot2::geom_point(shape = 1, colour = "black",
                size = psize, show.legend = FALSE) + ggplot2::geom_point(size = ssize,
            colour = "black", ggplot2::aes(shape = judgement),
            show.legend = FALSE) + ggplot2::labs(caption = "  Domains:
  D1: Patient selection.
  D2: Index test.
  D3: Reference standard.
  D4: Flow & timing.


                  ") +
            ggplot2::scale_x_discrete(position = "top", name = "Risk of bias domains") +
            ggplot2::scale_y_continuous(limits = c(1, 1), labels = NULL,
                breaks = NULL, name = "Study", position = "left") +
            ggplot2::scale_colour_manual(values = c(h = high_colour,
                s = concerns_colour, l = low_colour, n = ni_colour, x=na_colour), labels = c(h = "High",
                s = "Some concerns", l = "Low", n = "No information", x = "Not applicable")) + ggplot2::scale_shape_manual(values = c(h = 120,
            s = 45, l = 43, n= 63, x=32), labels = c(h = "High", s = "Some concerns",
            l = "Low", n = "No information", x = "Not applicable")) + ggplot2::scale_size(range = c(5,
            20)) + ggplot2::theme_bw() + ggplot2::theme(panel.border = ggplot2::element_rect(colour = "grey"),
            panel.spacing = ggplot2::unit(0, "line"), legend.position = "bottom",
            legend.justification = "right", legend.direction = "vertical",
            legend.margin = ggplot2::margin(t = -0.2, r = 0,
                b = adjust_caption, l = -10, unit = "cm"),
            strip.text.x = ggplot2::element_text(size = 10),
            strip.text.y.left = ggplot2::element_text(angle = 0,
                size = 10), legend.text = ggplot2::element_text(size = 9),
            legend.title = ggplot2::element_text(size = 9),
            strip.background = ggplot2::element_rect(fill = "#a9a9a9"),
            plot.caption = ggplot2::element_text(size = 10,
                hjust = 0, vjust = 1)) + ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(fill = NA))) +
            ggplot2::labs(shape = "Judgement", colour = "Judgement")  # Need to be exactly the same

    }



# ROB-1/Generic=================================================================

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
        if (unique(grepl("^[-]{0,1}[0-9]{0,}.{0,1}[0-9]{1,}$",
            data[[ncol(data)]])) == TRUE) {
            for (i in 2:(ncol(data) - 1)) {
              data[[i]] <- gsub('\\b(\\pL)\\pL{1,}|.','\\U\\1',data[[i]],perl = TRUE)
              data[[i]] <- tolower(data[[i]])
              data[[i]] <- trimws(data[[i]])
              data[[i]] <- ifelse(data[[i]]%in%c("na","n")|is.na(data[[i]]),"x",data[[i]])
              data[[i]] <- substr(data[[i]], 0, 1)
              data[[i]] <- gsub("u", "s", data[[i]])
              data[[i]] <- gsub("m", "s", data[[i]])
            }

            # Select relevant columns, excluding the 'Weights' column
            data <- data[, c(1:(ncol(data) - 1))]
        } else {
            for (i in 2:(ncol(data))) {
              data[[i]] <- gsub('\\b(\\pL)\\pL{1,}|.','\\U\\1',data[[i]],perl = TRUE)
              data[[i]] <- tolower(data[[i]])
              data[[i]] <- trimws(data[[i]])
              data[[i]] <- ifelse(data[[i]]=="na","x",data[[i]])
              data[[i]] <- substr(data[[i]], 0, 1)
              data[[i]] <- gsub("u", "s", data[[i]])
              data[[i]] <- gsub("m", "s", data[[i]])
            }
        }

        data.tmp <- data

        # Remove dots from column names
        for (i in 1:(ncol(data.tmp))) {
            names(data.tmp)[i] <- invisible(gsub(".", " ",
                names(data.tmp)[i], fixed = TRUE))
        }

        # Create caption vector, and add line breaks to maintain spacing
        captiondf <- data.frame(V1 = rep("",8), stringsAsFactors = FALSE)
        for (i in 2:(ncol(data.tmp) - 1)) {
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
        for (i in 2:(ncol(data.tmp) - 1)) {
            names(data.tmp)[i] <- paste0("D", i - 1)
        }

        # Convert to long format
        rob.tidy <- suppressWarnings(tidyr::gather(data.tmp,
            domain, judgement, -Study))

        # Relevel rob.tidy$domain
        for (i in 1:(ncol(data.tmp))) {
            levels(rob.tidy$domain)[i] <- colnames(data.tmp)[i]
        }

        rob.tidy$domain <- factor(rob.tidy$domain, levels = levels(rob.tidy$domain))

        rob.tidy$Study <- factor(rob.tidy$Study, levels = unique(data.tmp$Study))

        rob.tidy$judgement <- as.factor(rob.tidy$judgement)

        rob.tidy$judgement <- factor(rob.tidy$judgement, levels = c("c", "h", "s", "l", "n","x"))

        adjust_caption <- -0.7 + length(unique(rob.tidy$judgement))*-0.6

        # Set sizes
        ssize <- psize - (psize/4)

        # PLot graph
        trafficlightplot <- ggplot2::ggplot(rob.tidy, ggplot2::aes(x = 1,
            y = 1, colour = judgement)) + ggplot2::facet_grid(Study ~
            factor(domain), switch = "y", space = "free") +
            ggplot2::geom_point(size = 6) + ggplot2::geom_point(size = 4,
            colour = "black", ggplot2::aes(shape = judgement)) +
            ggplot2::geom_rect(data = rob.tidy[which(rob.tidy$domain !=
                "Overall"), ], fill = "#ffffff", xmin = -Inf,
                xmax = Inf, ymin = -Inf, ymax = Inf, show.legend = FALSE) +
            ggplot2::geom_rect(data = rob.tidy[which(rob.tidy$domain ==
                "Overall"), ], fill = "#d3d3d3", xmin = -Inf,
                xmax = Inf, ymin = -Inf, ymax = Inf, show.legend = FALSE) +
            ggplot2::geom_point(size = psize, show.legend = FALSE) +
            ggplot2::geom_point(shape = 1, colour = "black",
                size = psize, show.legend = FALSE) + ggplot2::geom_point(size = ssize,
            colour = "black", ggplot2::aes(shape = judgement),
            show.legend = FALSE) + ggplot2::labs(caption = caption) +
            ggplot2::scale_x_discrete(position = "top", name = "Risk of bias domains") +
            ggplot2::scale_y_continuous(limits = c(1, 1), labels = NULL,
                breaks = NULL, name = "Study", position = "left") +
            ggplot2::scale_colour_manual(values = c(l = low_colour,
                s = concerns_colour, h = high_colour, c = critical_colour, n = ni_colour, x=na_colour),
                labels = c(l = "Low", s = "Unclear", h = "High",
                  c = "Critical", n = "No information", x = "Not applicable")) + ggplot2::scale_shape_manual(values = c(l = 43,
            s = 45, h = 120, c = 33, n= 63, x = 32), labels = c(l = "Low",
            s = "Unclear", h = "High", c = "Critical", n="No information",x = "Not applicable")) + ggplot2::scale_size(range = c(5,
            20)) + ggplot2::theme_bw() + ggplot2::theme(panel.border = ggplot2::element_rect(colour = "grey"),
            panel.spacing = ggplot2::unit(0, "line"), legend.position = "bottom",
            legend.justification = "right", legend.direction = "vertical",
            legend.margin = ggplot2::margin(t = -0.2, r = 0,
                b = adjust_caption, l = -10, unit = "cm"),
            strip.text.x = ggplot2::element_text(size = 10),
            strip.text.y.left = ggplot2::element_text(angle = 0,
                size = 10), legend.text = ggplot2::element_text(size = 9),
            legend.title = ggplot2::element_text(size = 9),
            strip.background = ggplot2::element_rect(fill = "#a9a9a9"),
            plot.caption = ggplot2::element_text(size = 10,
                hjust = 0, vjust = 1)) + ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(fill = NA))) +
            ggplot2::labs(shape = "Judgement", colour = "Judgement")
    }

# Return-plot===================================================================

    if (quiet != TRUE) {
        return(trafficlightplot)
    }

}
