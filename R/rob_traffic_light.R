#' Produce traffic-light plots of risk-of-bias assessments.
#' @description A function
#' @param data A dataframe containing summary (domain) level risk-of-bias assessments, with the first column containing the study details, the second column containing the first domain of your assessments, and the final column containing a weight to assign to each study. The function assumes that the data includes a column for overall risk-of-bias. For example, a ROB2.0 dataset would have 8 columns (1 for study details, 5 for domain level judgements, 1 for overall judgements, and 1 for weights, in that order).
#' @param tool The risk of bias assessment tool used. RoB2.0 (tool="ROB2"), ROBINS-I (tool="ROBINS-I"), and QUADAS-2 (tool="QUADAS-2") are currently supported.
#' @param save An option to save the plot as the specified file type. Default is FALSE, and available extensions are ".eps", ".ps", ".tex", ".pdf", ".jpeg", ".tiff", ".png" and ".bmp".
#' @param overall An option to include a bar for overall risk-of-bias in the figure. Default is FALSE.
#' @param quiet An option to quietly produce and save the plot without it displaying in R/Rstudio.
#' @return Risk of bias assessment barplot figure..
#' @export

data.tmp <- data_rob
  if(NCOL(data.tmp) < 8){stop("Column missing (number of columns < 8). Likely that a column detailing weights for each study is missing.")}
  names(data.tmp)[2] <- "Bias due to randomisation"
  names(data.tmp)[3] <- "Bias due to deviations from intended intervention"
  names(data.tmp)[4] <- "Bias due to missing data"
  names(data.tmp)[5] <- "Bias due to outcome measurement"
  names(data.tmp)[6] <- "Bias due to selection of reported result"
  names(data.tmp)[7] <- "Overall risk of bias"
  names(data.tmp)[8] <- "Weight"

  data.tmp <- data.tmp[, c(1:7)]

  rob.tidy <- suppressWarnings(tidyr::gather(data.tmp,
                                            domain, judgement, -Study))

psize <- 3

outcome_graph <-  ggplot(rob.tidy, aes(x=1, y=1, colour = judgement)) +
  facet_grid(Study ~ factor(domain, levels=c("Bias due to randomisation",
                                             "Bias due to deviations from intended intervention",
                                             "Bias due to missing data",
                                             "Bias due to outcome measurement",
                                             "Bias due to selection of reported result",
                                             "Overall risk of bias")), switch = "y", drop = TRUE) +
  geom_point(aes(size = psize)) +
  geom_point(shape = 1, colour = "black", aes(size = psize)) +
  geom_point(aes)
  scale_x_discrete(position = "top", name = "Risk of bias domains") +
  scale_y_continuous(limits = c(1, 1), labels = NULL, breaks = NULL, name = "Study", position = "left") +
  scale_colour_brewer(palette = "RdYlBu") +
  scale_size_continuous(range = c(5,20)) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "grey"),
        panel.spacing = unit(0, "line"),
        legend.position = "none")


outcome_graph
