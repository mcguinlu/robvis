rob_summary_plot <- function(data, tool, save="FALSE"){
  rob.df <- data.frame(data)

  rob.df$Study <- NULL

  ncol.rob.df<-ncol(rob.df)

  last<-colnames(rob.df[ncol.rob.df])

  first<-colnames(rob.df[1])

  rob.long <- suppressWarnings(tidyr::gather(data,
                     domain, judgement,
                     first:last,
                     factor_key=FALSE))

  if(tool=="ROB2"){
  rob.long<- dplyr::mutate(rob.long, domain_renamed=dplyr::case_when(domain=="D1"~"Bias due to randomisation",
                                            domain=="D2"~"Bias due to deviations from intended intervention",
                                            domain=="D3"~"Bias due to missing data",
                                            domain=="D4"~"Bias due to outcome measurement",
                                            domain=="D5"~"Bias due to selection of reported result",
                                            TRUE ~ domain)  )

  rob.long$judgement<-as.factor(rob.long$judgement)

  rob.long$domain_renamed<-as.factor(rob.long$domain_renamed)

  rob.long$domain_renamed<-factor(rob.long$domain_renamed,
                                  levels(rob.long$domain_renamed)[c(5,2,3,1,4)])


  rob.long$judgement<-factor(rob.long$judgement,
                               levels(rob.long$judgement)[c(1,3,2)])

  plot<-ggplot2::ggplot(data=rob.long)+
    geom_bar(mapping=aes(x=domain_renamed,fill=judgement),
             width=0.7,
             position = "fill",
             color="black")+
    coord_flip(ylim = c(0,1))+
    guides(fill = guide_legend(reverse = TRUE))+
    scale_fill_manual("Risk of Bias",
                      values = c("Low" = "#66c2a5",
                                 "Some concerns" = "#808080",
                                 "High" = "#fc8d62"),
                      labels = c("    High risk of bias  ",
                                 "    Some concerns       ",
                                 "    Low risk of bias          "))+
    scale_y_continuous(labels = scales::percent)+
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.y = element_text(size=10, color = "black"),
          axis.line.x = element_line(colour = "black",
                                     size = 0.5, linetype = "solid"),
          legend.position = "bottom",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          legend.background = element_rect(linetype="solid",
                                           colour ="black"),
          legend.title = element_blank(),
          legend.key.size = unit(0.75,"cm"),
          legend.text=element_text(size=14))
  }

  if(tool=="ROBINS-I"){
    rob.long<- dplyr::mutate(rob.long, domain_renamed=dplyr::case_when(domain=="D1"~"Bias due to confounding",
                                                         domain=="D2"~"Bias due to selection of participants",
                                                         domain=="D3"~"Bias in classification of interventions",
                                                         domain=="D4"~"Bias due to deviations from intended interventions",
                                                         domain=="D5"~"Bias due to missing data",
                                                         domain=="D6"~"Bias in measurement of outcomes",
                                                         domain=="D7"~"Bias in selection of the reported result",
                                                         TRUE ~ domain)  )

    rob.long$judgement<-as.factor(rob.long$judgement)
    rob.long$domain_renamed<-as.factor(rob.long$domain_renamed)

    rob.long$domain_renamed<-factor(rob.long$domain_renamed,
                                    levels(rob.long$domain_renamed)[c(7,6,3,5,2,4,1)])

    rob.long$judgement<-factor(rob.long$judgement,
                               levels(rob.long$judgement)[c(1,2,4,3)])

    plot<-ggplot2::ggplot(data=rob.long)+
      geom_bar(mapping=aes(x=domain_renamed,fill=judgement),
               width=0.7,
               position = "fill",
               color="black")+
      coord_flip(ylim = c(0,1))+
      guides(fill = guide_legend(reverse = TRUE))+
      scale_fill_manual("Risk of Bias",
                        values = c(
                          "High" = "#fc8d62",
                          "Some concerns" = "#808080",
                          "Low" = "#66c2a5",
                          "Critical" = "#ff0000"),
                        labels = c(
                          "    Critical risk of bias",
                          "    High risk of bias  ",
                          "    Some concerns       ",
                          "    Low risk of bias          "))+
      scale_y_continuous(labels = scales::percent)+
      theme(axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.text.y = element_text(size=12, color = "black"),
            axis.line.x = element_line(colour = "black",
                                       size = 0.5, linetype = "solid"),
            legend.position = "bottom",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            legend.background = element_rect(linetype="solid",
                                             colour ="black"),
            legend.title = element_blank(),
            legend.key.size = unit(0.75,"cm"),
            legend.text=element_text(size=14))
  }

  if(tool=="QUADAS-2"){
  rob.long<- dplyr::mutate(rob.long, domain_renamed=dplyr::case_when(domain=="D1"~"Patient Selection",
                                                                     domain=="D2"~"Index Test",
                                                                     domain=="D3"~"Reference Standard",
                                                                     domain=="D4"~"Flow & Timing",
                                                                     TRUE ~ domain))

  rob.long$judgement<-as.factor(rob.long$judgement)
  rob.long$domain_renamed<-as.factor(rob.long$domain_renamed)

  rob.long$judgement<-factor(rob.long$judgement,
                             levels(rob.long$judgement)[c(3,2,1)])

  #forcats to relevel -> fix tomorrow.

  rob.long$domain_renamed<-factor(rob.long$domain_renamed,
                             levels(rob.long$domain_renamed)[c(1,4,2,3)])


  plot<-ggplot2::ggplot(data=rob.long)+
    geom_bar(mapping=aes(x=domain_renamed,fill=judgement),
             width=0.7,
             position = "fill",
             color="black")+
    coord_flip(ylim = c(0,1))+
    guides(fill = guide_legend(reverse = TRUE))+
    scale_fill_manual("Risk of Bias",
                      values = c(
                        "High" = "#fc8d62",
                        "Some concerns" = "#808080",
                        "Low" = "#66c2a5"),
                      labels = c(
                        "    High risk of bias  ",
                        "    Some concerns       ",
                        "    Low risk of bias          "))+
    scale_y_continuous(labels = scales::percent)+
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.y = element_text(size=12, color = "black"),
          axis.line.x = element_line(colour = "black",
                                     size = 0.5, linetype = "solid"),
          legend.position = "bottom",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          legend.background = element_rect(linetype="solid",
                                           colour ="black"),
          legend.title = element_blank(),
          legend.key.size = unit(0.75,"cm"),
          legend.text=element_text(size=14))
  }

  if(save=="TRUE"){
    ggplot2::ggsave("rob_summary_plot.png", width = 14)
  }

  return(plot)
}

