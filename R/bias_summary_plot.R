bias_summary_plot <- function(data, tool){
  rob.df <- data.frame(data)

  rob.df$Study <- NULL

  ncol.rob.df<-ncol(rob.df)

  last<-colnames(rob.df[ncol.rob.df])

  first<-colnames(rob.df[1])

  rob.long <- suppressWarnings(gather(data,
                     domain, judgement,
                     first:last,
                     factor_key=FALSE))

  if(tool=="ROB2"){

  mutate(rob.long, domain_renamed=case_when(domain==1~"Bias due to randomisation",
                                            domain==2~"Bias due to deviations",
                                            domain==3~"Bias due to missing data",
                                            domain==4~"Bias due to outcome measurement",
                                            domain==5~"Bias due to selection of reported result",
                                            TRUE ~ domain)  )

  rob.long$judgement<-as.factor(rob.long$judgement)

  rob.long$judgement<-factor(rob.long$judgement,
                               levels(rob.long$judgement)[c(1,3,2)])

  plot<-ggplot(data=rob.long)+
    geom_bar(mapping=aes(x=domain,fill=judgement),
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
          axis.text.y = element_text(size=18, color = "black"),
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
    plot<-ggplot(data=rob.long)+
      geom_bar(mapping=aes(x=domain,fill=judgement),
               width=0.7,
               position = "fill",
               color="black")+
      coord_flip(ylim = c(0,1))+
      guides(fill = guide_legend(reverse = TRUE))+
      scale_fill_manual("Risk of Bias",
                        labels = c("    Critical risk of bias      ",
                                   "    High risk of bias          ",
                                   "    Unclear risk of bias       ",
                                   "    Low risk of bias  "),
                        values = c(
                          "Critical" = "#ff0000",
                          "High" = "#BF0000",
                          "Unclear" = "#02C100",
                          "Low" = "#E2DF07"))+
      scale_y_continuous(labels = scales::percent)+
      theme(axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.text.y = element_text(size=18, color = "black"),
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
  return(plot)
}
