#libraries
library(ggplot2)
library(tidyverse)
library(magrittr)

rob <- read.csv("data/data_rob.csv")

str(rob)

rob

#We need to make this long data

rob.long <- gather(rob, key = "Domain", value = "Levelofconcern", -Study)
rob.long %<>% mutate(cex = rep(5, nrow(rob.long)),
                     `Levelofconcern` = as.factor(`Levelofconcern`))

vec.study.names <- rob.long$Study %>% unique %>% as.character()

rob.long$Study <- factor(rob.long$Study, levels = vec.study.names)

rob.long$Levelofconcern = factor(rob.long$Levelofconcern, levels = c('Low','High','Some concerns'))

#Lets try this
 ggplot(rob.long, aes(x = 1, y = 1)) +
  facet_grid(Study ~ factor(Domain,levels=c("D1", "D2", "D3", "D4", "D5")), switch = "y", drop = TRUE) +
  geom_point(aes(size = cex, colour = Levelofconcern)) +
  geom_point(shape = 1, colour = "black", aes(size = cex)) +
  scale_x_discrete(position = "top", name = "Domains") +
  scale_y_continuous(limits = c(1, 1), labels = NULL, breaks = NULL, name = "Study", position = "left") +
 scale_colour_brewer(palette = "Dark2") +
  scale_size_continuous(range = c(5,20)) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "grey"),
        panel.spacing = unit(0, "line"),
        legend.position = "none")
