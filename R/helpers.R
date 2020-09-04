get_colour <- function(tool, colour){

  rob_colours <- c()

  # Define colours
  rob_colours$na_colour <- "#cccccc"

  if (tool == "ROB2" || tool == "ROB2-Cluster" || tool == "QUADAS-2") {
    if (length(colour) > 1) {
      rob_colours$low_colour <- colour[c(1)]
      rob_colours$concerns_colour <- colour[c(2)]
      rob_colours$high_colour <- colour[c(3)]
      rob_colours$ni_colour <- colour[c(4)]
    } else {
      if (colour == "colourblind") {
        rob_colours$low_colour <- "#fed98e"
        rob_colours$concerns_colour <- "#fe9929"
        rob_colours$high_colour <- "#d95f0e"
        rob_colours$ni_colour <- "#ffffff"
      }
      if (colour == "cochrane") {
        rob_colours$low_colour <- "#02C100"
        rob_colours$concerns_colour <- "#E2DF07"
        rob_colours$high_colour <- "#BF0000"
        rob_colours$ni_colour <- "#4EA1F7"
      }
    }
  }else{
    if (length(colour) > 1) {
      rob_colours$low_colour <- colour[c(1)]
      rob_colours$concerns_colour <- colour[c(2)]
      rob_colours$high_colour <- colour[c(3)]
      rob_colours$critical_colour <- colour[c(4)]
      rob_colours$ni_colour <- colour[c(5)]
    } else {
      if (colour == "colourblind") {
        rob_colours$low_colour <- "#fed98e"
        rob_colours$concerns_colour <- "#fe9929"
        rob_colours$high_colour <- "#d95f0e"
        rob_colours$critical_colour <- "#993404"
        rob_colours$ni_colour <- "#ffffff"
      }
      if (colour == "cochrane") {
        rob_colours$low_colour <- "#02C100"
        rob_colours$concerns_colour <- "#E2DF07"
        rob_colours$high_colour <- "#BF0000"
        rob_colours$critical_colour <- "#820000"
        rob_colours$ni_colour <- "#4EA1F7"
      }
    }
  }

  return(rob_colours)
}

get_height <- function(data, tool, psize = 15, type = "tf"){
  if (type == "tf") {
    tool_adj <- ifelse(tool %in% c("ROBINS-I","Generic"), 2.5, 2)
    nrows <- nrow(data)
    height <- tool_adj + nrows * .75 / (15 / psize)
  } else {
    height <- 2.41
  }
  return(height)
}

get_width <- function(data, type = "tf"){
  # Account for long study names
  if (type == "tf") {
    nchar_study <- max(nchar(as.character(data$Study)))
    nchar_domain <- max(nchar(as.character(colnames(data)))) + 3
    width_adj <- ifelse(nchar_study > 8, 8+nchar_study*0.05, 8)
    width <- ifelse(nchar_domain > 42, width_adj+(nchar_domain-42)*0.05, width_adj)
  } else {
    width <- 8
  }
  return(width)
}
