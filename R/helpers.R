
rob_tf_theme <-function(adjust_caption, judgement_label = "Judgement"){
  list(
  ggplot2::theme_bw(),
    ggplot2::theme(
      panel.border = ggplot2::element_rect(colour = "grey"),
      panel.spacing = ggplot2::unit(0, "line"), legend.position = "bottom",
      legend.justification = "right", legend.direction = "vertical",
      legend.margin = ggplot2::margin(
        t = -0.2, r = 0,
        b = adjust_caption, l = -10, unit = "cm"
      ),
      strip.text.x = ggplot2::element_text(size = 10),
      strip.text.y.left = ggplot2::element_text(
        angle = 0,
        size = 10
      ), legend.text = ggplot2::element_text(size = 9),
      legend.title = ggplot2::element_text(size = 9),
      strip.background = ggplot2::element_rect(fill = "#a9a9a9"),
      plot.caption = ggplot2::element_text(
        size = 10,
        hjust = 0, vjust = 1
      )
    ),
    ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(fill = NA))),
    ggplot2::labs(shape = judgement_label, colour = judgement_label) # Need to be exactly the same

)
}


check_data <- function(data){

  header <- stringr::str_to_lower(data[1,])

  if (any(c("study","overall", "weight") %in% header)) {
    stop(paste("It looks like the first row of your dataset contains column",
               "headings (e.g. \"Study\", \"Overall\"). Did you set ",
               "\"header = TRUE\" when reading in your data?")
    )
  }

}

# Check colours
check_colour <- function(tool, colour) {
  if(!(colour[1] %in% c("cochrane","colourblind"))){
    if (tool == "ROB2" || tool == "ROB2-Cluster" || tool == "QUADAS-2") {
      if(length(colour)!=4){
        stop("Wrong number of colours specified. This template expects 4 colours.")
      }

    } else{
      if(length(colour)!=5){
        stop("Wrong number of colours specified. This template expects 5 colours.")
      }
    }
  }
}

# Define colours used
get_colour <- function(tool, colour) {
  rob_colours <- c()

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
  } else {
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

# Make sure specified tool is allowed
check_tool <- function(tool) {
  tools <- c(suppressMessages(rob_tools()), "ROB1")

  if ((tool %in% tools) == FALSE) {
    stop(
      paste(
        "\nTool name \"",
        tool,
        "\" not recognised \nAcceptable tools names can be found using the rob_tools() function"
      )
    )
  }
}

# Give depreciated warning if "ROB1" specified
rob1_warning <- function(tool) {
  if (tool == "ROB1") {
    message(
      paste0(
        "Note: In future versions of robvis, the 'tool = \"ROB1\"' ",
        "argument will be depreciated.\n",
        "Please use 'tool = \"Generic\"' instead."
      )
    )
  }
}

# Get recommended height of figure, used by rob_save
get_height <- function(data, tool, psize, type = "tf") {
  if (type == "tf") {
    tool_adj <- ifelse(tool %in% c("ROBINS-I", "Generic"), 2.5, 2)
    nrows <- nrow(data)
    height <- tool_adj + nrows * .4 / (10 / psize)
  } else {
    height <- 2.41
  }
  return(height)
}

# Get recommended width of figure, used by rob_save
get_width <- function(data, psize, type = "tf") {
  if (type == "tf") {
    # Account for long study names
    nchar_study <- max(nchar(as.character(data$Study)))
    nchar_domain <- max(nchar(as.character(colnames(data)))) + 3
    width_adj <- ifelse(nchar_study > 8, 6 + nchar_study * 0.05, 6)
    width <-
      ifelse(nchar_domain > 42,
        width_adj + (nchar_domain - 42) * 0.05,
        width_adj
      )
  } else {
    width <- 8
  }
  return(width)
}

# Allow for US spelling of "colourblind"
weird_spelling <- function(colour) {
  colour <- ifelse(colour == "colorblind", "colourblind", colour)
  return(colour)
}

# Acceptable file extensions for saving

get_extension <- function(file){
  ex <- strsplit(basename(file), split="\\.")[[1]]
  return(ex[-1])
}

check_extension <- function(file){
  if (!(get_extension(file) %in% c("png","jpeg","tiff","eps"))) {
    stop(paste0("Saving to this file type is not supported by robvis. ",
                "Acceptable file types are \".png\", \".jpeg\", ",
                " \".tiff\", and \".eps\". "))
  }
}


clean_data <- function(col) {
  col <- gsub("\\b(\\pL)\\pL{1,}|.", "\\U\\1", col, perl = TRUE)
  col <- trimws(tolower(col))
  col <- ifelse(col %in% c("na", "n") | is.na(col), "x", col)
  col <- substr(col, 0, 1)
  return(col)
}
