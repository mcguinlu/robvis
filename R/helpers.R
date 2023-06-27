# Check inputs =================================================================
# Functions in this section do not return a value
# Instead, they either print an informative (error) message to the console
# Functions in this category should start with the `check_` prefix

# Check that the specified tool is supported
check_tool <- function(tool, forest = FALSE) {

  if (forest) {
    tools <- c(suppressMessages(rob_tools(forest = TRUE)))
    message_content <- "rob_tools(forest = TRUE)"
  }else {
    tools <- c(suppressMessages(rob_tools()), "ROB1")
    message_content <- "rob_tools()"
  }

  if ((tool %in% tools) == FALSE) {
    stop(
      paste0(
        "\nTool name \"",
        tool,
        "\" not recognised \nAcceptable values for the \"tool\" " ,
        "parameter include ",
        paste0("\"",paste0(tools, collapse = "\", \""),"\".")
      )
    )
  }
}

# Check the first row of the data for column titles
check_first_row <- function(data){

  header <- stringr::str_to_lower(data[1,])

  if (any(c("study","overall", "weight") %in% header)) {
    stop(paste("It looks like the first row of your dataset contains column",
               "headings (e.g. \"Study\", \"Overall\"). Did you set ",
               "\"header = TRUE\" when reading in your data?")
    )
  }

}

# Check user-provided colours
check_colour <- function(tool, colour) {
  if(!(colour[1] %in% c("cochrane","colourblind"))){
    if (tool == "ROB2" || tool == "ROB2-Cluster" || tool == "QUADAS-2") {
      if(length(colour)!=4){
        stop(paste("Wrong number of colours specified.",
                   "This template expects 4 colours."))
      }

    } else{
      if(length(colour)!=5){
        stop(paste("Wrong number of colours specified.",
                   "This template expects 5 colours."))
      }
    }
  }
}

# Checks that the number of observed columns equals the number of expected
# columns, given function arguments
check_cols <- function(data,
                       max_domain_column,
                       overall,
                       type = "tf",
                       weight = FALSE){

  expected_col <- max_domain_column + 1

  if (!overall & !weight) {
    expected_col <- expected_col - 2
    domain_text = paste0(expected_col,
                         ": a \"Study\" column and ",
                         max_domain_column - 2,
                         " \"Domain\" columns.")
    var_ind <- "neither"

  }

  if (!overall & weight) {
    expected_col <- expected_col - 1
    domain_text = paste0(
      expected_col,
      ": a \"Study\" column, ",
      max_domain_column - 1,
      " \"Domain\" columns, and \"Weight\" column."
    )
    var_ind <- "weight"
  }

  if (overall & !weight) {
    expected_col <- expected_col - 1
    domain_text = paste0(
      expected_col,
      ": a \"Study\" column, ",
      max_domain_column - 1,
      " \"Domain\" columns, and an \"Overall\" column."
    )
    var_ind <- "overall"
  }

  if (overall & weight) {
    expected_col <- expected_col
    domain_text = paste0(
      expected_col,
      ": a \"Study\" column, ",
      max_domain_column - 2,
      " \"Domain\" columns, an \"Overall\" column and a \"Weight\" column."
    )
    var_ind <- "both"
  }

  if (type == "summ") {
    weighted_text <- paste(" and weighted =", weight)
  } else {
    weighted_text <- ""
  }

  if (ncol(data) == expected_col) {
    if ((var_ind %in% c("both", "weight")) &&
        unique(grepl("^[-]{0,1}[0-9]{0,}.{0,1}[0-9]{1,}$",
                     data[[ncol(data)]])) == FALSE) {
      stop(
        "Error. The final column does not seem to contain numeric values ",
        "(expected for weighted = TRUE)."
      )
    }} else {
      if (ncol(data) != expected_col) {
        stop(
          "The number of columns in your data (",
          ncol(data),
          ") does not match the number expected for this",
          " tool when using overall = ", overall, weighted_text,
          ". The expected number of columns is ",
          domain_text
        )}
    }
}

# Checks that specified file type is supported
check_extension <- function(file){

  ex <- strsplit(basename(file), split="\\.")[[1]]

  if (!(ex[-1] %in% c("png","jpeg","tiff","eps"))) {
    stop(paste0("Saving to this file type is not supported by robvis. ",
                "Acceptable file types are \".png\", \".jpeg\", ",
                " \".tiff\", and \".eps\". "))
  }
}

# Give depreciated warning if "ROB1" specified
check_rob1 <- function(tool) {
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



# Return cleaned data ==========================================================
# Functions in this section return "cleaned" versions of their inputs

# Covert US spelling of "colorblind" to "colourblind"
clean_colour_spelling <- function(colour) {
  colour <- ifelse(colour == "colorblind", "colourblind", colour)
  return(colour)
}


# Sub-helper function used in the tidy_ functions
clean_data <- function(col) {
  col <- gsub("\\b(\\pL)\\pL{1,}|.", "\\U\\1", col, perl = TRUE)
  col <- trimws(tolower(col))
  col <- ifelse(col %in% c("na", "n") | is.na(col), "x", col)
  col <- substr(col, 0, 1)
  return(col)
}


# Convert to long tidy format
tidy_data_tf <- function(data,
                      max_domain_column,
                      domain_names,
                      overall,
                      levels) {

  check_cols(data = data,
             max_domain_column = max_domain_column,
             overall = overall,
             weight = FALSE)

  if (!overall) {
    max_domain_column <- max_domain_column - 1
    domain_names <- domain_names[1:max_domain_column]
  }

  data.tmp <-
    cbind(data[, 1], data.frame(lapply(data[, 2:max_domain_column], clean_data),
                                stringsAsFactors = F))

  names(data.tmp) <- domain_names

  rob.tidy <- suppressWarnings(tidyr::gather(data.tmp,
                                             domain, judgement,-Study))


  rob.tidy$Study <-
    factor(rob.tidy$Study, levels = unique(data.tmp$Study))

  rob.tidy$judgement <- as.factor(rob.tidy$judgement)

  rob.tidy$judgement <-
    factor(rob.tidy$judgement, levels = levels)

  rob.tidy
}


# Convert to long tidy format
tidy_data_summ <- function(data,
                           max_domain_column,
                           overall,
                           weighted,
                           domain_names,
                           levels) {

  # Deal with legacy versions of the example datasets
  if (ncol(data) == max_domain_column + 1) {
    if (overall == FALSE) {
      data <- data[,c(1:max_domain_column-1,max_domain_column + 1)]
    }
    if (weighted == FALSE) {
      data <- data[,-ncol(data)]
    }
  }

  # Check columns are as expected, given the options
  check_cols(
    data = data,
    max_domain_column = max_domain_column,
    overall = overall,
    type = "summ",
    weight = weighted
  )

  if (overall ==  FALSE) {
    max_domain_column <- max_domain_column - 1
    domain_names <- domain_names[c(1:max_domain_column, length(domain_names))]
  }

  if (weighted == FALSE) {
    data[, max_domain_column + 1] <- rep(1, length(nrow(data)))
  }

  data.tmp <-
    cbind(data[,1],data.frame(lapply(data[, 2:max_domain_column], clean_data),
                              data[, ncol(data)],
                              stringsAsFactors = F))

  names(data.tmp) <- domain_names

  rob.tidy <- suppressWarnings(tidyr::gather(
    data.tmp[-1],
    domain, judgement, -Weights
  ))

  rob.tidy$domain <- as.factor(rob.tidy$domain)

  rob.tidy$domain <-
    factor(rob.tidy$domain,
           levels = rev(domain_names))

  rob.tidy$judgement <-
    factor(rob.tidy$judgement, levels = levels)

  rob.tidy
}










# Return ggplot themes =========================================================
# Functions in this section return a ggplot2 theme object

theme_rob_summ <- function(overall = TRUE, max_domain_column){
  standard <- list(
    ggplot2::geom_bar(
      mapping = ggplot2::aes(
        x = domain,
        fill = judgement,
        weight = Weights
      ),
      width = 0.7,
      position = "fill",
      color = "black"
    ),
    ggplot2::coord_flip(ylim = c(
      0,
      1
    )),
      ggplot2::guides(fill = ggplot2::guide_legend(reverse = T)),
    ggplot2::scale_y_continuous(labels = scales::percent),
      ggplot2::theme(
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        axis.line.x = ggplot2::element_line(
          colour = "black",
          linewidth = 0.5,
          linetype = "solid"
        ),
        legend.position = "bottom",
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        legend.background = ggplot2::element_rect(
          linetype = "solid",
          colour = "black"
        ),
        legend.title = ggplot2::element_blank(),
        legend.key.size = ggplot2::unit(0.75, "lines"),
        legend.text = ggplot2::element_text(size = 6)
      ),
    bold_overall = ggplot2::theme(axis.text.y = ggplot2::element_text(
      size = 10,
      color = "black"
    ))
  )

  if (overall) {
    standard[["bold_overall"]] <-
      ggplot2::theme(axis.text.y = suppressWarnings(ggplot2::element_text(size = 10,
                                                            color = "black",
                                                            face = c("bold", rep("plain",max_domain_column)))))
  }

  return(standard)
}

theme_rob_tf <-function(rob.tidy,
                        domain_names,
                        psize,
                        ssize,
                        adjust_caption,
                        overall,
                        judgement_title = "Judgement",
                        overall_name = "Overall",
                        x_title = "Risk of bias domains",
                        y_title = "Study"){
  standard <- list(
      ggplot2::facet_grid(Study ~
                            factor(domain, levels = domain_names),
                          switch = "y",
                          space = "free"),
      ggplot2::geom_point(size = 6),
      ggplot2::geom_point(size = 4,
                          colour = "black",
                          ggplot2::aes(shape = judgement)),
      ggplot2::geom_rect(
        data = rob.tidy[which(rob.tidy$domain !=
                                overall_name),],
        fill = "#ffffff",
        color = "#ffffff",
        xmin = -Inf,
        xmax = Inf,
        ymin = -Inf,
        ymax = Inf,
        show.legend = FALSE
      ),
      overall_name = ggplot2::geom_rect(
        data = rob.tidy[which(rob.tidy$domain ==
                                overall_name),],
        fill = "#d3d3d3",
        color = "#d3d3d3",
        xmin = -Inf,
        xmax = Inf,
        ymin = -Inf,
        ymax = Inf,
        show.legend = FALSE
      ),
      ggplot2::geom_point(size = psize, show.legend = FALSE),
      ggplot2::geom_point(
        data = rob.tidy[which(rob.tidy$judgement !=
                                "x"),],
        shape = 1,
        colour = "black",
        size = psize,
        show.legend = FALSE
      ),
      ggplot2::geom_point(
        size = ssize,
        colour = "black",
        ggplot2::aes(shape = judgement),
        show.legend = FALSE
      ),
      ggplot2::scale_x_discrete(position = "top", name = x_title),
        ggplot2::scale_y_continuous(
          limits = c(1, 1),
          labels = NULL,
          breaks = NULL,
          name = y_title,
          position = "left"
        ),
  ggplot2::scale_size(range = c(5,20)),
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
  ggplot2::guides(shape = ggplot2::guide_legend(
    override.aes = list(fill = NA))),
  ggplot2::labs(shape = judgement_title, colour = judgement_title)

)

  # Remove element that draws dark box for "Overall" column
  if (!overall) {
    standard[["overall_name"]] <- ggplot2::geom_blank()
  }

  return(standard)

}

# Return tool-specific values ==================================================
# Functions in this section return specific values based on user-defined inputs.
# This includes tool-specific values (judgement labels, colours) based on the
# selected tool; and plot adjustment/saving values based on the dimensions of
# the user-defined data.
# All functions in this section start with the get_ prefix


# Get acceptable judgements by tool
get_judgements <- function(tool){
  # TODO Need to double check the options for each tool

  if (tool == "ROB2") {
    values = c("High",
               "Some concerns",
               "Low",
               "No information")
  }

  if (tool == "ROB2-Cluster") {
    values = c("High",
               "Some concerns",
               "Low",
               "No information",
               "Not applicable")
  }

  if (tool == "ROBINS-I") {
    values = c("Critical",
               "Serious",
               "Moderate",
               "Low",
               "No information")
  }

  if (tool == "ROBINS-E") {
    values = c("Very high",
               "High",
               "Some concerns",
               "Low",
               "No information")
  }

  if (tool == "QUADAS-2") {
    values = c("High",
               "Some concerns",
               "Low",
               "No information")

  }

  if (tool == "Generic") {
    values = c(
      "High",
      "Unclear",
      "Some concerns",
      "Moderate",
      "Low",
      "No information",
      "Not applicable"
    )
  }

  return(values)
}


# Define caption adjustment value, based on number of unique judgements
get_caption_adjustment <- function(data){
  -0.7 + length(unique(data$judgement)) * -0.6
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


# Used only in testing =========================================================
# Functions in this section are used solely to enable testing of the package.
# YOU ALMOST CERTAINLY DO NOT NEED TO EDIT FUNCTIONS IN THIS SECTION.

save_png <- function(code, width = 1400, height = 800) {
  path <- tempfile(fileext = ".png")
  grDevices::png(path, width = width, height = height)
  on.exit(grDevices::dev.off())
  code

  return(path)
}

get_res <- function(tool){

  dat.bcg <-
    cbind(metadat::dat.bcg, rob_dummy(13, tool, study = FALSE))

  dat <-
    metafor::escalc(
      measure = "RR",
      ai = tpos,
      bi = tneg,
      ci = cpos,
      di = cneg,
      data = dat.bcg,
      slab = paste(author, year, sep = ", ")
    ) %>%
    dplyr::mutate(Study = paste(author, year))

  # Prep bias datasets
  res <- metafor::rma(yi, vi, data = dat, slab = paste(author, year))

  return(res)
}
