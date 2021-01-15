#' @title Append a risk-of-bias traffic-light plot to a forest plot
#'
#' @description A wrapper for metafor::forest function, which adds a risk of
#'   bias traffic-light plot to the right-hand side of the forest plot. The
#'   heavy lifting for this function is done by metafor. Note that if not
#'   specified as additional arguments, this functions sets the header argument
#'   of metafor::forest() to TRUE.
#'
#' @param res Output from metafor meta-analysis function
#' @param rob_data Risk of bias dataset in standard robvis format
#' @param rob_psize Size of the points in the appended risk of bias table
#' @param rob_tool The risk-of-bias assessment tool used to perform the
#'   assessments
#' @param rob_colour Risk of bias colour scheme to use for the traffic-light
#'   plot
#' @param rob_caption Logical specifying whether a message containing the
#'   risk-of-bias domains should be printed. Default is TRUE
#' @param rob_legend Logical specifying whether a legend for the risk-of-bias
#'   plot should be shown. Default is TRUE.
#' @param ... Additional arguments to be passed to the metafor::forest()
#'   function
#' @export

rob_append_to_forest <-
  function(res,
           rob_data,
           rob_tool = "ROB2",
           rob_colour = "cochrane",
           rob_psize = 2,
           rob_caption = TRUE,
           rob_legend = TRUE,
           ...) {

    rob_colour <- rob_colour

    # Checks

    if (!("rma" %in% class(res))) {
      stop("Result objects need to be of class \"meta\" - output from metafor package functions")
    }

    # Check that the specified tool is supported
    check_tool(rob_tool, forest = TRUE)

    # Check the data supplied is okay
    check_data(rob_data)

    # Convert spelling if US
    rob_colour <- weird_spelling(rob_colour)

    # Check names
    res$slab <- gsub(",", "", res$slab)

    colnames(rob_data) <- stringr::str_to_lower(colnames(rob_data))

    if (!all(res$slab %in% rob_data$study)) {
      stop("Mismatched names between the \"meta\" object and the risk of bias dataset")
    }

    # Clean data
    max_domain_column <- ifelse(rob_tool == "ROB2",7,9)

    rob_data <-
      cbind(rob_data[, 1], data.frame(lapply(rob_data[, 2:max_domain_column], clean_data),
                                 stringsAsFactors = F))


    # Get and expand original x limits to allow for ROB plot ====

    # Call metafor::forest to get returned plot values
    # Function is defined in blobbogram_helpers.R

    t <- forest.invisible(res, ...)

    # Using original x_lim, create elements needed for new plot
    # Position of columns along the x-axis
    x_pos <- seq(t$xlim[2], by = 0.55, length.out = max_domain_column - 2)

    # Position of overall column on x-axis
    x_overall_pos <- max(x_pos) + 1

    # Convenience vector, specifying x-axis positions for all risk of bias columns
    header_row <- c(x_pos, x_overall_pos)

    legend_pos <- t$xlim[2]+(max(header_row)-min(header_row))/2

    # New right-hand x-axis limit
    new_x_lim <- x_overall_pos + .5

    # Sequence of row numbers, descending
    nrow_seq <- length(t$rows):1

    # Sort colours  and symbols ====
    rob_colours <- get_colour(rob_tool, rob_colour)


    if (rob_tool %in% c("ROB2", "QUADAS-2")) {
      cols <- c(
        h = rob_colours$high_colour,
        s = rob_colours$concerns_colour,
        l = rob_colours$low_colour,
        n = rob_colours$ni_colour,
        x = rob_colours$na_colour
      )

      syms <- c(h = "X",
                s = "-",
                l = "+",
                n = "?",
                x = ""
      )
    }


    if (rob_tool == "ROBINS-I") {
      cols <- c(
        c = rob_colours$critical_colour,
        s = rob_colours$high_colour,
        m = rob_colours$concerns_colour,
        l = rob_colours$low_colour,
        n = rob_colours$ni_colour,
        x = rob_colours$na_colour
      )

      syms <- c(c = "!",
                s = "X",
                m = "-",
                l = "+",
                n = "?",
                x = "")
    }

    tsize <- rob_psize * 0.3

    # Clean arguments being passed to
    # Remove arguments being defined within this function from the .. argument
    a <- list(...)
    a$xlim <- NULL


    # CEX PROCESSING NOT WORKING PROPERLY IF SPECIFIED IN FOREST CALL!
    rob_textpos <- a$textpos

    if (is.null(rob_textpos)) {
      rob_textpos <- c(t$xlim[1], t$xlim[2] - 1)
    } else {
      rob_textpos <- a$textpos
      a$textpos <- NULL
    }

    # Get cex value, and assign to default from forest() if not specified
    rob_cex <- a$cex

    if (is.null(rob_cex)) {
      rob_cex <- t$cex
    } else {
      a$cex <- NULL
    }

    # Get top value, and assign to 3 if not specified
    rob_top <- a$top

    if (is.null(rob_top)) {
      rob_top <- 3
    }

    # Get header value, and assign to TRUE if not specified
    a$header

    if (is.null(a$header)) {
      a$header <- TRUE
    }

    # Set plotting values
    graphics::par(cex = rob_cex, font = 2)

    # Pass all arguments to forest(), removing those that this function defines
    do.call(metafor::forest, c(list(
      x = res,
      xlim = c(t$xlim[1], new_x_lim),
      textpos = rob_textpos,
      cex = rob_cex
    ), a))

    graphics::par(cex = rob_cex, font = 2)

    # Plot title of domains

    headers <- if(rob_tool == "ROB2"){
      c("R", "D", "Mi", "Me", "S", "O")}else{
        c("D1", "D2", "D3", "D4", "D5","D6","D7", "O")
      }

    # Need to add handling of top here
    graphics::text(mean(header_row), t$ylim[2], labels = "Risk of Bias")
    graphics::text(header_row, t$ylim[2]-(rob_top-1) + 1, labels =headers)

    # Plot domain points
    for (j in 1:length(x_pos)) {
      graphics::points(
        rep(x_pos[j], length(t$rows)),
        nrow_seq,
        pch = 19,
        col = cols[rob_data[[paste0("d", j)]]],
        cex = rob_psize
      )
      graphics::text(x_pos[j], nrow_seq, syms[rob_data[[paste0("d", j)]]], cex = tsize)
    }

    # Plot overall column
    graphics::points(
      rep(x_overall_pos, length(t$rows)),
      nrow_seq,
      pch = 19,
      col = cols[rob_data[["overall"]]],
      cex = rob_psize
    )
    graphics::text(x_overall_pos, nrow_seq, syms[rob_data[["overall"]]], cex = tsize)



    if (rob_legend) {
      graphics::par(font = 1)

      graphics::legend(
        legend_pos,
        -1,
        c("High risk of bias",
          "Some concerns",
          "Low risk of bias",
          "No information"),
        pch = 19,
        xjust = 0.5,
        col = c(rob_colours$high_colour,
                rob_colours$concerns_colour,
                rob_colours$low_colour,
                rob_colours$ni_colour),
        xpd = TRUE,
        title = parse(text = "bold(\"Judgement\")"),
        title.adj = 0.1,
        cex = .7,
        pt.cex = .7,
        y.intersp = 0.7
      )

    }


    # Define caption ====
    domains <-   paste("R: Bias arising from the randomization process.",
                       "D: Bias due to deviations from intended intervention.",
                       "Mi: Bias due to missing outcome data.",
                       "Me: Bias in measurement of the outcome.",
                       "S: Bias in selection of the reported result.",
                       "O: Overall risk of bias")


    if (rob_caption) {
      message(
        "We recommend copying the description below into your figure caption:\n\n",
        "\"Risk-of-bias assessement was performed using the ",
        rob_tool,
        " tool, which has the following domains: ",
        domains,
        "\""
      )
    }

    # Return same list of values as metafor::forest()
    res <- list(xlim = c(t$xlim[1], new_x_lim), alim = t$alim, at = t$at,
                ylim = t$ylim, rows = t$rows, cex = rob_cex, cex.lab = rob_cex,
                cex.axis = rob_cex)

    invisible(res)

  }
