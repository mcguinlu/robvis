#' @title Append a risk-of-bias traffic-light plot to a forest plot
#'
#' @description A wrapper for metafor::forest function, which adds a risk of
#'   bias traffic-light plot to the right-hand side of the forest plot. The
#'   heavy lifting for this function is done by metafor. Note that if not
#'   specified as additional arguments, this functions sets the header argument
#'   of metafor::forest() to TRUE.
#'
#' @param res Output from metafor meta-analysis function
#' @param rob_tool The risk-of-bias assessment tool used to perform the
#'   assessments
#' @param rob_levels Vector of judgments [e.g. c("Low","Some
#'   concerns","High","Critical")] that controls the ordering of subgroups
#'   within the plot
#' @param title Text to use for plot title
#' @param rob_legend Logical specifying whether a legend for the risk-of-bias
#'   plot should be shown. Default is TRUE.
#' @param rob_legend_cex Expansion factor for the risk-of-bias legend
#' @param ... Additional arguments to be passed to the metafor::forest()
#'   function
#'
#' @family main
#'
#' @export

rob_forest <-
  function(res,
           rob_tool = "ROB2",
           rob_me = NULL,
           rob_levels = NULL,
           title = NULL,
           rob_legend = TRUE,
           rob_legend_cex = 0.9,
           ...) {


    # Check that res is of class RMA
    if (!("rma" %in% class(res))) {
      stop("Result objects need to be of class \"meta\" - output from metafor package functions")
    }

    # Check that the specified tool is supported
    check_tool(rob_tool, forest = TRUE)


    if (is.null(rob_levels)) {

    if (rob_tool == "ROB2") {
      rob_levels <- rev(c("Low","Some concerns","High","Critical"))
    } else {
      rob_levels <- rev(c("Low","Moderate","Serious","Critical"))
    }

    }

    colnames(res$data) <- stringr::str_to_lower(colnames(res$data))

    dat <- res$data %>%
      dplyr::mutate(overall = factor(overall, levels = rob_levels)) %>%
      dplyr::arrange(overall, dplyr::desc(author))


    # Get maximum domain

    max_domain_column <- dat %>%
      dplyr::select(dplyr::matches("^d.$")) %>%
      colnames() %>%
      gsub("d","",.) %>%
      as.numeric() %>%
      max() + 2


    # Use this to define the gaps between different groups
    # Will be important when adding argument to prevent subgroup analyses
    offset_n <- 3

    dat_rob_vec <- dat %>%
      dplyr::mutate(row_n = 1:dplyr::n()) %>%
      dplyr::group_by(overall) %>%
      dplyr::summarise(n=dplyr::n(),max = max(row_n), min = min(row_n)) %>%
      dplyr::mutate(offset = seq(1,length(unique(.$overall))*offset_n,by=offset_n)) %>%
      dplyr::mutate(min = min+offset, max =max+offset, heading = max+1, stats = min-1.25) %>%
      dplyr::mutate(min = ifelse(n==1,min-1,min),
                    max = ifelse(n==1,max-1,max),
                    heading = ifelse(n==1,heading-1,heading))

    if (length(unique(dat$overall))==1) {
      dat_rob_vec <- dat_rob_vec %>%
        dplyr::mutate(dplyr::across(c(min, max, heading),~.-1))
    }

    res <- update(res, data = dat)

    rows <- c()

    for (i in 1:nrow(dat_rob_vec)) {

      rows <-c(rows, dat_rob_vec$min[i]:dat_rob_vec$max[i])

    }

    arg <- list(...)

    if (is.null(arg$at)) {
      x_adj <- log(3)
    } else {
      x_adj <- arg$at[3]
    }

    if (is.null(arg$x_min)) {
      x_min = -10
    } else {
      x_min <- arg$x_min
    }

    x_max = 4.6 - log(3) + x_adj
    textpos <- c(x_min, x_max-1)
    y_max <- max(rows)+4

    #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
    # Deal with adding rob data

    dat <- dat %>%
      dplyr::mutate(dplyr::across(dplyr::matches("^d.$|overall"),
                                  clean_data))

    x_pos <- seq(x_max, by = 0.45, length.out = max_domain_column - 2)

    x_overall_pos <- max(x_pos) + 1

    # Convenience vector, specifying x-axis positions for all risk of bias columns
    header_row <- c(x_pos, x_overall_pos)

    legend_pos <- x_max+(max(header_row)-min(header_row))/2

    # New right-hand x-axis limit
    new_x_lim <- x_overall_pos + .5

    rob_colours <- robvis:::get_colour(rob_tool, "cochrane")

    if (rob_tool %in% c("ROB2", "QUADAS-2")) {
      judgements<-   c("High risk of bias",
                       "Some concerns",
                       "Low risk of bias",
                       "No information")

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

      shapes <- c(h = 15,
                  s = 15,
                  l = 15,
                  n = 15,
                  x = 15
      )
    }

    if (rob_tool == "ROBINS-I") {
      judgements<-   c("Serious risk of bias",
                       "Moderate risk of bias",
                       "Low risk of bias",
                       "No information")
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
                n = "",
                x = "")


      shapes <- c(c = 15,
                  s = 15,
                  m = 15,
                  l = 15,
                  n = 15,
                  x = 15)

    }

    rob_psize = 3
    tsize <- rob_psize * 0.3

    #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
    # Make forest plot

    ### set up forest plot (with 2x2 table counts added; the 'rows' argument is
    ### used to specify in which rows the outcomes will be plotted)
    metafor::forest(res, xlim=c(x_min, new_x_lim), atransf=exp,
           cex=1.2, ylim=c(-1.5, y_max), rows=rows, textpos = textpos,
           mlab=mlabfun("RE Model for all studies", res),
           header="Author(s) and Year", addpred = T,...)

    ### set font expansion factor (as in forest() above) and use a bold font

    if (any(grepl("\\*", dat$year))) {
      dat <- dat %>%
        mutate(measure = case_when(grepl("\\*", year) ~ "OR",
                                   T ~ "HR"))

      graphics::text(rep(-2.25,length(rows)), rows, dat$measure, cex = 1.2 )

      par(font = 2)
      graphics::text(-2.25, y_max - 1, labels = "Measure", cex=1.2)
    }

    op <- par(font=2)

    ### switch to bold italic font
    par(font=2)

    ### add text for the subgroups
    for (i in 1:nrow(dat_rob_vec)) {

      text(x_min, dat_rob_vec$heading[i], pos=4, dat_rob_vec$overall[i], cex = 1.2)
    }

    ### set par back to the original settings
    par(op)

    #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
    # Add risk of bias data

    headers <- c(paste0("D",seq_len(max_domain_column-2)),"O")

    par(font = 2)
    # Need to add handling of top here
    graphics::text(mean(header_row), y_max, labels = "Risk of Bias", cex=1.2)
    graphics::text(header_row, y_max-2 + 1, labels = headers, cex=1.2)
    par(op)

    # Plot domain points
    for (j in 1:length(x_pos)) {
      graphics::points(
        x = rep(x_pos[j], length(rows)),
        y = rows,
        pch = shapes[dat[[paste0("d", j)]]],
        col = scales::alpha(cols[dat[[paste0("d", j)]]],0.6),
        cex = rob_psize
      )
      graphics::text(x_pos[j], rows, syms[dat[[paste0("d", j)]]], cex = tsize)
    }


    graphics::points(
      rep(x_overall_pos, length(rows)),
      rows,
      pch = 15,
      col = scales::alpha(cols[dat[["overall"]]],0.6),
      cex = rob_psize
    )
    graphics::text(x_overall_pos, rows, syms[dat[["overall"]]], cex = tsize)
    par(op)
    #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
    # Add sub-group, summary polygons & text

    rma_flexi <- function(x) {
        update(res,
          subset = (overall == x)
        )
      }

    subgroup_res <- purrr::map(unique(dat$overall), rma_flexi)

    if (length(unique(dat$overall))>1) {

      ### add summary polygons for the three subgroups
      for (i in 1:nrow(dat_rob_vec)) {

        if (length(subgroup_res[[i]]$slab) == 1) {
          next
        }

        metafor::addpoly(
          subgroup_res[[i]],
          # fonts = 4,
          row = dat_rob_vec$stats[i],
          cex = 1.2,
          textpos=textpos,
          atransf = exp,
          annotate = F,
          mlab = mlabfun("RE Model for Subgroup", subgroup_res[[i]])
        )

        annotate_poly(subgroup_res[[i]]$b,
                      subgroup_res[[i]]$ci.lb,
                      subgroup_res[[i]]$ci.ub,
                      textpos = textpos,
                      rows = dat_rob_vec$stats[[i]])

      }
    }

    #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

    if(!is.null(title)){
      par(font = 2)
      text(x_min, y_max, pos=4, bquote(bold(underline(.(title)))), cex = 1.2)
      par(op)
    }


    #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

    if (length(unique(dat$overall))>1 && nrow(dat)>9) {

      # Fit meta-regression model to test for subgroup differences
      subgroup_res <- update(res, mods = ~ overall, method = "DL")


      ### add text for the test of subgroup differences
      text(x_min,-1.8, pos = 4, cex = 1.2, bquote(
        paste(
          "Test for Subgroup Differences: ",
          Q[M],
          " = ",
          .(formatC(
            subgroup_res$QM, digits = 2, format = "f"
          )),
          ", df = ",
          .(subgroup_res$p - 1),
          ", p = ",
          .(formatC(
            subgroup_res$QMp, digits = 2, format = "f"
          ))
        )
      ))
    }


    # Add missing evidence
    if (!is.null(rob_me)) {
    rob_me <- robvis:::clean_data(rob_me)

    rob_me_colours <- robvis:::get_colour("ROB2", "cochrane")

    rob_me_cols <- c(
      h = rob_me_colours$high_colour,
      s = rob_me_colours$concerns_colour,
      l = rob_me_colours$low_colour,
      n = rob_me_colours$ni_colour,
      x = rob_me_colours$na_colour
    )

    rob_me_syms <- c(h = "X",
                     s = "-",
                     l = "+",
                     n = "?",
                     x = ""
    )

    text(x_pos[1]-.5,-1,pos=4,cex=1.2,"ROB Missing Evidence: ")


    graphics::points(
      x_overall_pos,
      -1,
      pch = 15,
      col = scales::alpha(rob_me_cols[rob_me],0.6),
      cex = rob_psize
    )
    graphics::text(x_overall_pos,font = 2, -1, rob_me_syms[rob_me], cex = tsize)
    }

    if (rob_legend == TRUE) {

      graphics::legend(
        legend_pos,
        -1.8,
        judgements,
        pch = 15,
        xjust = 0.5,
        col = head(cols,-1),
        xpd = TRUE,
        title = parse(text = "bold(\"Judgement\")"),
        title.adj = 0.1,
        cex = rob_legend_cex,
        pt.cex = rob_legend_cex,
        y.intersp = 0.7
      )
    }

  }
