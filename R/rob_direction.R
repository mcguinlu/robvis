# TODO add argument that prevents subgroup summary estimates - in this case, each subgroup will need to be one row closer to each other
# TODO Finish description of function

#' Bias direction plots
#'
#' @description Used to
#'
#' @param dat Dataframe
#' @param vi Vector containing the sampling variances (normally defined as the column within the dataset, i.e. dat$vi). Note: either vi or sei must be set.
#' @param sei Vector containing the corresponding standard errors (normally defined as the column within the dataset, i.e. dat$sei). Note: either vi or sei must be set.
#' @param title Graph title
#' @param legend_cex Expansion factor for figure legend.
#' @param grouping Variable of the provided dataset by which the resulting plot will be stratified. Often will study design or overall risk-of-bias level.
#' @param grouping_levels Ordering of grouping variable. Note: the levels will be plotted
#'   in order, starting at the bottom of the graph (i.e. the last item in the
#'   vector will be placed at the top of the graph)
#' @param label_subgroup_summary Annotation text for subgroup label
#' @param ... Other arguments to pass to metafor::forest
#'
#' @keywords internal

rob_direction <-
  function(dat,
           sei = NULL,
           title = NULL,
           legend_cex = 0.9,
           grouping = "type",
           grouping_levels = c("MR","NRSI","Obs","RCT"),
           label_subgroup_summary = "RE Model for Subgroup",
           ...) {

    ### calculate log risk ratios and corresponding sampling variances (and use
    ### the 'slab' argument to store study labels as part of the data frame)

    if (("study" %in% colnames(dat)) == FALSE) {
      dat$study <- paste("Study", 1:nrow(dat))
    }

    rob_levels = c("Low","Moderate","High","Critical")

    dat <- dat %>%
      dplyr::mutate(type = factor(type, levels = grouping_levels)) %>%
      dplyr::mutate(overall = factor(overall, levels = rob_levels)) %>%
      dplyr::arrange(type, overall, dplyr::desc(study))

    #dat[is.na(dat)] <- "None"


    # Use this to define the gaps between different groups
    # Will be important when adding argument to prevent subgroup analyses
    offset_n <- 3

    dat_rob_vec <- dat %>%
      dplyr::mutate(row_n = 1:dplyr::n()) %>%
      dplyr::group_by(type) %>%
      dplyr::summarise(n=dplyr::n(),max = max(row_n), min = min(row_n)) %>%
      dplyr::mutate(offset = seq(1,length(unique(.$type))*offset_n,by=offset_n)) %>%
      dplyr::mutate(min = min+offset, max =max+offset, heading = max+1, stats = min-1.25) %>%
      dplyr::mutate(min = ifelse(n==1,min-1,min),
                    max = ifelse(n==1,max-1,max),
                    heading = ifelse(n==1,heading-1,heading))

    if (length(unique(dat$type))==1) {
      dat_rob_vec <- dat_rob_vec %>%
        dplyr::mutate(dplyr::across(c(min, max, heading),~.-1))
    }

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
      dplyr::mutate(dplyr::across(-c(result_id,study,type,yi,vi), clean_data))

    # Combine direction and type
    for (j in paste0("d",1:7)) {
      for (i in 1:nrow(dat)) {
        dat[i,paste0(j,"d")] <- paste0(dat[i,paste0(j,"d")],dat[i,paste0(j,"t")])
      }
    }

    x_pos <- seq(x_max-0.5, by = 0.45, length.out = 9 - 2)

    x_overall_pos <- max(x_pos) + 1

    # Convenience vector, specifying x-axis positions for all risk of bias columns
    header_row <- c(x_pos, x_overall_pos)

    legend_pos <- x_max+(max(header_row)-min(header_row))/2

    # New right-hand x-axis limit
    new_x_lim <- x_overall_pos + .5

    # Setting colours (changed)
    rob_colours <- c()
    rob_colours$na_colour <- "#cccccc"
    rob_colours$low_colour <- "#02C100"
    rob_colours$concerns_colour <- "#E2DF07"
    rob_colours$high_colour <- "#BF0000"
    rob_colours$critical_colour <- "#820000"
    rob_colours$ni_colour <- "#4EA1F7"

    judgements<-c("Very high risk",  #changed
                  "High risk",
                  "Moderate risk",
                  "Low risk")
    cols <- c(
      c = rob_colours$critical_colour, #changed
      h = rob_colours$high_colour,
      m = rob_colours$concerns_colour,
      l = rob_colours$low_colour,
      n = rob_colours$ni_colour,
      x = "transparent"
    )

    syms <- c(ua = "?",
              up = "?",
              lp = "<",
              rp = ">",
              la = "\U2190",
              ra = "\U2192",
              l = "\U2190",
              r = "\U2192",
              xx = "",
              x = "")

    shapes <- c(c = 15,
                v = 15,
                h = 15,
                m = 15,
                l = 15,
                n = 15,
                x = 15)


    rob_psize = 3
    tsize <- rob_psize * 0.3

    #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
    # Make forest plot

    res_all <- rma(yi, vi, data=dat)

    ### set up forest plot (with 2x2 table counts added; the 'rows' argument is
    ### used to specify in which rows the outcomes will be plotted)
    metafor::forest(res_all,
                    #x = dat$yi,
                    #vi = dat$vi,
                    #sei = sei,
                    xlim=c(x_min, new_x_lim),
                    atransf=exp,
                    slab = paste0("  ", dat$study),
                    cex=1.2,
                    ylim=c(-1.5, y_max),
                    rows=rows,
                    textpos = textpos,
                    mlab = mlabfun("RE Model for All Studies", res_all),
                    header="Studies",
                    ...
    )

    ### set font expansion factor (as in forest() above) and use a bold font
    op <- graphics::par(font=2)

    ### switch to bold italic font
    graphics::par(font=2)

    ### add text for the subgroups
    for (i in 1:nrow(dat_rob_vec)) {

      graphics::text(x_min, dat_rob_vec$heading[i], pos=4, dat_rob_vec$type[i], cex = 1.2)
    }

    ### set par back to the original settings
    graphics::par(op)

    #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
    # Add risk of bias data

    headers <- c("D1", "D2", "D3", "D4", "D5", "D6","D7", "O")

    graphics::par(font = 2)
    # Need to add handling of top here
    graphics::text(mean(header_row), y_max, labels = "Risk of Bias", cex=1.2)
    graphics::text(header_row, y_max-2 + 1, labels = headers, cex=1.2)
    graphics::par(op)

    # Plot domain points
    for (j in 1:length(x_pos)) {
      graphics::points(
        x = rep(x_pos[j], length(rows)),
        y = rows,
        pch = shapes[dat[[paste0("d", j,"j")]]],
        col = scales::alpha(cols[dat[[paste0("d", j,"j")]]],0.6),
        cex = rob_psize
      )
      graphics::text(x_pos[j], rows, syms[dat[[paste0("d", j,"d")]]], cex = tsize)
    }

    graphics::points(
      rep(x_overall_pos, length(rows)),
      rows,
      pch = 15,
      col = scales::alpha(cols[dat[["overall"]]],0.6),
      cex = rob_psize
    )
    # graphics::text(x_overall_pos, rows, syms[dat[["overall"]]], cex = tsize)
    graphics::par(op)

    # #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
    #



    #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
    # Add sub-group, summary polygons & text

    rma_flexi <- function(x) {
      metafor::rma(
        yi,
        vi,
        subset = (type == x),
        data = dat,
        #method = "DL"  ### CHANGE to have "REML" the default rma
      )
    }


    res <- purrr::map(dat_rob_vec$type, rma_flexi)

    if (length(unique(dat$type))>1) {

      ### add summary polygons for the three subgroups
      for (i in 1:nrow(dat_rob_vec)) {

        if (length(res[[i]]$slab) == 1) {
          next
        }

        metafor::addpoly(
          res[[i]],
          #fonts = 1,
          row = dat_rob_vec$stats[i],
          cex = 1.2,
          textpos=textpos,
          atransf = exp,
          annotate = F,
          mlab = mlabfun(label_subgroup_summary, res[[i]])
        )

        annotate_poly(res[[i]]$b,
                      res[[i]]$ci.lb,
                      res[[i]]$ci.ub,
                      textpos = textpos,
                      rows = dat_rob_vec$stats[[i]])

      }
    }

    #
    #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

    # if (nrow(dat)>9) {
    #
    #   # Fit meta-regression model to test for subgroup differences
    #
    #   if (!is.null(sei)) {
    #
    #     res <- rma(yi, vi, mods = ~ type, data = dat, method = "DL")
    #
    #   } else {
    #
    #     res <- rma(yi, sei=sei, mods = ~ type, data = dat, method = "DL")
    #
    #   }
    #
    #   ### add text for the test of subgroup differences
    #   text(x_min,-1.8, pos = 4, cex = 1.2, bquote(
    #     paste(
    #       "Test for Subgroup Differences: ",
    #       Q[M],
    #       " = ",
    #       .(formatC(
    #         res$QM, digits = 2, format = "f"
    #       )),
    #       ", df = ",
    #       .(res$p - 1),
    #       ", p = ",
    #       .(formatC(
    #         res$QMp, digits = 2, format = "f"
    #       ))
    #     )
    #   ))
    # }


    #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

    if(!is.null(title)){
      graphics::par(font = 2)
      graphics::text(x_min, y_max, pos=4, bquote(bold(underline(.(title)))), cex = 1.2)
      graphics::par(op)
    }

    #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

    graphics::legend(
      legend_pos-2,                         #changed
      -1.5,                                   #changed
      c(judgements),
      pch = c(15,15,15,15,16,50),           #changed
      xjust = 0.5,
      col = c(cols[1:4],"white","white"),   #changed
      xpd = TRUE,
      title = parse(text = "bold(\"Extent of bias\")"),
      title.adj = 0.05,
      cex = legend_cex,
      pt.cex = legend_cex-.1,
      y.intersp = 0.6,                     #changed
      box.col = "white",
    )

    graphics::legend(
      legend_pos+1.5,                    #changed
      -1.5,                                #changed
      c("\U2190  \U2192  Additive","  <   >   Proportional", "    ?     Unpredictable"),
      xjust = 0.5,
      xpd = TRUE,
      adj = 0.15,
      title = parse(text = "bold(\"Type of bias\")"),
      title.adj = 0.05,
      cex = legend_cex,
      pt.cex = legend_cex,
      y.intersp = 0.6,
      box.col = "white"
    )


  }
