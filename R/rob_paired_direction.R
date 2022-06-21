

tmp <- read.csv("data_raw/bias_direction.csv") %>%
  mutate(overall = sample(c("Low","Moderate","High"),nrow(tmp), TRUE),
         result_id = X) %>%
  mutate(type = case_when(result_id %in% c(11,17) ~ "MR",
                          result_id %in% c(20) ~ "RCT",
         T ~ type))



rob_paired_direction <-
  function(dat,
           sei = NULL,
           title = NULL,
           legend_cex = 0.9,
           rob_levels = c("Low","Moderate","Serious","Critical"),
           type_levels = c("MR","NRSI","NRSE","RCT"),
           ...) {

    ### calculate log risk ratios and corresponding sampling variances (and use
    ### the 'slab' argument to store study labels as part of the data frame)

    if (("author" %in% colnames(dat)) == FALSE) {
      dat$author <- letters[1:nrow(dat)]
    }

    if (("year" %in% colnames(dat)) == FALSE) {
      dat$year <- "2000"
    }

    dat <- dat %>%
      mutate(type = factor(type, levels = type_levels)) %>%
      mutate(overall = factor(overall, levels = rob_levels)) %>%
      arrange(type, overall, desc(author))

    dat[is.na(dat)] <- "None"

    dat_rob_vec <- dat %>%
      mutate(row_n = 1:n()) %>%
      group_by(type) %>%
      summarise(n=n(),max = max(row_n), min = min(row_n)) %>%
      mutate(offset = seq(1,length(unique(.$type))*3,by=3)) %>%
      mutate(min = min+offset, max =max+offset, heading = max+1, stats = min-1.25) %>%
      mutate(min = ifelse(n==1,min-1,min),
             max = ifelse(n==1,max-1,max),
             heading = ifelse(n==1,heading-1,heading))

    if (length(unique(dat$type))==1) {
      dat_rob_vec <- dat_rob_vec %>%
        mutate(across(c(min, max, heading),~.-1))
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
      mutate(across(-c(result_id,author,type,yi,vi, year), robvis:::clean_data))

    # Combine direction and type
    for (j in paste0("d",1:7)) {
      for (i in 1:nrow(dat)) {
        dat[i,paste0(j,"d")] <- paste0(dat[i,paste0(j,"d")],dat[i,paste0(j,"t")])
      }
    }

    x_pos <- seq(x_max, by = 0.45, length.out = 9 - 2)

    x_overall_pos <- max(x_pos) + 1

    # Convenience vector, specifying x-axis positions for all risk of bias columns
    header_row <- c(x_pos, x_overall_pos)

    legend_pos <- x_max+(max(header_row)-min(header_row))/2

    # New right-hand x-axis limit
    new_x_lim <- x_overall_pos + .5

    rob_colours <- robvis:::get_colour("ROBINS-I", "cochrane")

    judgements<-   c(  "High risk of bias",
                       "Moderate risk of bias",
                       "Low risk of bias")
    cols <- c(
      h = rob_colours$high_colour,
      m = rob_colours$concerns_colour,
      l = rob_colours$low_colour,
      n = rob_colours$ni_colour,
      x = "transparent"
    )

    syms <- c(ua = "?",
              up = "?",
              tp = "<",
              rp = ">",
              la = "\U2190",
              ra = "\U2192",
              l = "\U2190",
              r = "\U2192",
              xx = "",
              x = "")

    shapes <- c(c = 15,
                s = 15,
                m = 15,
                l = 15,
                n = 15,
                x = 15)


    rob_psize = 3
    tsize <- rob_psize * 0.3

    #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
    # Make forest plot

    ### set up forest plot (with 2x2 table counts added; the 'rows' argument is
    ### used to specify in which rows the outcomes will be plotted)
    metafor::forest(x = dat$yi,
                    vi = dat$vi,
#          sei = dat$sei,
           xlim=c(x_min, new_x_lim),
           atransf=exp,
           slab = paste0("  ", dat$author, " ", dat$year),
           cex=1.2,
           ylim=c(-1.5, y_max),
           rows=rows,
           textpos = textpos,
           mlab = "",
           header="Author(s) and Year",
           ...
)

    ### set font expansion factor (as in forest() above) and use a bold font
    op <- par(font=2)

    ### switch to bold italic font
    par(font=2)

    ### add text for the subgroups
    for (i in 1:nrow(dat_rob_vec)) {

      text(x_min, dat_rob_vec$heading[i], pos=4, dat_rob_vec$type[i], cex = 1.2)
    }

    ### set par back to the original settings
    par(op)

    #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
    # Add risk of bias data

    headers <- c("D1", "D2", "D3", "D4", "D5","D6","D7", "O")

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
        pch = shapes[dat[[paste0("d", j,"j")]]],
        col = alpha(cols[dat[[paste0("d", j,"j")]]],0.6),
        cex = rob_psize
      )
      graphics::text(x_pos[j], rows, syms[dat[[paste0("d", j,"d")]]], cex = tsize)
    }

    graphics::points(
      rep(x_overall_pos, length(rows)),
      rows,
      pch = 15,
      col = alpha(cols[dat[["overall"]]],0.6),
      cex = rob_psize
    )
    # graphics::text(x_overall_pos, rows, syms[dat[["overall"]]], cex = tsize)
    par(op)

    #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
    # Add sub-group, summary polygons & text

    rma_flexi <- function(x) {
      rma(
        yi,
        vi,
        subset = (type == x),
        data = dat,
        method = "DL"
      )
    }


    res <- purrr::map(dat_rob_vec$type, rma_flexi)

    if (length(unique(dat$type))>1) {

      ### add summary polygons for the three subgroups
      for (i in 1:nrow(dat_rob_vec)) {

        if (length(res[[i]]$slab) == 1) {
          next
        }

        addpoly(
          res[[i]],
          fonts = 4,
          row = dat_rob_vec$stats[i],
          cex = 1.2,
          textpos=textpos,
          atransf = exp,
          annotate = F,
          mlab = mlabfun("RE Model for Subgroup", res[[i]])
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
      par(font = 2)
      text(x_min, y_max, pos=4, bquote(bold(underline(.(title)))), cex = 1.2)
      par(op)
    }

    #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

    graphics::legend(
      legend_pos-1.625,
      -1.7,
      c(judgements),
      pch = c(15,15,15,16,50),
      xjust = 0.5,
      col = c(cols[1:3],"white","white"),
      xpd = TRUE,
      title = parse(text = "bold(\"Extent of bias\")"),
      title.adj = 0.05,
      cex = legend_cex,
      pt.cex = legend_cex-.1,
      y.intersp = 0.7
    )

    graphics::legend(
      legend_pos+0.95,
      -1.7,
      c("\U2190  \U2192  Additive bias  ","  <   >   Proportional bias", "    ?     Unpredictable"),
      xjust = 0.5,
      xpd = TRUE,
      adj = 0.15,
      title = parse(text = "bold(\"Type of bias\")"),
      title.adj = 0.05,
      cex = legend_cex,
      pt.cex = legend_cex,
      y.intersp = 0.7
    )

  }


mlabfun <- function(text, res) {
  list(bquote(paste(.(text),
                    " (",
                    # " Q = ", .(formatC(res$QE, digits=2, format="f")),
                    # ", df = ", .(res$k - res$p),
                    "p ", .(metafor:::.pval(res$pval, digits=2, showeq=TRUE, sep=" ")), "; ",
                    I^2, " = ", .(formatC(res$I2, digits=1, format="f")), "%, ",
                    tau^2, " = ", .(formatC(res$tau2, digits=2, format="f")), ")")))}



annotate_poly <- function(yi, ci.lb, ci.ub, atransf = exp, textpos = 2, width, rows, cex=1.2){

  if (is.function(atransf)) {

    annotext <- cbind(sapply(yi, atransf), sapply(ci.lb, atransf), sapply(ci.ub, atransf))
    ### make sure order of intervals is always increasing

    tmp <- .psort(annotext[,2:3])
    annotext[,2:3] <- tmp

  } else {

    annotext <- cbind(yi, ci.lb, ci.ub)

  }

  annotext <- .fcf(annotext, 2)

  if (missing(width) || is.null(width)) {
    width <- apply(annotext, 2, function(x) max(nchar(x)))
  } else {
    if (length(width) == 1L)
      width <- rep(width, ncol(annotext))
  }

  for (j in seq_len(ncol(annotext))) {
    annotext[,j] <- formatC(annotext[,j], width=width[j])
  }

  annotext <- cbind(annotext[,1], " [", annotext[,2], ", ", annotext[,3], "]")
  annotext <- apply(annotext, 1, paste, collapse="")
  text(x=textpos, rows, labels=annotext, pos=2, cex=cex)

}


.fcf <- function(x, digits) {

  if (all(is.na(x))) { # since formatC(NA, format="f", digits=2) fails
    x
  } else {
    trimws(formatC(x, format="f", digits=digits))
  }

}

.psort <- function(x,y) {

  ### t(apply(xy, 1, sort)) would be okay, but problematic if there are NAs;
  ### either they are removed completely (na.last=NA) or they are always put
  ### first/last (na.last=FALSE/TRUE); but we just want to leave the NAs in
  ### their position!

  if (is.null(x) || length(x) == 0L) ### need to catch this
    return(NULL)

  if (missing(y)) {
    if (is.matrix(x)) {
      xy <- x
    } else {
      xy <- rbind(x) ### in case x is just a vector
    }
  } else {
    xy <- cbind(x,y)
  }

  n <- nrow(xy)

  for (i in seq_len(n)) {
    if (anyNA(xy[i,]))
      next
    xy[i,] <- sort(xy[i,])
  }

  colnames(xy) <- NULL

  return(xy)

}

add_subgroup <- function(res, row, method = "RE"){

  addpoly(
    res,
    fonts = 4,
    row = row,
    cex = 1,
    textpos = -4,
    atransf = exp,
    annotate = F,
    mlab = mlabfun(paste(method,"Model for lipid fraction"), res)
  )

  annotate_poly(res["b"],
                res["ci.lb"],
                res["ci.ub"],
                rows = row,
                textpos = 2.25,
                cex = 1)
}

rma_flexi <- function(x,method = "DL") {
  rma(
    yi,
    sei = sei,
    subset = (term == x),
    data = main_effects,
    method = method
  )
}
