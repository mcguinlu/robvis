#' # Put in NEWS.md when ready
#'
#' # * A new function, `rob_blobbogram()` has been added in response to
#' # https://github.com/mcguinlu/robvis/issues/102 by @rdboyes.
#'
#'
#'
#' #' Risk of bias blobbogram plot
#' #'
#' #' @param rma an rma object from the metafor package
#' #' @param rob a risk of bias table formatted like data_rob2
#' #' @param rob_tool risk of bias tool used
#' #' @param rob_colour risk of bias colour scheme
#' #' @param subset_col Column containing the variable to subset the MA by
#' #' @param add_tests include summary of statistical tests by subgroup
#' #' @param overall_estimate show a summary measure across groups?
#' #' @param theme optional theme for the forest plot table
#' #' @param ggplot_is_x_times_right_width relative width of the forest plot
#' #' @param null_line_at what value is the null? default 1.0
#' #' @param file_path path to save the image
#' #' @param dpi image resolution
#' #' @param display should the image be shown in the viewer? set to false for markdown
#' #' @param blank_na show NA values? default is FALSE
#' #' @param font_family (experimental) change fonts in the table
#' #' @param estimate_col_name title of the estimate column, default "Estimate"
#' #'
#' #' @return an image
#' rob_blobbogram <- function(rma,
#'                            rob,
#'                            rob_tool = "ROB2",
#'                            rob_colour = "cochrane",
#'                            subset_col = "Overall",
#'                            add_tests = TRUE,
#'                            overall_estimate = TRUE,
#'                            theme = NULL,
#'                            ggplot_is_x_times_right_width = 1.2,
#'                            null_line_at = 1,
#'                            file_path = tempfile(pattern = "rob_blobbogram", fileext = ".png"),
#'                            dpi = 600,
#'                            display = TRUE,
#'                            blank_na = TRUE,
#'                            font_family = "mono",
#'                            estimate_col_name = "Estimate"){
#'
#'   # This will need to be separated out into its own function, with a good deal of error handling
#'   table <- merge(data.frame(Study = gsub(",","",rma$slab),
#'                             yi = rma$yi,
#'                             vi = rma$vi,
#'                             est = exp(rma$yi),
#'                             ci_low = exp(rma$yi - 1.96 * sqrt(rma$vi)),
#'                             ci_high = exp(rma$yi + 1.96 * sqrt(rma$vi)),
#'                             stringsAsFactors = FALSE),
#'                  rob,
#'                  by = "Study")
#'
#'   # Reorder data
#'   table <- dplyr::select(table, Study, dplyr::everything())
#'
#'   # Define levels by which to subset (ideal use case in ROB2, but allow any)
#'   if (subset_col == "Overall" & rob_tool == "ROB2") {
#'   # Set ROB2 levels
#'   levels <- c("Low", "Some concerns", "High")
#'
#'   # Limit to those levels actually present in the dataset
#'   levels <- levels[which(levels %in% unique(table[[subset_col]]))]
#'   } else {
#'   # Clean level names so that they look nice in the table
#'   table[[subset_col]] <- stringr::str_to_sentence(table[[subset_col]])
#'   levels <- unique(table[[subset_col]])
#'   }
#'
#'
#'   # Work out if only one level is present. Passed to create_subtotal_row(), so
#'   # that if only one group, no subtotal is created.
#'   single_group <- ifelse(length(levels)==1,TRUE,FALSE)
#'
#'   # Subset data by levels, run user-defined metafor function on them, and
#'   # recombine along with Overall rma output
#'   subset <-lapply(levels, function(level){dplyr::filter(table, !!as.symbol(subset_col) == level)})
#'   names(subset) <- levels
#'
#'   # This takes the same metafor::rma function (including args) and runs it on each subsetted dataset
#'   subset_res <- lapply(levels, function(level){metafor_function(rma, data = subset[[level]])})
#'   names(subset_res) <- levels
#'
#'
#'
#'   # This
#'   subset_tables <-
#'     lapply(levels, function(level){
#'       rbind(
#'         create_title_row(level),
#'         dplyr::select(subset[[level]], Study, .data$est, .data$ci_low, .data$ci_high),
#'         create_subtotal_row(subset_res[[level]], single_group = single_group, add_tests = add_tests)
#'       )
#'     })
#'
#'   subset_table <- do.call("rbind", lapply(subset_tables, function(x) x))
#'
#'   ordered_table <- rbind(subset_table,
#'                          if (overall_estimate) {
#'                            create_subtotal_row(rma, "Overall", add_blank = FALSE)
#'                          })
#'
#'   rob_data_for_graph <- dplyr::left_join(ordered_table, rob, by = "Study")
#'
#'   # Indent the subgroup if there is a number in the placebo column
#'   ordered_table$Study <- as.character(ordered_table$Study)
#'   ordered_table$Study <- ifelse(!(ordered_table$Study %in% levels) & ordered_table$Study != "Overall",
#'                                 paste0(" ", ordered_table$Study),
#'                                 ordered_table$Study)
#'
#'
#'   left_side_data <- dplyr::select(ordered_table, Study)
#'
#'   estimate <- ordered_table$est
#'   ci_low <- ordered_table$ci_low
#'   ci_high <- ordered_table$ci_high
#'
#'   # Set subset titles to bold.italic
#'   bold_vec <-
#'     ifelse(stringr::str_trim(as.vector(left_side_data$Study)) %in% c(levels),
#'            "bold.italic",
#'            "plain")
#'
#'   # Set overall title to bold
#'   bold_vec <-
#'     ifelse(stringr::str_trim(as.vector(left_side_data$Study)) %in% c("Overall"),
#'            "bold",
#'            bold_vec)
#'
#'   if(is.null(theme)){
#'     theme <- gridExtra::ttheme_minimal(core=list(
#'       fg_params = list(hjust = 0, x = 0.05,
#'                        fontfamily = font_family,
#'                        fontface = bold_vec),
#'       bg_params = list(fill=c(rep(c("#eff3f2", "white"), length.out=4)))
#'     ),
#'     colhead = list(fg_params = list(hjust = 0, x = 0.05,
#'                                     fontfamily = font_family),
#'                    bg_params = list(fill = "white"))
#'     )
#'   }
#'
#'   # theme_rob is only white background, otherwise the same
#'
#'   theme_rob <- gridExtra::ttheme_minimal(
#'     core=list(
#'       fg_params = list(hjust = 0, x = 0.05, fontfamily = font_family)
#'     ),
#'     colhead = list(fg_params = list(hjust = 0, x = 0.05,
#'                                     fontfamily = font_family,
#'                                     col = "white"),
#'                    bg_params = list(fill = "white"))
#'   )
#'
#'   gdata <- data.frame(estimate = estimate,
#'                       ci_low = ci_low,
#'                       ci_high = ci_high)
#'
#'   tdata <- gdata
#'
#'   tdata <- dplyr::mutate_all(tdata, ~sprintf(.,
#'                                              fmt = paste0('%#.', 2,'f')
#'   ))
#'
#'   tdata[tdata == "NA"] <- " "
#'
#'   # pretty formatting for confidence intervals
#'   right_side_data <- data.frame(Estimate = ifelse(tdata$estimate == " ",
#'                                                   " ", paste0(tdata$estimate, " (", tdata$ci_low,
#'                                                               " to ", tdata$ci_high, ")")))
#'
#'   colnames(right_side_data) <- estimate_col_name
#'
#'   # finds width in number of characters for monospaced font
#'
#'   find_width_mono <- function(data){
#'     num_of_rows <- nrow(data)
#'     num_of_cols <- ncol(data)
#'
#'     print_data <- dplyr::mutate_all(data, as.character)
#'
#'     num_char_across <- 0
#'     width <- 0
#'
#'     for(i in 1:num_of_cols){
#'       for(j in 1:num_of_rows){
#'         num_char_across[j] <- nchar(print_data[j, i])
#'       }
#'       width[i] <- max(max(num_char_across, na.rm = TRUE),
#'                       nchar(colnames(print_data)[i]), na.rm = TRUE)
#'     }
#'     return(sum(width, na.rm = TRUE))
#'   }
#'
#'   # finds width using shape_string from the systemfonts package
#'   # if not using monospaced font
#'
#'   find_width <- function(data){
#'     num_of_rows <- nrow(data)
#'     num_of_cols <- ncol(data)
#'
#'     print_data <- dplyr::mutate_all(data, as.character)
#'
#'     width <- 0
#'
#'     names <- colnames(print_data)
#'
#'     for (i in 1:num_of_cols){
#'       temp <- systemfonts::shape_string(print_data[[names[i]]], family = font_family)
#'       temp_col <- systemfonts::shape_string(names[i], family = font_family)
#'       width[i] <- max(max(temp$metrics$width, na.rm = TRUE),
#'                       temp_col$metrics$width, na.rm = TRUE)
#'     }
#'     return(round((sum(width, na.rm = TRUE)/7.2), 0))
#'   }
#'
#'   # calculate widths for each side with the appropriate function
#'
#'   if(font_family == "mono"){
#'     left_width <- find_width_mono(left_side_data)
#'     right_width <- find_width_mono(right_side_data)
#'   }else{
#'     left_width <- find_width(left_side_data)
#'     right_width <- find_width(right_side_data)
#'   }
#'
#'
#'   if(blank_na == TRUE){
#'     left_side_data <- dplyr::mutate_all(left_side_data, as.character)
#'     left_side_data[is.na(left_side_data)] <- " "
#'   }
#'
#'   # insert a blank column so we can put the ggplot object on top
#'   # and correctly order columns
#'
#'   ggplot_width <- round(right_width * ggplot_is_x_times_right_width, 0)
#'   total_width <- left_width + right_width + ggplot_width
#'
#'   if (!font_family == "mono"){
#'     ggplot_width <- round((8/3) * ggplot_width, 0)
#'   }
#'
#'   tdata_print <- left_side_data
#'   tdata_print$` ` <- paste(rep(" ", times = ggplot_width),
#'                            collapse = '')
#'   tdata_print <- cbind(tdata_print, right_side_data)
#'
#'   # function to calculate greatest common factor
#'
#'   gcf <- function(a, b){
#'     t <- 0
#'     while (b != 0){
#'       t <- b
#'       b <- a %% b
#'       a <- t
#'     }
#'     return(a)
#'   }
#'
#'   # make one unit in patchwork equal in width to the greatest common factor of
#'   # the three widths
#'
#'   one_patchwork_unit <- gcf(gcf(left_width, right_width), ggplot_width)
#'
#'   left_pw <- left_width/one_patchwork_unit
#'   right_pw <- right_width/one_patchwork_unit
#'   total_pw <- total_width/one_patchwork_unit
#'
#'   # create rob table skeleton
#'
#'   rob_table <- data.frame(D1 = rep("", times = nrow(ordered_table)),
#'                           D2 = rep("", times = nrow(ordered_table)),
#'                           D3 = rep("", times = nrow(ordered_table)),
#'                           D4 = rep("", times = nrow(ordered_table)),
#'                           D5 = rep("", times = nrow(ordered_table)),
#'                           Overall = rep("", times = nrow(ordered_table)))
#'
#'
#'   rob_width <- find_width_mono(rob_table)
#'
#'   # calculated patchwork layout
#'   # 1. the forest plot table
#'   # 2. the forest plot ggplot
#'   # 3. the rob plot ggplot
#'
#'   layout <- c(patchwork::area(t = 1,
#'                               b = nrow(gdata),
#'                               l = 1,
#'                               r = total_pw),
#'               patchwork::area(t = 1,
#'                               b = (nrow(gdata) + 1),
#'                               l = left_pw + 1,
#'                               r = total_pw - right_pw + 1),
#'               patchwork::area(t = 1,
#'                               b = nrow(gdata),
#'                               l = total_pw + 2,
#'                               r = total_pw + 2 + rob_width %/% one_patchwork_unit),
#'               patchwork::area(t = 1,
#'                               b = (nrow(gdata) + 1),
#'                               l = total_pw + 2,
#'                               r = total_pw + 2 + rob_width %/% one_patchwork_unit))
#'
#'   gdata$row_num <- (nrow(gdata) - 1):0
#'
#'   y_low <- -.54 - .1381 * log(nrow(gdata))
#'   y_high <- nrow(gdata) + -.3
#'
#'   #### turn the rob input into ggplot-able data ################################
#'
#'   rob_gdata <- dplyr::select(rob_data_for_graph, .data$D1:.data$Overall)
#'
#'   rob_gdata$row_num = (nrow(rob_gdata) - 1):0
#'
#'   rob_gdata <- dplyr::rename(rob_gdata, D6 = .data$Overall)
#'
#'   rob_gdata <- tidyr::pivot_longer(rob_gdata, !.data$row_num, names_to = "x", values_to = "colour")
#'
#'   rob_gdata <- dplyr::mutate(rob_gdata, x = as.integer(substr(.data$x, 2, 2)))
#'
#'   rob_gdata$x[rob_gdata$x == 6] <- 6.5
#'
#'   rob_colours <- get_colour(rob_tool, rob_colour)
#'
#'   bias_colours <- c("High" = rob_colours$high_colour,
#'                     "Some concerns" = rob_colours$concerns_colour,
#'                     "Low" = rob_colours$low_colour,
#'                     "No information" = rob_colours$ni_colour)
#'
#'   titles <- data.frame(names = c("D1", "D2", "D3", "D4", "D5", "Overall"),
#'                        y = max(rob_gdata$row_num),
#'                        x = c(1, 2, 3, 4, 5, 6.5))
#'
#'   rectangles <- rob_gdata[stats::complete.cases(rob_gdata), ]
#'
#'   rectangles$xmin <- rectangles$x - 0.5
#'   rectangles$xmax <- rectangles$x + 0.5
#'   rectangles$ymin <- rectangles$row_num - 0.5
#'   rectangles$ymax <- rectangles$row_num + 0.5
#'
#'   ########## the main figure - this will be overlaid on the table ##############
#'
#'   center <- ggplot2::ggplot(data = gdata, ggplot2::aes(y = .data$row_num, x = estimate)) +
#'     ggplot2::geom_point(size = 3.25) + # the point estimates, with big dots
#'     ggplot2::geom_errorbarh(ggplot2::aes(y = .data$row_num,
#'                                          xmin = ci_low,
#'                                          xmax = ci_high),
#'                             height = .25) + # the CIs, with short ends
#'     ggplot2::theme_classic() + # base theme
#'     ggplot2::scale_y_continuous(expand = c(0,0), #remove padding
#'                                 limits = c(y_low, y_high)) + # position dots
#'     ggplot2::theme(axis.title.y = ggplot2::element_blank(), # remove axis, make bg transparent
#'                    axis.text.y = ggplot2::element_blank(),
#'                    axis.ticks.y = ggplot2::element_blank(),
#'                    axis.line.y = ggplot2::element_blank(),
#'                    axis.ticks.length.x = grid::unit(.1, "in"),
#'                    text = ggplot2::element_text(family = font_family, size = 12),
#'                    panel.background = ggplot2::element_rect(fill = "transparent"),
#'                    plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
#'                    panel.grid.major = ggplot2::element_blank(),
#'                    panel.grid.minor = ggplot2::element_blank(),
#'                    legend.background = ggplot2::element_rect(fill = "transparent"),
#'                    legend.box.background = ggplot2::element_rect(fill = "transparent")) +
#'     ggplot2::geom_vline(xintercept = null_line_at, linetype = "dashed") + # null line
#'     ggplot2::scale_x_continuous(labels = scales::number_format(accuracy = 0.1), trans = "log10",
#'                                 breaks = c(0.1, 0.2, 0.5, 1, 2, 5, 10),
#'                                 limits = c(0.1,10)) +
#'     ggplot2::xlab("")
#'
#'   ############################## the rob ggplot figure #########################
#'
#'   rob_plot <- ggplot2::ggplot(data = rob_gdata[stats::complete.cases(rob_gdata), ]) +
#'     ggplot2::geom_rect(data = rectangles,
#'                        ggplot2::aes(xmin = .data$xmin,
#'                                     ymin = .data$ymin,
#'                                     xmax = .data$xmax,
#'                                     ymax = .data$ymax),
#'                        fill = "white",
#'                        colour = "#eff3f2") +
#'     ggplot2::geom_point(size = 5, ggplot2::aes(x = .data$x, y = .data$row_num, colour = .data$colour)) +
#'     ggplot2::geom_point(size = 3, ggplot2::aes(x = .data$x, y = .data$row_num, shape = .data$colour)) +
#'     ggplot2::scale_y_continuous(expand = c(0,0), #remove padding
#'                                 limits = c(y_low, y_high)) + # position dots
#'     ggplot2::geom_text(data = titles, ggplot2::aes(label = .data$names, x = .data$x, y = .data$y)) +
#'     ggplot2::theme_classic() + # base theme
#'     ggplot2::theme(axis.title.y = ggplot2::element_blank(), # remove axis, make bg transparent
#'                    axis.text.y = ggplot2::element_blank(),
#'                    axis.ticks.y = ggplot2::element_blank(),
#'                    axis.line.y = ggplot2::element_blank(),
#'                    axis.title.x = ggplot2::element_blank(), # remove axis, make bg transparent
#'                    axis.text.x = ggplot2::element_blank(),
#'                    axis.ticks.x = ggplot2::element_blank(),
#'                    axis.line.x = ggplot2::element_blank(),
#'                    panel.background = ggplot2::element_rect(fill = "transparent"),
#'                    plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
#'                    panel.grid.major = ggplot2::element_blank(),
#'                    panel.grid.minor = ggplot2::element_blank(),
#'                    legend.background = ggplot2::element_rect(fill = "transparent"),
#'                    legend.box.background = ggplot2::element_rect(fill = "transparent"),
#'                    legend.position = "none") +
#'     ggplot2::scale_x_continuous(limits = c(0.5, 7.5),
#'                                 expand = c(0, 0)) +
#'     ggplot2::scale_color_manual(values = bias_colours,
#'                                 na.translate = FALSE) +
#'     ggplot2::scale_shape_manual(
#'       values = c(
#'         "High" = 120,
#'         "Some concerns" = 45,
#'         "Low" = 43,
#'         "No information" = 63
#'       ))
#'
#'   ######### using patchwork, overlay the ggplots on the tables #################
#'
#'   final <- patchwork::wrap_elements(gridExtra::tableGrob(tdata_print, theme = theme, rows = NULL)) +
#'     center +
#'     patchwork::wrap_elements(gridExtra::tableGrob(rob_table, theme = theme_rob, rows = NULL)) +
#'     rob_plot +
#'     patchwork::plot_layout(design = layout)
#'
#'   h_adj <- 1
#'
#'   if(!font_family == "mono"){h_adj <- 25.6/23.25} # ratio of heights fira/mono
#'
#'   ######### save the plot as a png, then display it with magick ################
#'
#'   ggplot2::ggsave(dpi = dpi, height = h_adj * (nrow(gdata) + 3)/3.85,
#'                   width = (total_width + rob_width)/10 + 2, units = "in",
#'                   filename = file_path)
#'
#'   if(display == TRUE){
#'      magick::image_resize(magick::image_read(file_path),
#'                           paste0(grDevices::dev.size("px")[1],
#'                                  "x",
#'                                  grDevices::dev.size("px")[2]))
#'   }
#' }
#'
#' # Helpers for rob_blobbogram ====
#'
#' metafor_function <- function(res, data = dat){
#'   eval(rlang::call_modify(res$call, data = quote(data)))
#' }
#'
#' create_subtotal_row <- function(rma,
#'                                 name = "Subtotal",
#'                                 single_group = FALSE,
#'                                 add_tests = FALSE,
#'                                 add_blank = TRUE){
#'
#'   if (single_group == FALSE) {
#'     row <- data.frame(Study = name,
#'                       est = exp(rma$b),
#'                       ci_low = exp(rma$ci.lb),
#'                       ci_high = exp(rma$ci.ub))
#'
#'
#'     if (add_tests) {
#'
#'       tests <- data.frame(
#'         Study = paste0(
#'           "Q=",
#'           formatC(rma$QE, digits = 2, format =
#'                     "f"),
#'           ", df=",
#'           rma$k - rma$p,
#'           ", p=",
#'           formatC(rma$QEp, digits = 2, format = "f"),
#'           "; ",
#'           "I^2",
#'           "=",
#'           formatC(rma$I2, digits = 1, format =
#'                     "f"),
#'           "%"
#'         ),
#'
#'         est = c(NA),
#'         ci_low = c(NA),
#'         ci_high = c(NA))
#'
#'
#'       row <- rbind(row,
#'                    tests)
#'     }
#'
#'
#'     if(add_blank){row <- dplyr::add_row(row)}
#'
#'
#'     return(row)
#'   }
#' }
#'
#' create_title_row <- function(title){
#'   return(data.frame(Study = title, est = NA, ci_low = NA, ci_high = NA))
#' }
#'
