# Helpers for rob_append_to_forest ====

forest.invisible <- function(...){
  ff <- tempfile()
  grDevices::png(filename=ff)
  res <- metafor::forest(...)
  grDevices::dev.off()
  unlink(ff)
  res
}


# Helpers for rob_blobbogram ====

metafor_function <- function(res, data = dat){
  eval(rlang::call_modify(res$call, data = quote(data)))
}

create_subtotal_row <- function(rma,
                                name = "Subtotal",
                                single_group = FALSE,
                                add_tests = FALSE,
                                add_blank = TRUE){

  if (single_group == FALSE) {
  row <- data.frame(Study = name,
                    est = exp(rma$b),
                    ci_low = exp(rma$ci.lb),
                    ci_high = exp(rma$ci.ub))


  if (add_tests) {

    tests <- data.frame(
      Study = paste0(
        "Q=",
        formatC(rma$QE, digits = 2, format =
                  "f"),
        ", df=",
        rma$k - rma$p,
        ", p=",
        formatC(rma$QEp, digits = 2, format = "f"),
        "; ",
        "I^2",
        "=",
        formatC(rma$I2, digits = 1, format =
                  "f"),
        "%"
      ),

                        est = c(NA),
                        ci_low = c(NA),
                        ci_high = c(NA))


    row <- rbind(row,
                 tests)
  }


  if(add_blank){row <- dplyr::add_row(row)}


  return(row)
  }
}

create_title_row <- function(title){
  return(data.frame(Study = title, est = NA, ci_low = NA, ci_high = NA))
}
