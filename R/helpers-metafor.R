############################################################################
# The functions contained in this file were written by Wolfgang Viechtbauer as
# part of his {metafor} (https://www.metafor-project.org/doku.php) package.

# As rob_append_to_forest() builds on the metafor::forest() function, there were
# some internal {metafor} functions needed to properly render the plot.

############################################################################

.pval <- function(p, digits=4, showeq=FALSE, sep="", add0=FALSE) {

  digits  <- max(digits, 1)
  cutoff  <- paste(c(".", rep(0,digits-1),1), collapse="")
  ncutoff <- as.numeric(cutoff)

  ifelse(is.na(p), paste0(ifelse(showeq, "=", ""), sep, NA),
         ifelse(p >= ncutoff, paste0(ifelse(showeq, "=", ""), sep, formatC(p, digits=digits, format="f")),
                paste0("<", sep, ifelse(add0, "0", ""), cutoff)))

}

mlabfun <- function(text, res) {

  list(bquote(paste(.(text),
                    " (",
                    # " Q = ", .(formatC(res$QE, digits=2, format="f")),
                    # ", df = ", .(res$k - res$p),
                    "p ", .(metafor:::.pval(res$pval, digits=2, showeq=TRUE, sep=" ")), "; ",
                    I^2, " = ", .(formatC(res$I2, digits=1, format="f")), "%, ",
                    tau^2, " = ", .(formatC(res$tau2, digits=2, format="f")), ")")))

}



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
