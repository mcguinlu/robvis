#' Create "realistic" dummy risk of bias assessment data
#'
#' @description This function returns N example risk of bias assessments for the
#'   tool specified, where N is set by the user. Assessments are "realistic" in
#'   that the judgment in the overall column is set to the highest judgement
#'   across the domains for a given study, reflecting the recommendations of the
#'   tool creators.
#'
#' @param n Number of assessments to create
#' @param tool Tool used for assessment (see rob_tools()). Default is "ROB2".
#' @param study Should the returned dataframe contain a "Study" column. Default
#'   is true.
#'
#' @family helper
#'
#' @export
#' @importFrom magrittr %>%

rob_dummy <- function(n, tool = "ROB2", study = TRUE){
  # TODO Need to work out how to handle the cluster/generic tools

  if (tool == "ROB2") {
    prob = c(.025, .5, .2, 0.1)
    ncol = 5
  }

  if (tool %in% c("ROBINS-I","ROBINS-E")) {
    prob = c(.025, .5, .5, 0.25, 0.025)
    ncol = 7
  }

  if (tool == "QUADAS-2") {
    prob = c(.025, .5, .2, 0.1)
    ncol = 4
  }

  values = rev(get_judgements(tool))

  v_values = seq_len(length(values))

  dat <- matrix(nrow = n, ncol = ncol) %>%
      as.data.frame() %>%
      dplyr::mutate(dplyr::across(dplyr::everything(),
                    ~ sample(v_values, dplyr::n(), T, prob)))


  # Probability of a low judgement in D1 of these tools is v small
  # But need high prob of Low for other domains in these tools
  if(tool %in% c("ROBINS-I", "ROBINS-E")){
      dat[which(dat[,1]==2),1] <- 3
  }

  dat <- dat %>%
      dplyr::rowwise() %>%
      dplyr::mutate(Overall = max(dplyr::across(dplyr::everything()))) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~dplyr::case_when(. == "1" ~ values[1],
                                             . == "2" ~ values[2],
                                             . == "3" ~ values[3],
                                             . == "4" ~ values[4],
                                             . == "5" ~ values[5],
                                             . == "6" ~ values[6],
                                             . == "7" ~ values[7],
                                              T ~ .))) %>%
      dplyr::ungroup() %>%
      dplyr::rename_with(~gsub("V","D",.), .cols = dplyr::starts_with("V")) %>%
      dplyr::select(dplyr::starts_with("D"),Overall)



  if (study) {
    dat <- dat %>%
      dplyr::mutate(Study = paste("Study", 1:dplyr::n())) %>%
      dplyr::select(Study, dplyr::everything())
  }

  return(dat)

}

