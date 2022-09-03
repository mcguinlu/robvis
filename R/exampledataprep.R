# Load datasets
# data_rob1 <-
#   tidyr::as_tibble(read.csv(
#     "data_raw/data_rob1.csv",
#     stringsAsFactors = FALSE,
#     header = TRUE,
#     fileEncoding = "latin1"
#   ))
# data_rob2 <-
#   tidyr::as_tibble(read.csv(
#     "data_raw/data_rob2.csv",
#     stringsAsFactors = FALSE,
#     header = TRUE,
#     fileEncoding = "latin1"
#   ))
# data_rob2_cluster <-
#   tidyr::as_tibble(read.csv(
#     "data_raw/data_rob2_cluster.csv",
#     stringsAsFactors = FALSE,
#     header = TRUE,
#     fileEncoding = "latin1"
#   ))
# data_robins_i <-
#   tidyr::as_tibble(read.csv(
#     "data_raw/data_robins_i.csv",
#     stringsAsFactors = FALSE,
#     header = TRUE,
#     fileEncoding = "latin1"
#   ))
# data_robins_e <-
#   tidyr::as_tibble(read.csv(
#     "data_raw/data_robins_e.csv",
#     stringsAsFactors = FALSE,
#     header = TRUE,
#     fileEncoding = "latin1"
#   ))
# data_quadas <-
#   tidyr::as_tibble(read.csv(
#     "data_raw/data_quadas.csv",
#     stringsAsFactors = FALSE,
#     header = TRUE,
#     fileEncoding = "latin1"
#   ))
# data_quips <-
#   tidyr::as_tibble(read.csv(
#     "data_raw/data_quips.csv",
#     stringsAsFactors = FALSE,
#     header = TRUE,
#     fileEncoding = "latin1"
#   ))
#
# usethis::use_data(data_rob1, overwrite = TRUE)
# usethis::use_data(data_rob2, overwrite = TRUE)
# usethis::use_data(data_rob2_cluster, overwrite = TRUE)
# usethis::use_data(data_quadas, overwrite = TRUE)
# usethis::use_data(data_robins_i, overwrite = TRUE)
# usethis::use_data(data_robins_e, overwrite = TRUE)
# usethis::use_data(data_quips, overwrite = TRUE)



# data_bias_direction <-
#     tidyr::as_tibble(read.csv(
#       "data_raw/bias_direction.csv",
#       stringsAsFactors = FALSE,
#       header = TRUE,
#       fileEncoding = "latin1"
#     )) %>%
#   select(-c(X,X.1))
# usethis::use_data(data_bias_direction, overwrite = TRUE)
