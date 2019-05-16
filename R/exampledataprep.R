# Load datasets
data_rob1 <- read.csv("data_raw/data_rob1.csv", header = TRUE)
data_rob2 <- read.csv("data_raw/data_rob2.csv", header = TRUE)
data_robins <- read.csv("data_raw/data_robins.csv", header = TRUE)
data_quadas <- read.csv("data_raw/data_quadas.csv", header = TRUE)

usethis::use_data(data_rob1, overwrite = TRUE)
usethis::use_data(data_rob2, overwrite = TRUE)
usethis::use_data(data_quadas, overwrite = TRUE)
usethis::use_data(data_robins, overwrite = TRUE)
