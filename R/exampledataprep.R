# Load datasets
data_rob <- read.csv("data_raw/data_rob.csv", header = TRUE)
data_robins <- read.csv("data_raw/data_robins.csv", header = TRUE)
data_quadas <- read.csv("data_raw/data_quadas.csv", header = TRUE)

usethis::use_data(data_rob, overwrite = TRUE)
usethis::use_data(data_quadas, overwrite = TRUE)
usethis::use_data(data_robins, overwrite = TRUE)
