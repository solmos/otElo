## code to prepare `rschedule` dataset goes here

rounds <- 29:34
rschedule <- purrr::map_dfr(rounds, extractRound)

usethis::use_data(rschedule)
readr::write_csv(rschedule, "data-raw/rschedule.csv")
