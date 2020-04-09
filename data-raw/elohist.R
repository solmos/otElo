## code to prepare `elohist` dataset goes here

data("resultshist")

elohistwide <- getElo(resultshist, k = 25, home_adv = 100, s = 400, carry = 0.8)

elohist <- pivotEloLong(elohistwide)

usethis::use_data(elohist, overwrite = TRUE)
readr::write_csv(elohist, "data-raw/elohist.csv")
usethis::use_data(elohistwide, overwrite = TRUE)
readr::write_csv(elohistwide, "data-raw/elohistwide.csv")
