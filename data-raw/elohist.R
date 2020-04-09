## code to prepare `elohist` dataset goes here

data("resultshist")

elohistwide <- getElo(resultshist, k = 25, home_adv = 100, s = 400, carry = 0.8)

diff <- elohistwide %>%
  dplyr::group_by(season, game_code) %>%
  dplyr::summarise(
    diff_home = points_home - points_away
  ) %>%
  dplyr::ungroup()

elohist <- pivotEloLong(elohistwide)

# Find the difference in points in each game
diff <- elohistwide %>%
  dplyr::group_by(season, game_code) %>%
  dplyr::summarise(
    diff = Reduce("-", points)
  ) %>%
  dplyr::ungroup()

x <- elohist %>%
  dplyr::left_join(diff, by = c("season", "game_code")) %>%
  dplyr::mutate(diff = ifelse(where == "home", diff, -diff))

usethis::use_data(elohist, overwrite = TRUE)
readr::write_csv(elohist, "data-raw/elohist.csv")
usethis::use_data(elohistwide, overwrite = TRUE)
readr::write_csv(elohistwide, "data-raw/elohistwide.csv")
