simGame <- function(teams, elo, fit, home = TRUE) {
  h <- ifelse(home == TRUE, 100, 0)
  win_prob_home <- getExpectedProb(elo[1], elo[2], home_adv = h, s = 400)
  win_prob_away <- 1 - win_prob_home

  elo_diff_home <- elo[1] - elo[2]
  point_spread <- predictPointDiff(fit, elo_diff_home)

  win_home <- ifelse(point_spread >= 0, 1, 0)
  win_away <- ifelse(win_home == 1, 0, 1)

  mov_home <- getMovMultiplier(point_spread, elo_diff_home)
  mov_away <- getMovMultiplier(point_spread, -elo_diff_home)

  elo_new_home <- elo[1] + 25 * (win_home - win_prob_home) * mov_home
  elo_new_away <- elo[2] + 25 * (win_away - win_prob_away) * mov_away

  tibble::tibble(
    team = teams,
    win = as.logical(c(win_home, win_away)),
    win_prob = c(win_prob_home, win_prob_away),
    pt_diff = c(point_spread, -point_spread),
    elo_new = c(elo_new_home, elo_new_away)
  ) %>%
    dplyr::arrange(team)
}
