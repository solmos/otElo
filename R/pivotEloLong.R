#' Convert data frame of Elo ratings from wide to long
#'
#' @param df Data frame of Elo ratings in wide format
#'   as returned by \code{getElo()}
#'
#' @return
#' @export
#'
#' @examples
pivotEloLong <- function(df) {

  df_long <- df %>%
    tidyr::pivot_longer(
      cols = c(tidyselect::ends_with("home"), tidyselect::ends_with("away")),
      names_to = c(".value", "where"),
      names_pattern = "(.+_?.?)_(.+)"
    )
  df %>%
    dplyr::select(season, game_code, team_home, team_away) %>%
    dplyr::right_join(df_long, by = c("season", "game_code")) %>%
    dplyr::mutate(
      team_opp = ifelse(where == "home", team_away, team_home),
      home_adv = ifelse(where == "home", home_adv, -home_adv)
    ) %>%
    dplyr::select(-team_home, -team_away) %>%
    dplyr::select(
      season:round_code,
      team, team_opp,
      winner, pt_diff, elo_diff,
      dplyr::everything()
    )
}

#df <- elohistwide
#df_long <- df %>%
#  tidyr::pivot_longer(
#    cols = c(tidyselect::ends_with("home"), tidyselect::ends_with("away")),
#    names_to = c(".value", "where"),
#    names_pattern = "(.+_?.?)_(.+)"
#  )
#df %>%
#  dplyr::select(season, game_code, team_home, team_away) %>%
#  dplyr::right_join(df_long, by = c("season", "game_code")) %>%
#  dplyr::mutate(team_opp = ifelse(where == "home", team_home, team_away)) %>%
#  dplyr::select(-team_home, -team_away) %>%
#  dplyr::select(
#    season:round_code,
#    team, team_opp,
#    winner, pt_diff, elo_diff,
#    dplyr::everything()
#  )
#
#library(dplyr)
#x <- df %>%
#  dplyr::select(season, game_code, team_home, team_away) %>%
#  dplyr::right_join(df_long, by = c("season", "game_code"))
#x %>%
#  transmute(team_opp = ifelse(where == "home", team_home, team_away)) %>%
#

