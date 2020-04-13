#' Get table of wins and overall point differential from past games
#'
#' @param df Data frame with past Elo ratings in long format
#'
#' @return
#' @export
#'
#' @examples
getWinsPast <- function(df) {
  df %>%
    dplyr::group_by(team) %>%
    dplyr::summarise(
      wins = sum(winner == team_code),
      diff = sum(pt_diff)
    ) %>%
    dplyr::arrange(dplyr::desc(wins), dplyr::desc(diff))
}

#' Get table of wins and overall point differential from simulated games
#'
#' @param df Data frame of simulated games as returned by \code{simElo()}
#'
#' @return
#' @export
#'
#' @examples
getWinsSim <- function(df) {
  df %>%
    dplyr::group_by(team) %>%
    dplyr::summarise(
      wins = sum(winner == team),
      diff = sum(pt_diff)
    ) %>%
    dplyr::arrange(dplyr::desc(wins), dplyr::desc(diff))
}

#' Merge past and simulated win tables
#'
#' @param past Data frame with past win table
#' @param sim Data frame with simulated win table
#'
#' @return
#' @export
#'
#' @examples
mergeWins <- function(past, sim) {
  past %>%
    dplyr::left_join(sim, by = "team", suffix = c("_past", "_sim")) %>%
    dplyr::mutate(wins = wins_past + wins_sim,
                  diff = diff_past + diff_sim) %>%
    dplyr::arrange(dplyr::desc(wins), dplyr::desc(diff)) %>%
    dplyr::select(team, wins, diff, dplyr::everything())
}

#' Get teams with same number of wins
#'
#' @param df Data frame of wins
#'
#' @return
#' @export
#'
#' @examples
getTiedTeams <- function(df) {
  teams_by_wins <- split(df$team, df$wins)
  # Select only the elements with more than one team
  idx <- purrr::map_lgl(teams_by_wins, ~ length(.) > 1)
  teams_by_wins[idx]
}

rankTiedTeams <- function(teams, df) {

  overall_pt_diff <- df %>%
    dplyr::group_by(team) %>%
    dplyr::summarise(diff = sum(pt_diff))

  games <- df %>%
    dplyr::filter(team %in% teams & team_opp %in% teams)

  games %>%
    dplyr::group_by(team) %>%
    dplyr::summarise(
      wins = sum(win_points),
      pt_diff = sum(pt_diff)
    ) %>%
    dplyr::left_join(overall_pt_diff, by = "team") %>%
    dplyr::arrange(wins, pt_diff) %>%
    dplyr::mutate(rank_points = 1:length(teams) / 100)
}
