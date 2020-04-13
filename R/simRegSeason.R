#' Simulate regular season
#'
#' @param fgames Data frame with future games in wide format (as in rschedule)
#' @param pgames Data frame with past games in long format (as in elohist)
#'
#' @return
#' @export
#'
#' @examples
simRegSeason <- function(fgames, pgames, fit) {
  # Simulate Elo ratings
  init_elo <- getLatestElo(pgames, 2019)
  elo_sim <- simElo(fgames, initial_elo = init_elo, fit = fit)
  ratings <- elo_sim$ratings
  final_ratings <- tibble::tibble(
    team = names(ratings),
    elo = unlist(ratings)
  )
  elo_sim_long <- pivotEloLong(elo_sim$data)
  # Combine all games
  # TODO: Deal when it is the start of the season and there are no past games
  elo_df <- dplyr::bind_rows(pgames[colnames(elo_sim_long)], elo_sim_long)

  # Get total wins
  wins_sim <- getWinsSim(elo_sim_long)
  wins_past <- getWinsPast(pgames)
  wins_final <- mergeWins(wins_past, wins_sim)

  # Unbreak ties
  tied_teams <- getTiedTeams(wins_final)
  tied_df <- purrr::map_df(tied_teams, rankTiedTeams, df = elo_df) %>%
    dplyr::select(team, rank_points)
  ranking_final <- wins_final %>%
    dplyr::left_join(tied_df, by = "team") %>%
    dplyr::left_join(final_ratings, by = "team") %>%
    dplyr::mutate(
      rank_points = ifelse(is.na(rank_points), 0, rank_points),
      rank = rank(-(wins + rank_points))
    ) %>%
    dplyr::arrange(rank)

  ranking_final
}
