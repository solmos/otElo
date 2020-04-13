simSeason <- function(fgames, pgames, fit) {

  regular_season <- simRegSeason(fgames, pgames, fit)

  playoff_table <- regular_season %>%
    dplyr::filter(rank <= 8) %>%
    dplyr::select(team, rank, elo)

  matchups_po <- createMatchups(
    teams = playoff_table$team,
    elo = playoff_table$elo,
    type = "PO"
  )

  results_po <- purrr::map(
    matchups_po,
    ~ simSeries(teams = .$teams, elo = .$elo, fit = fit)
  )
  winners_po <- purrr::map_dfr(results_po, getSeriesWinner)

  final4 <- simFinal4(teams = winners_po$team, elo = winners_po$elo, fit = fit)

  list(
    playoff_teams = playoff_table,
    final4_teams = winners_po,
    finals = final4$final,
    champion = final4$champion
  )
}
